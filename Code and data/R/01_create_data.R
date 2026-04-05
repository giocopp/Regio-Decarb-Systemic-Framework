# ── 01_create_data.R ── Functions to create initial data for TRI pipeline ─────
#
# Each function takes file paths as arguments (targets-compatible) and returns
# a tibble. No file writing -- targets handles persistence.
# ──────────────────────────────────────────────────────────────────────────────


# ── 1) Employment weights ────────────────────────────────────────────────────

#' Download and process regional employment weights from Eurostat
#'
#' Downloads sbs_r_nuts2021 (EMP_LOC_NR) from Eurostat, handles confidential
#' cells, prefers 2022 over 2021, imputes missing regionals from country-sector
#' medians, re-anchors regionals to national totals, enforces C-additivity,
#' aggregates to 11 macro sectors, and computes employment shares.
#'
#' @param base_data_path Path to base_data_plus.xlsx (used to identify valid
#'   NUTS-2 regions)
#' @return Tibble with columns: Country_ID, NUTS_ID, Sector_ID,
#'   pers_employed, weight
create_employment_weights <- function(base_data_path) {

  nace_codes <- c(
    "C", "C10", "C11", "C12", "C13", "C14", "C15",
    "C16", "C17", "C18", "C19", "C20", "C21", "C22",
    "C23", "C24", "C25", "C26", "C27", "C28", "C29",
    "C30", "C31", "C32", "C33"
  )


  # ── Download from Eurostat ──────────────────────────────────────
  raw <- restatapi::get_eurostat_data(
    id          = "sbs_r_nuts2021",
    filters     = list(
      indic_sbs = "EMP_LOC_NR",
      sizeclas  = "TOTAL",
      nace_r2   = nace_codes
    ),
    date_filter = c(2022, 2021),
    exact_match = TRUE,
    label       = FALSE,
    cflags      = TRUE,
    keep_flags  = TRUE
  )

  # ── Valid regions from base data ────────────────────────────────
  base_d <- readxl::read_xlsx(base_data_path) |>
    dplyr::select(CNTR_CODE, NUTS_ID) |>
    dplyr::rename(geo = NUTS_ID, country = CNTR_CODE)

  # ── Keep EU-27 national (2-char) & NUTS-2 (4-char); align keys ─
  df <- raw |>
    dplyr::mutate(
      geo       = as.character(geo),
      values    = as.numeric(values),
      country   = substr(geo, 1, 2),
      geo_level = dplyr::if_else(nchar(geo) == 2, "national", "regional")
    ) |>
    dplyr::filter(
      country %in% eu27,
      !grepl("ZZ$", geo),
      nchar(geo) %in% c(2, 4)
    ) |>
    dplyr::semi_join(base_d, by = c("country", "geo"))

  # ── Confidential cells -> NA ────────────────────────────────────
  df <- df |>
    dplyr::mutate(values = dplyr::if_else(flags == "c", NA_real_, values))

  # ── Prefer 2022; if 2022 is NA but 2021 exists, fill from 2021 ─
  df <- df |>
    dplyr::mutate(Year = as.integer(as.character(time))) |>
    dplyr::group_by(geo, country, nace_r2, geo_level) |>
    dplyr::arrange(Year, .by_group = TRUE) |>
    dplyr::mutate(
      values    = dplyr::if_else(
        Year == 2022 & is.na(values),
        values[Year == 2021][1],
        values
      ),
      has2022   = any(Year == 2022),
      pick_year = dplyr::if_else(has2022, 2022L, 2021L)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(Year == pick_year) |>
    dplyr::select(-Year, -has2022, -pick_year)

  # ── Impute regional NAs from country x sector median ────────────
  country_sector_median <- df |>
    dplyr::filter(geo_level == "regional") |>
    dplyr::group_by(country, nace_r2) |>
    dplyr::summarise(donor = median(values, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(donor = dplyr::if_else(is.nan(donor), NA_real_, donor))

  df <- df |>
    dplyr::left_join(country_sector_median, by = c("country", "nace_r2")) |>
    dplyr::mutate(
      values = dplyr::if_else(
        is.na(values) & geo_level == "regional" & !is.na(donor),
        donor, values
      )
    ) |>
    dplyr::select(-donor)

  # ── Re-anchor regionals to national sector totals ───────────────
  nat_tot <- df |>
    dplyr::filter(geo_level == "national") |>
    dplyr::select(country, nace_r2, nat_total = values)

  df <- df |>
    dplyr::left_join(nat_tot, by = c("country", "nace_r2")) |>
    dplyr::group_by(country, nace_r2) |>
    dplyr::mutate(
      sum_reg = sum(values[geo_level == "regional"], na.rm = TRUE),
      values  = dplyr::if_else(
        geo_level == "regional" & !is.na(nat_total) & sum_reg > 0,
        values * nat_total / sum_reg, values
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-nat_total, -sum_reg)

  # ── Enforce sum(C10-C33) == C within each geo (additivity) ──────
  c_geo <- df |>
    dplyr::filter(nace_r2 == "C") |>
    dplyr::select(geo, C_geo = values)

  df <- df |>
    dplyr::left_join(c_geo, by = "geo") |>
    dplyr::group_by(geo) |>
    dplyr::mutate(
      sub_sum = sum(values[nace_r2 != "C"], na.rm = TRUE),
      values  = dplyr::if_else(
        nace_r2 != "C" & !is.na(C_geo) & sub_sum > 0,
        values * C_geo / sub_sum, values
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-C_geo, -sub_sum)

  # ── Aggregate detailed NACE to 11 macro sectors ─────────────────
  # Uses sector_aggregation lookup table from utils.R
  df <- df |>
    dplyr::left_join(sector_aggregation, by = c("nace_r2" = "nace_detail")) |>
    dplyr::filter(!is.na(Sector_ID)) |>
    dplyr::group_by(geo, country, Sector_ID) |>
    dplyr::summarise(pers_employed = sum(values, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(NUTS_ID = geo, Country_ID = country)

  # ── Build employment shares (NUTS-2 only) ──────────────────────
  df <- df |>
    dplyr::filter(nchar(NUTS_ID) == 4) |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::mutate(
      country_emp = sum(pers_employed, na.rm = TRUE),
      weight      = dplyr::if_else(country_emp > 0,
                                   pers_employed / country_emp, NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-country_emp)

  # ── Fallback: use aggregate-C weights when sector weights are NA
  w_c <- df |>
    dplyr::filter(Sector_ID == "C") |>
    dplyr::select(Country_ID, NUTS_ID, weight_C = weight)

  df <- df |>
    dplyr::left_join(w_c, by = c("Country_ID", "NUTS_ID")) |>
    dplyr::mutate(weight = dplyr::coalesce(weight, weight_C)) |>
    dplyr::select(-weight_C)

  # ── Re-normalise weights to sum to 1 within country x sector ───
  df <- df |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::mutate(
      w_sum  = sum(weight, na.rm = TRUE),
      weight = dplyr::if_else(!is.na(w_sum) & w_sum > 0,
                              weight / w_sum, weight)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-w_sum)

  df |>
    dplyr::mutate(pers_employed = round(pers_employed)) |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, pers_employed, weight) |>
    tibble::as_tibble()
}


# ── 2) Diversification HHI ──────────────────────────────────────────────────

#' Compute Herfindahl-Hirschman Index of manufacturing employment concentration
#'
#' Higher HHI = more concentrated = less diversified = more vulnerable.
#' Uses 10 subsectors (excluding aggregate C): min HHI = 1/10 = 0.10.
#'
#' @param empl_shares_path Path to EMPL_Region.xlsx (employment shares by
#'   NUTS-2 region and sector)
#' @return Tibble with columns: Country_CD, NUTS_ID, Component, Dimension,
#'   Variable, Unit, Value
create_hhi <- function(empl_shares_path) {

  empl <- readxl::read_xlsx(empl_shares_path)

  hhi <- empl |>
    dplyr::filter(!is.na(Value), Value >= 0) |>
    dplyr::group_by(NUTS_ID) |>
    dplyr::summarise(
      n_sectors  = dplyr::n(),
      sum_shares = sum(Value, na.rm = TRUE),
      HHI        = sum(Value^2, na.rm = TRUE),
      .groups    = "drop"
    )

  hhi |>
    dplyr::transmute(
      Country_CD   = substr(NUTS_ID, 1, 2),
      Country_Name = NA_character_,
      NUTS_ID      = NUTS_ID,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = NA_character_,
      Component    = "Vulnerability",
      Dimension    = "Diversification",
      Variable     = "HHI_Employment",
      Unit         = "Index (0-1)",
      Value        = round(HHI, 6),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 3) Policy Pressure ──────────────────────────────────────────────────────

#' Create sector-level policy-pressure indicator (ETS + CBAM coverage)
#'
#' Hardcodes ETS and CBAM coverage scores per sector and replicates the
#' composite (equal-weighted average) across all NUTS-2 regions.
#'
#' @param base_data_path Path to base_data_plus.xlsx (provides NUTS-2 region
#'   list)
#' @return Tibble with columns: Country_CD, NUTS_ID, Sector_ID, Component,
#'   Dimension, Variable, Unit, Value
create_policy_pressure <- function(base_data_path) {

  policy_data <- tibble::tibble(
    Sector_ID = c(
      "C", "C10-C12", "C13-C15", "C16-C18", "C19-C20",
      "C21-C22", "C23", "C24", "C25+C28-C30", "C26-C27", "C31-C33"
    ),
    ETS_Coverage = c(
      0.50, 0.25, 0.00, 0.50, 1.00,
      0.25, 1.00, 1.00, 0.25, 0.00, 0.00
    ),
    CBAM_Coverage = c(
      0.25, 0.00, 0.00, 0.00, 0.50,
      0.00, 1.00, 1.00, 0.00, 0.00, 0.00
    )
  ) |>
    dplyr::mutate(Policy_Pressure = (ETS_Coverage + CBAM_Coverage) / 2)

  nuts2 <- readxl::read_xlsx(base_data_path) |>
    dplyr::filter(nchar(NUTS_ID) == 4) |>
    dplyr::select(NUTS_ID)

  tidyr::crossing(nuts2, policy_data) |>
    dplyr::transmute(
      Country_CD   = substr(NUTS_ID, 1, 2),
      Country_Name = NA_character_,
      NUTS_ID      = NUTS_ID,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = Sector_ID,
      Component    = "Exposure",
      Dimension    = "Exposure",
      Variable     = "Policy_Pressure",
      Unit         = "Index (0-1)",
      Value        = round(Policy_Pressure, 4),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 4) Scope 2 Emissions ────────────────────────────────────────────────────

#' Approximate Scope 2 GHG emissions for manufacturing subsectors
#'
#' Downloads electricity consumption from Eurostat (nrg_bal_c), multiplies by
#' hardcoded grid emission factors (EEA/EMBER 2022), and downscales to NUTS-2
#' regions via employment shares.
#'
#' @param base_data_path Path to base_data_plus.xlsx (not used directly but
#'   kept for pipeline symmetry)
#' @param empl_shares_path Path to EMPL_Region.xlsx (employment shares used
#'   for regional downscaling)
#' @return Tibble with columns: Country_ID, NUTS_ID, Sector_ID, Indicator,
#'   Unit, Value
create_scope2 <- function(base_data_path, empl_shares_path) {

  # ── Grid emission factors (gCO2/kWh), EEA/EMBER 2022 ──────────
  grid_ef <- tibble::tribble(
    ~Country_ID, ~Grid_EF_gCO2_kWh,
    "AT",   88, "BE",  149, "BG",  422, "HR",  151, "CY",  605,
    "CZ",  391, "DK",  108, "EE",  470, "FI",   73, "FR",   56,
    "DE",  366, "EL",  342, "HU",  208, "IE",  279, "IT",  247,
    "LV",   80, "LT",   64, "LU",   71, "MT",  389, "NL",  307,
    "PL",  663, "PT",  148, "RO",  248, "SK",  107, "SI",  215,
    "ES",  142, "SE",    8
  )

  # ── Download electricity consumption by industrial subsector ───
  elec_raw <- restatapi::get_eurostat_data(
    id          = "nrg_bal_c",
    filters     = list(siec = "E7000", unit = "GWH", geo = eu27),
    date_filter = 2022,
    exact_match = FALSE,
    label       = FALSE,
    cflags      = TRUE,
    keep_flags  = TRUE
  )

  ind_codes <- c(
    "FC_IND_FBT_E", "FC_IND_TL_E", "FC_IND_WP_E", "FC_IND_PPP_E",
    "FC_IND_CPC_E", "FC_IND_IS_E", "FC_IND_NFM_E", "FC_IND_NMM_E",
    "FC_IND_MAC_E", "FC_IND_TE_E", "FC_IND_NSP_E", "FC_IND_E"
  )

  elec <- elec_raw |>
    dplyr::filter(nrg_bal %in% ind_codes) |>
    dplyr::mutate(
      Country_ID = as.character(geo),
      Elec_GWh   = as.numeric(values)
    ) |>
    dplyr::select(Country_ID, nrg_bal, Elec_GWh)

  # ── Map energy-balance codes to aggregated sectors ─────────────
  sector_map <- tibble::tribble(
    ~nrg_bal,        ~Sector_ID,
    "FC_IND_FBT_E",  "C10-C12",
    "FC_IND_TL_E",   "C13-C15",
    "FC_IND_WP_E",   "C16-C18",
    "FC_IND_PPP_E",  "C16-C18",
    "FC_IND_CPC_E",  "C19-C20",
    "FC_IND_IS_E",   "C24",
    "FC_IND_NFM_E",  "C24",
    "FC_IND_NMM_E",  "C23",
    "FC_IND_MAC_E",  "C25+C28-C30",
    "FC_IND_TE_E",   "C25+C28-C30",
    "FC_IND_E",      "C"
  )

  elec_mapped <- elec |>
    dplyr::inner_join(sector_map, by = "nrg_bal") |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::summarise(Elec_GWh = sum(Elec_GWh, na.rm = TRUE), .groups = "drop")

  # ── Distribute FC_IND_NSP_E equally among unmapped sectors ─────
  # ASSUMPTION: FC_IND_NSP_E ("not elsewhere specified" industrial electricity)
  # is split equally among 3 subsectors with no direct Eurostat energy-balance
  # counterpart: C21-C22 (Pharmaceutical & Plastic), C26-C27 (Electronic &
  # Electrical), C31-C33 (Other Manufacturing & Repair). In practice,
  # electricity intensity varies across these sectors; alternatives include
  # employment-weighted or value-added-weighted allocation.
  nsp <- elec |>
    dplyr::filter(nrg_bal == "FC_IND_NSP_E") |>
    dplyr::select(Country_ID, NSP_GWh = Elec_GWh)

  unmapped_sectors <- c("C21-C22", "C26-C27", "C31-C33")
  n_unmapped <- length(unmapped_sectors)

  nsp_split <- nsp |>
    tidyr::crossing(Sector_ID = unmapped_sectors) |>
    dplyr::mutate(Elec_GWh = NSP_GWh / n_unmapped) |>
    dplyr::select(Country_ID, Sector_ID, Elec_GWh)

  elec_all <- dplyr::bind_rows(elec_mapped, nsp_split)

  # ── Scope 2 = Elec_GWh * Grid_EF / 1000 (tonnes CO2) ─────────
  scope2 <- elec_all |>
    dplyr::inner_join(grid_ef, by = "Country_ID") |>
    dplyr::mutate(Scope2_tCO2 = Elec_GWh * Grid_EF_gCO2_kWh / 1000)

  # ── Downscale to NUTS-2 using employment shares ────────────────
  empl <- readxl::read_xlsx(empl_shares_path) |>
    dplyr::select(NUTS_ID, Sector_ID, Share = Value) |>
    dplyr::mutate(Country_ID = substr(NUTS_ID, 1, 2))

  # Sector C: allocate via total employment share across regions
  empl_total <- empl |>
    dplyr::group_by(NUTS_ID) |>
    dplyr::summarise(Total_Share = sum(Share, na.rm = TRUE), .groups = "drop")

  empl_c <- empl_total |>
    dplyr::mutate(
      Country_ID = substr(NUTS_ID, 1, 2),
      Sector_ID  = "C"
    ) |>
    dplyr::group_by(Country_ID) |>
    dplyr::mutate(Share = Total_Share / sum(Total_Share, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(NUTS_ID, Sector_ID, Share, Country_ID)

  empl_weights <- dplyr::bind_rows(
    empl |>
      dplyr::group_by(Country_ID, Sector_ID) |>
      dplyr::mutate(Share = Share / sum(Share, na.rm = TRUE)) |>
      dplyr::ungroup(),
    empl_c
  )

  scope2_regional <- scope2 |>
    dplyr::select(Country_ID, Sector_ID, Scope2_tCO2) |>
    dplyr::left_join(empl_weights, by = c("Country_ID", "Sector_ID")) |>
    dplyr::mutate(Value = Scope2_tCO2 * Share) |>
    dplyr::filter(!is.na(NUTS_ID), !is.na(Value))

  scope2_regional |>
    dplyr::transmute(
      Country_ID = Country_ID,
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = "Scope2_Emissions",
      Unit       = "tCO2eq",
      Value      = round(Value, 2)
    ) |>
    tibble::as_tibble()
}


# ── 5) QoG Institutions ─────────────────────────────────────────────────────

#' Extract European Quality of Government Index (EQI) at NUTS-2 level
#'
#' Reads QoG EU Regional Dataset, filters to the 2017 EQI survey wave, and
#' keeps EU-27 NUTS-2 regions only.
#'
#' @param qog_path Path to qog_eureg.csv
#' @return Tibble with columns: Country_CD, NUTS_ID, Component, Dimension,
#'   Variable, Unit, Value
create_qog <- function(qog_path) {

  qog <- readr::read_csv(qog_path, show_col_types = FALSE)

  # Determine latest available EQI survey year (target: 2017)
  eqi_avail <- qog |>
    dplyr::filter(!is.na(eqi_score_nuts2)) |>
    dplyr::distinct(year) |>
    dplyr::pull(year)

  eqi_year <- if (2017 %in% eqi_avail) 2017L else max(eqi_avail)

  qog_filt <- qog |>
    dplyr::filter(
      year == eqi_year,
      !is.na(eqi_score_nuts2),
      !is.na(nuts2)
    ) |>
    dplyr::select(NUTS_ID = nuts2, EQI = eqi_score_nuts2)

  qog_filt |>
    dplyr::filter(
      nchar(NUTS_ID) == 4,
      substr(NUTS_ID, 1, 2) %in% eu27
    ) |>
    dplyr::transmute(
      Country_CD   = substr(NUTS_ID, 1, 2),
      Country_Name = NA_character_,
      NUTS_ID      = NUTS_ID,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = NA_character_,
      Component    = "Vulnerability",
      Dimension    = "Institutions",
      Variable     = "QoG_Index",
      Unit         = "EQI Score",
      Value        = round(EQI, 6),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 6) Climate Mitigation Laws ──────────────────────────────────────────────

#' Extract climate-mitigation-law count from QoG Environmental Indicators
#'
#' Reads QoG Environmental Indicators CSV, maps 3-letter ISO country codes to
#' 2-letter using iso3_to_iso2 from utils.R, extracts ccl_nmitlp for the
#' latest available year for EU-27 countries, and replicates the country-level
#' value to all NUTS-2 regions.
#'
#' @param qog_ei_path Path to QoG Environmental Indicators CSV
#' @param base_data_path Path to base_data_plus.xlsx (provides NUTS-2 list)
#' @return Tibble with columns: Country_CD, NUTS_ID, Component, Dimension,
#'   Variable, Unit, Value
create_climate_laws <- function(qog_ei_path, base_data_path) {


  # Try semicolon first, fall back to comma
  ei <- tryCatch(
    readr::read_delim(qog_ei_path, delim = ";", show_col_types = FALSE),
    warning = function(w) NULL,
    error   = function(e) NULL
  )
  if (is.null(ei) || ncol(ei) <= 1) {
    ei <- readr::read_csv(qog_ei_path, show_col_types = FALSE)
  }

  # Standardise column names to lowercase
  names(ei) <- tolower(names(ei))

  # Map 3-letter ISO -> 2-letter
  ei <- ei |>
    dplyr::rename(iso3 = ccodealp) |>
    dplyr::left_join(iso3_to_iso2, by = "iso3") |>
    dplyr::filter(iso2 %in% eu27)

  # Extract ccl_nmitlp for latest year per country
  laws <- ei |>
    dplyr::filter(!is.na(ccl_nmitlp)) |>
    dplyr::group_by(iso2) |>
    dplyr::filter(year == max(year)) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(Country_ID = iso2, Climate_Laws = ccl_nmitlp)

  # Replicate to all NUTS-2 regions

  nuts2 <- readxl::read_xlsx(base_data_path) |>
    dplyr::filter(nchar(NUTS_ID) == 4) |>
    dplyr::select(NUTS_ID) |>
    dplyr::mutate(Country_ID = substr(NUTS_ID, 1, 2))

  nuts2 |>
    dplyr::left_join(laws, by = "Country_ID") |>
    dplyr::transmute(
      Country_CD   = Country_ID,
      Country_Name = NA_character_,
      NUTS_ID      = NUTS_ID,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = NA_character_,
      Component    = "Vulnerability",
      Dimension    = "Institutions",
      Variable     = "Climate_Mitigation_Laws",
      Unit         = "Count",
      Value        = Climate_Laws,
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 7) Renewable Energy Potential ────────────────────────────────────────────

#' Compute total renewable-energy potential at NUTS-2 level from ENSPRESO
#'
#' Reads JRC ENSPRESO NUTS-2 integrated data (semicolon-separated CSV) and
#' computes total RE = wind_onshore + solar + biomass (medium scenario, TWh).
#'
#' @param enspreso_path Path to ENSPRESO_Integrated_NUTS2_Data.csv
#' @return Tibble with columns: Country_CD, NUTS_ID, Component, Dimension,
#'   Variable, Unit, Value
create_re_potential <- function(enspreso_path) {

  ensp <- readr::read_delim(enspreso_path, delim = ";", show_col_types = FALSE)

  ensp_clean <- ensp |>
    dplyr::mutate(
      Country_ID   = substr(nuts2_code, 1, 2),
      RE_Total_TWh = wind_onshore_production_twh_medium +
                     solar_production_twh_medium_total +
                     biomass_production_twh_medium_total
    ) |>
    dplyr::filter(Country_ID %in% eu27)

  ensp_clean |>
    dplyr::transmute(
      Country_CD   = Country_ID,
      Country_Name = NA_character_,
      NUTS_ID      = nuts2_code,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = NA_character_,
      Component    = "Vulnerability",
      Dimension    = "Diversification",
      Variable     = "RE_Potential",
      Unit         = "TWh (technical potential, medium scenario)",
      Value        = round(RE_Total_TWh, 4),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 8) EU Cohesion Fund Payments Per Capita ────────────────────────────────

#' Compute EU Cohesion Fund payments per capita at NUTS-2 level
#'
#' Downloads regionalised EU payment data (ERDF + CF + ESF, 2014-2020
#' programming period) from the Cohesion Open Data Portal, sums total
#' payments per NUTS-2 region, divides by population from Eurostat.
#'
#' @param base_data_path Path to base_data_plus.xlsx (provides NUTS-2 list)
#' @return Tibble with columns: Country_CD, NUTS_ID, Component, Dimension,
#'   Variable, Unit, Value
create_cohesion_fund <- function(base_data_path) {

  # ── 1. Download regionalised payment data from Cohesion Open Data ──
  # Dataset: "Historic EU payments annual timeseries - regionalised and modelled"
  # Socrata ID: tc55-7ysv
  coh_url <- paste0(
    "https://cohesiondata.ec.europa.eu/resource/tc55-7ysv.csv?",
    "$where=programming_period='2014-2020'",
    "%20AND%20fund%20in('ERDF','CF','ESF')",
    "&$limit=50000"
  )
  coh_raw <- readr::read_csv(coh_url, show_col_types = FALSE)

  # ── 2. Sum annual payments across years and funds per NUTS-2 ───────
  coh <- coh_raw |>
    dplyr::filter(
      !is.na(nuts2_id),
      nchar(nuts2_id) == 4,
      substr(nuts2_id, 1, 2) %in% eu27
    ) |>
    dplyr::group_by(nuts2_id) |>
    dplyr::summarise(
      total_payments = sum(as.numeric(eu_payment_annual), na.rm = TRUE),
      .groups = "drop"
    )

  # ── 3. NUTS-2013 -> NUTS-2021 recombination ────────────────────────
  hr_remap <- coh |>
    dplyr::filter(nuts2_id %in% c("HR02", "HR05", "HR06")) |>
    dplyr::summarise(total_payments = sum(total_payments, na.rm = TRUE)) |>
    dplyr::mutate(nuts2_id = "HR04")

  nl_remap <- coh |>
    dplyr::filter(nuts2_id %in% c("NL35", "NL36")) |>
    dplyr::mutate(nuts2_id = dplyr::case_when(
      nuts2_id == "NL35" ~ "NL31",
      nuts2_id == "NL36" ~ "NL33"
    ))

  pt_remap <- coh |>
    dplyr::filter(nuts2_id %in% c("PT19", "PT1A", "PT1B", "PT1C", "PT1D")) |>
    dplyr::mutate(target = dplyr::case_when(
      nuts2_id %in% c("PT19", "PT1D") ~ "PT16",
      nuts2_id %in% c("PT1A", "PT1B") ~ "PT17",
      nuts2_id == "PT1C"              ~ "PT18"
    )) |>
    dplyr::group_by(target) |>
    dplyr::summarise(total_payments = sum(total_payments, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rename(nuts2_id = target)

  obsolete_nuts <- c("HR02", "HR05", "HR06", "NL35", "NL36",
                     "PT19", "PT1A", "PT1B", "PT1C", "PT1D")
  coh <- coh |>
    dplyr::filter(!nuts2_id %in% obsolete_nuts) |>
    dplyr::bind_rows(hr_remap, nl_remap, pt_remap) |>
    dplyr::group_by(nuts2_id) |>
    dplyr::summarise(total_payments = sum(total_payments, na.rm = TRUE),
                     .groups = "drop")

  # ── 4. Download NUTS-2 population from Eurostat ────────────────────
  pop_raw <- restatapi::get_eurostat_data(
    id          = "demo_r_d2jan",
    filters     = list(sex = "T", age = "TOTAL"),
    date_filter = 2020,
    exact_match = FALSE,
    label       = FALSE
  )

  pop <- pop_raw |>
    dplyr::mutate(geo = as.character(geo)) |>
    dplyr::filter(nchar(geo) == 4, substr(geo, 1, 2) %in% eu27) |>
    dplyr::transmute(nuts2_id = geo, population = as.numeric(values))

  # Apply same NUTS recombination to population
  pop_hr <- pop |>
    dplyr::filter(nuts2_id %in% c("HR02", "HR05", "HR06")) |>
    dplyr::summarise(population = sum(population, na.rm = TRUE)) |>
    dplyr::mutate(nuts2_id = "HR04")

  pop_nl <- pop |>
    dplyr::filter(nuts2_id %in% c("NL35", "NL36")) |>
    dplyr::mutate(nuts2_id = dplyr::case_when(
      nuts2_id == "NL35" ~ "NL31",
      nuts2_id == "NL36" ~ "NL33"
    ))

  pop_pt <- pop |>
    dplyr::filter(nuts2_id %in% c("PT19", "PT1A", "PT1B", "PT1C", "PT1D")) |>
    dplyr::mutate(target = dplyr::case_when(
      nuts2_id %in% c("PT19", "PT1D") ~ "PT16",
      nuts2_id %in% c("PT1A", "PT1B") ~ "PT17",
      nuts2_id == "PT1C"              ~ "PT18"
    )) |>
    dplyr::group_by(target) |>
    dplyr::summarise(population = sum(population, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rename(nuts2_id = target)

  pop <- pop |>
    dplyr::filter(!nuts2_id %in% obsolete_nuts) |>
    dplyr::bind_rows(pop_hr, pop_nl, pop_pt) |>
    dplyr::group_by(nuts2_id) |>
    dplyr::summarise(population = sum(population, na.rm = TRUE),
                     .groups = "drop")

  # ── 5. Per-capita calculation ──────────────────────────────────────
  result <- coh |>
    dplyr::inner_join(pop, by = "nuts2_id") |>
    dplyr::mutate(per_capita = total_payments / population) |>
    dplyr::filter(!is.na(per_capita), is.finite(per_capita))

  # ── 6. Filter to valid NUTS-2 regions from base data ───────────────
  nuts2_valid <- readxl::read_xlsx(base_data_path) |>
    dplyr::filter(nchar(NUTS_ID) == 4) |>
    dplyr::pull(NUTS_ID)

  result |>
    dplyr::filter(nuts2_id %in% nuts2_valid) |>
    dplyr::transmute(
      Country_CD   = substr(nuts2_id, 1, 2),
      Country_Name = NA_character_,
      NUTS_ID      = nuts2_id,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = NA_character_,
      Component    = "Vulnerability",
      Dimension    = "Finance",
      Variable     = "Cohesion_Fund",
      Unit         = "EUR per capita (2014-2020 total, ERDF+CF+ESF)",
      Value        = round(per_capita, 2),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}
