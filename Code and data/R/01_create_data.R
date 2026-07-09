# 01_create_data.R — Eurostat indicator creators. Each function pulls one
# Eurostat dataset, picks the latest year with EU-27 coverage, downscales
# to NUTS-2 if needed, and returns a tibble. `targets` handles persistence.


#' Regional employment weights from `sbs_r_nuts2021` (EMP_LOC_NR).
#' Handles confidential cells, imputes regional NAs from country x sector
#' medians, re-anchors regional sums to national totals, enforces additivity
#' of sub-sectors to Sector C, and computes weights normalised within each
#' Country x Sector.
#'
#' @return Tibble: Country_ID, NUTS_ID, Sector_ID, pers_employed, weight.
create_employment_weights <- function(base_data_path) {

  nace_codes <- c(
    "C", "C10", "C11", "C12", "C13", "C14", "C15",
    "C16", "C17", "C18", "C19", "C20", "C21", "C22",
    "C23", "C24", "C25", "C26", "C27", "C28", "C29",
    "C30", "C31", "C32", "C33"
  )


  # ── Download from Eurostat (last 5 years; helper picks the best) ─
  raw <- restatapi::get_eurostat_data(
    id          = "sbs_r_nuts2021",
    filters     = list(
      indic_sbs = "EMP_LOC_NR",
      sizeclas  = "TOTAL",
      nace_r2   = nace_codes
    ),
    date_filter = seq(as.integer(format(Sys.Date(), "%Y")) - 5L,
                      as.integer(format(Sys.Date(), "%Y"))),
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

  # Pick latest year with full NUTS-2 coverage; per-cell fall back to the
  # most recent prior non-NA value if a region is missing in the chosen year.
  pick <- pick_latest_complete_year(
    df |> dplyr::filter(nchar(geo) == 4, nace_r2 == "C"),
    geo_dim = "geo", value_col = "values",
    expected_geos = unique(base_d$geo[nchar(base_d$geo) == 4]),
    max_years_back = 5L
  )

  # Per-cell fallback: for each (geo, nace_r2), use the most recent year with
  # a non-NA value within the search window. This handles two cases that the
  # legacy single-year filter dropped:
  #   (a) row exists at pick$year but values is NA → fill from earlier year
  #   (b) row entirely absent at pick$year (e.g. LV00 in 2023 sbs_r_nuts2021)
  #       → take the row from the most recent prior year present
  df <- df |>
    dplyr::mutate(Year = as.integer(as.character(time))) |>
    dplyr::filter(!is.na(values)) |>
    dplyr::group_by(geo, country, nace_r2, geo_level) |>
    dplyr::arrange(dplyr::desc(Year), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-Year)

  attr(df, "year_selected") <- pick$year
  attr(df, "year_coverage") <- pick$coverage

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

  out <- df |>
    dplyr::mutate(pers_employed = round(pers_employed)) |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, pers_employed, weight) |>
    tibble::as_tibble()

  attr(out, "year_selected") <- pick$year
  attr(out, "year_coverage") <- pick$coverage
  attr(out, "source_dataset") <- "sbs_r_nuts2021"
  out
}


# ── 1b) Write EMPL_Region.xlsx from empl_weights target ─────────────────────

#' Materialise the empl_weights tibble as EMPL_Region.xlsx
#'
#' EMPL_Region.xlsx is used as input by create_hhi(), create_scope2() and
#' create_scope3() for downscaling. We rebuild it from the in-memory
#' empl_weights target so its sector grouping always matches the current
#' sector_aggregation lookup (e.g. C25+C28 vs C29-C30 after the split).
#'
#' Output format (mirrors the original file produced outside the pipeline):
#'   NUTS_ID, Sector_ID, Sector_Name, Dimension, Indicator, Unit, Value
#'   where Value = within-NUTS-2 share of manufacturing employment (sums to 1
#'   per NUTS_ID across the 10 sub-sectors; the aggregate "C" is excluded).
#'
#' @param empl_weights tibble from create_employment_weights()
#' @param out_path     destination path (will overwrite)
#' @return out_path
write_empl_region_xlsx <- function(empl_weights, out_path) {

  df <- empl_weights |>
    dplyr::filter(Sector_ID != "C", !is.na(pers_employed)) |>
    dplyr::group_by(NUTS_ID) |>
    dplyr::mutate(
      total = sum(pers_employed, na.rm = TRUE),
      Value = dplyr::if_else(total > 0, pers_employed / total, NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      NUTS_ID,
      Sector_ID,
      Sector_Name = sector_name_map[Sector_ID],
      Dimension   = "Labor",
      Indicator   = "Share_of_Employment",
      Unit        = "Percentage",
      Value       = round(Value, 6)
    )

  writexl::write_xlsx(df, out_path)
  out_path
}


# ── 2) Sector concentration (Diversification dimension) ────────────────────

#' Share of the focal sector in the region's total manufacturing
#' employment, per (NUTS-2 region x NACE sub-sector). Higher share means
#' the region depends more heavily on that sector and has less alternative
#' employment to absorb a shock to it -- so this is the vulnerability-side
#' input to the Diversification dimension.
#'
#' Replaces an earlier region-only Herfindahl-Hirschman index, which by
#' construction did not vary across sectors and so produced identical
#' Diversification maps for every sector.
create_sector_concentration <- function(empl_shares_path) {

  empl <- readxl::read_xlsx(empl_shares_path)

  empl |>
    dplyr::filter(!is.na(Value), Value >= 0) |>
    dplyr::transmute(
      Country_CD   = substr(NUTS_ID, 1, 2),
      Country_Name = NA_character_,
      NUTS_ID      = NUTS_ID,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = Sector_ID,
      Component    = "Vulnerability",
      Dimension    = "Diversification",
      Variable     = "Sector_Concentration",
      Unit         = "Share of regional manufacturing employment [0, 1]",
      Value        = round(Value, 6),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 3) Policy Pressure ──────────────────────────────────────────────────────

#' Policy_Pressure per NACE sector = sum of hard-coded ETS and CBAM
#' coverage scores, replicated to every NUTS-2 region. The two schemes
#' stack rather than substitute: a sector covered by both faces both
#' costs simultaneously (ETS prices direct emissions, CBAM prices the
#' carbon content of imports of the same goods), so the cumulative
#' regulatory pressure is additive. Raw range [0, 2]; min-max
#' normalisation in 04_normalize.R rescales to [0.01, 0.99] within the
#' indicator.
create_policy_pressure <- function(base_data_path) {

  policy_data <- tibble::tibble(
    Sector_ID = c(
      "C", "C10-C12", "C13-C15", "C16-C18", "C19-C20",
      "C21-C22", "C23", "C24", "C25+C28", "C29-C30", "C26-C27", "C31-C33"
    ),
    ETS_Coverage = c(
      0.50, 0.25, 0.00, 0.50, 1.00,
      0.25, 1.00, 1.00, 0.25, 0.00, 0.00, 0.00
    ),
    CBAM_Coverage = c(
      0.25, 0.00, 0.00, 0.00, 0.50,
      0.00, 1.00, 1.00, 0.00, 0.00, 0.00, 0.00
    )
  ) |>
    dplyr::mutate(Policy_Pressure = ETS_Coverage + CBAM_Coverage)

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
      Unit         = "Score (0-2)",
      Value        = round(Policy_Pressure, 4),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 4) Scope 2 Emissions ────────────────────────────────────────────────────

#' Approximate Scope 2 GHG emissions = national industrial electricity
#' consumption (nrg_bal_c) x country-level grid emission factor
#' (EEA/EMBER 2022), then downscaled to NUTS-2 by employment share.
create_scope2 <- function(base_data_path, empl_weights) {

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

  ind_codes <- c(
    "FC_IND_FBT_E", "FC_IND_TL_E", "FC_IND_WP_E", "FC_IND_PPP_E",
    "FC_IND_CPC_E", "FC_IND_IS_E", "FC_IND_NFM_E", "FC_IND_NMM_E",
    "FC_IND_MAC_E", "FC_IND_TE_E", "FC_IND_NSP_E", "FC_IND_E"
  )

  # ── Download electricity consumption (multi-year for year-picker) ─
  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  elec_raw <- restatapi::get_eurostat_data(
    id          = "nrg_bal_c",
    filters     = list(siec = "E7000", unit = "GWH", geo = eu27,
                       nrg_bal = ind_codes),
    date_filter = seq(this_yr - 5L, this_yr),
    exact_match = TRUE,
    label       = FALSE,
    cflags      = TRUE,
    keep_flags  = TRUE
  )

  elec_raw <- elec_raw |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values)) |>
    dplyr::filter(geo %in% eu27, nrg_bal == "FC_IND_E")

  pick <- pick_latest_complete_year(
    elec_raw, geo_dim = "geo", value_col = "values",
    expected_geos = eu27, max_years_back = 5L
  )

  elec <- restatapi::get_eurostat_data(
    id          = "nrg_bal_c",
    filters     = list(siec = "E7000", unit = "GWH", geo = eu27,
                       nrg_bal = ind_codes),
    date_filter = pick$year,
    exact_match = TRUE, label = FALSE
  ) |>
    dplyr::mutate(
      Country_ID = as.character(geo),
      Elec_GWh   = as.numeric(values)
    ) |>
    dplyr::filter(Country_ID %in% eu27, nrg_bal %in% ind_codes) |>
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
    "FC_IND_MAC_E",  "C25+C28",
    "FC_IND_TE_E",   "C29-C30",
    "FC_IND_E",      "C"
  )

  elec_mapped <- elec |>
    dplyr::inner_join(sector_map, by = "nrg_bal") |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::summarise(Elec_GWh = sum(Elec_GWh, na.rm = TRUE), .groups = "drop")

  # FC_IND_NSP_E ("not elsewhere specified") is split equally among the
  # three sub-sectors without a direct nrg_bal counterpart: C21-C22, C26-C27,
  # C31-C33.
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
  scope2_regional <- downscale_national_to_nuts2(
    national_df      = scope2 |> dplyr::select(Country_ID, Sector_ID, Scope2_tCO2),
    empl_weights = empl_weights,
    value_cols       = "Scope2_tCO2"
  )

  out <- scope2_regional |>
    dplyr::transmute(
      Country_ID = Country_ID,
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = "Scope2_Emissions",
      Unit       = "tCO2eq",
      Value      = round(Scope2_tCO2, 2)
    ) |>
    tibble::as_tibble()

  attr(out, "year_selected") <- pick$year
  attr(out, "source_dataset") <- "nrg_bal_c"
  out
}


# ── 4b) Scope 3 Emissions (Producer-side MRIO upstream) ─────────────────────

#' Producer-side upstream Scope 3 via MRIO Leontief: for each (country,
#' industry), the upstream multiplier is m_j = sum_i f_i (L_{i,j} - delta_{i,j})
#' with f = Scope 1 emissions / total output, L = (I - A)^{-1}, A = Z diag(1/x).
#' Electricity (D35) is set to f[D35] = 0 to avoid double-counting Scope 2.
#' FIGARO IO and GHG tables are cached as RDS on first call.
create_scope3 <- function(empl_weights) {

  # ── 0. Pick latest year for FIGARO tables (both must agree) ────
  toc <- restatapi::get_eurostat_toc()
  fig_io_end  <- toc$dataEnd[toc$code == "naio_10_fcp_ii4"][1]
  fig_ghg_end <- toc$dataEnd[toc$code == "env_ac_ghgfp"][1]
  figaro_year <- as.integer(min(fig_io_end, fig_ghg_end))

  io_cache  <- sprintf("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_%d.rds", figaro_year)
  ghg_cache <- sprintf("Initial data/Non sector data/FIGARO_env_ac_ghgfp_%d.rds", figaro_year)

  # ── 1. Cache FIGARO IO table (one-off bulk download) ───────────
  if (!file.exists(io_cache)) {
    options(timeout = 600)
    io_all <- restatapi::get_eurostat_bulk(
      id = "naio_10_fcp_ii4", check_toc = FALSE
    )
    io_all <- tibble::as_tibble(io_all) |>
      dplyr::filter(time == figaro_year) |>
      dplyr::select(-time, -unit) |>
      dplyr::mutate(values = as.numeric(values))
    saveRDS(io_all, io_cache, compress = "xz")
  }
  io <- readRDS(io_cache)

  # ── 2. Universe of real industries and regions ─────────────────
  real_ind <- c(
    "A01","A02","A03","B","C10-12","C13-15","C16","C17","C18",
    "C19","C20","C21","C22","C23","C24","C25","C26","C27","C28",
    "C29","C30","C31_32","C33","D35","E36","E37-39","F","G45",
    "G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60",
    "J61","J62_63","K64","K65","K66","L","M69_70","M71","M72",
    "M73","M74_75","N77","N78","N79","N80-82","O84","P85","Q86",
    "Q87_88","R90-92","R93","S94","S95","S96","T","U"
  )
  fd_use   <- c("P3_S13","P3_S14","P3_S15","P5M","P51G")
  regions  <- sort(setdiff(unique(io$c_orig), "DOM"))
  stopifnot(length(real_ind) == 64, length(regions) == 50)

  # ── 3. Build (region, industry) -> integer index ───────────────
  N <- length(regions) * length(real_ind)
  idx <- expand.grid(region = regions, ind = real_ind,
                     stringsAsFactors = FALSE) |>
    dplyr::mutate(k = dplyr::row_number())
  idx$key <- paste(idx$region, idx$ind, sep = "|")
  key_to_k <- setNames(idx$k, idx$key)

  # ── 4. Sparse intermediate flow matrix Z ───────────────────────
  Z_rows <- io |>
    dplyr::filter(
      c_orig != "DOM",
      c_orig %in% regions, c_dest %in% regions,
      ind_ava %in% real_ind, ind_use %in% real_ind,
      !is.na(values), values > 0
    ) |>
    dplyr::mutate(
      i = key_to_k[paste(c_orig, ind_ava, sep = "|")],
      j = key_to_k[paste(c_dest, ind_use, sep = "|")]
    )
  Z <- Matrix::sparseMatrix(i = Z_rows$i, j = Z_rows$j,
                            x = Z_rows$values, dims = c(N, N))

  # ── 5. Total output x = rowSum(Z) + final demand ───────────────
  fd_rows <- io |>
    dplyr::filter(
      c_orig != "DOM",
      c_orig %in% regions, c_dest %in% regions,
      ind_ava %in% real_ind, ind_use %in% fd_use, !is.na(values)
    ) |>
    dplyr::group_by(c_orig, ind_ava) |>
    dplyr::summarise(fd = sum(values, na.rm = TRUE), .groups = "drop")

  x_int <- Matrix::rowSums(Z)
  fd_vec <- numeric(N)
  fd_idx <- key_to_k[paste(fd_rows$c_orig, fd_rows$ind_ava, sep = "|")]
  fd_vec[fd_idx] <- fd_rows$fd
  x_tot <- x_int + fd_vec

  # ── 6. A matrix and Leontief inverse L = (I - A)^{-1} ──────────
  x_inv <- ifelse(x_tot > 0, 1 / x_tot, 0)
  A <- as.matrix(Z %*% Matrix::Diagonal(N, x_inv))
  L <- solve(diag(N) - A)

  # ── 7. Cache producer-side emissions (chunked API download) ────
  if (!file.exists(ghg_cache)) {
    chunks <- split(regions, ceiling(seq_along(regions) / 10))
    ghg_list <- list()
    for (ci in seq_along(chunks)) {
      d <- restatapi::get_eurostat_data(
        id = "env_ac_ghgfp",
        filters = list(na_item = "TOTAL",
                       c_orig = chunks[[ci]],
                       nace_r2 = real_ind),
        date_filter = figaro_year, exact_match = TRUE, label = FALSE
      )
      if (!is.null(d) && nrow(d) > 0) {
        ghg_list[[ci]] <- tibble::as_tibble(d) |>
          dplyr::mutate(values = as.numeric(values)) |>
          dplyr::select(c_orig, c_dest, nace_r2, values)
      }
    }
    ghg_all <- dplyr::bind_rows(ghg_list)
    saveRDS(ghg_all, ghg_cache, compress = "xz")
  }
  ghg <- readRDS(ghg_cache) |>
    dplyr::filter(c_orig %in% regions, nace_r2 %in% real_ind) |>
    dplyr::group_by(c_orig, nace_r2) |>
    dplyr::summarise(scope1_kt = sum(values, na.rm = TRUE), .groups = "drop")

  # ── 8. f = tCO2eq per MEUR output, with f[D35] := 0 ────────────
  ghg$k <- key_to_k[paste(as.character(ghg$c_orig),
                          as.character(ghg$nace_r2), sep = "|")]
  ghg <- ghg |> dplyr::filter(!is.na(k))
  emis_t <- numeric(N)
  emis_t[ghg$k] <- ghg$scope1_kt * 1000
  f <- ifelse(x_tot > 0, emis_t / x_tot, 0)
  f[idx$k[idx$ind == "D35"]] <- 0     # exclude electricity (Scope 2 boundary)

  # ── 9. Upstream multiplier (Scope 3 intensity per MEUR output) ─
  m_total <- as.numeric(t(f) %*% (L - diag(N)))
  m_own   <- f * (diag(L) - 1)         # subtract own-sector self-loop
  m_up    <- pmax(m_total - m_own, 0)

  # ── 10. Scope 3 emissions per (region, industry) ──────────────
  scope3 <- idx |>
    dplyr::mutate(Scope3_tCO2 = m_up * x_tot) |>
    dplyr::select(region, ind, Scope3_tCO2)

  # ── 11. Aggregate FIGARO NACE -> 11 manufacturing groups ──────
  nace_map <- tibble::tribble(
    ~ind,        ~Sector_ID,
    "C10-12",    "C10-C12",
    "C13-15",    "C13-C15",
    "C16",       "C16-C18", "C17", "C16-C18", "C18", "C16-C18",
    "C19",       "C19-C20", "C20", "C19-C20",
    "C21",       "C21-C22", "C22", "C21-C22",
    "C23",       "C23",
    "C24",       "C24",
    "C25",       "C25+C28", "C28", "C25+C28",
    "C29",       "C29-C30", "C30", "C29-C30",
    "C26",       "C26-C27", "C27", "C26-C27",
    "C31_32",    "C31-C33", "C33", "C31-C33"
  )
  s3_grp <- scope3 |>
    dplyr::inner_join(nace_map, by = "ind") |>
    dplyr::group_by(Country_ID = region, Sector_ID) |>
    dplyr::summarise(Scope3_tCO2 = sum(Scope3_tCO2, na.rm = TRUE),
                     .groups = "drop")
  s3_C <- s3_grp |>
    dplyr::group_by(Country_ID) |>
    dplyr::summarise(Sector_ID = "C",
                     Scope3_tCO2 = sum(Scope3_tCO2, na.rm = TRUE),
                     .groups = "drop")
  s3_country <- dplyr::bind_rows(s3_grp, s3_C) |>
    dplyr::filter(Country_ID %in% eu27)

  # ── 12. Downscale to NUTS-2 via employment shares ─────────────
  out <- downscale_national_to_nuts2(
    national_df      = s3_country,
    empl_weights = empl_weights,
    value_cols       = "Scope3_tCO2"
  ) |>
    dplyr::transmute(
      Country_ID = Country_ID,
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = "Scope3_Emissions",
      Unit       = "tCO2eq",
      Value      = round(Scope3_tCO2, 2)
    ) |>
    tibble::as_tibble()

  attr(out, "year_selected") <- figaro_year
  attr(out, "source_dataset") <- "naio_10_fcp_ii4 + env_ac_ghgfp"
  out
}


# ── 5) QoG Institutions ─────────────────────────────────────────────────────

#' Extract European Quality of Government Index (EQI) at NUTS-2 level
#'
#' Reads the standalone EQI regional release (qog_eqi_long_24.csv, Charron
#' et al. 2024; waves 2010-2024) and uses the requested wave (default 2024,
#' the latest). Countries surveyed at NUTS-2 enter directly (NUTS-2021
#' codes; HR02/HR05/HR06 are recombined to HR04 downstream in
#' reshape_to_grid). Belgium and Germany are surveyed at NUTS-1 and are
#' replicated to their NUTS-2 regions; the single-NUTS-2 states (CY, EE,
#' LU, LV, MT) carry the national score.
#'
#' @param qog_path Path to qog_eqi_long_24.csv
#' @param wave EQI survey year to use (default 2024)
#' @return Tibble with columns: Country_CD, NUTS_ID, Component, Dimension,
#'   Variable, Unit, Value
create_qog <- function(qog_path, base_data_path, wave = 2024L) {

  eqi <- readr::read_csv(qog_path, show_col_types = FALSE)

  w <- eqi |>
    dplyr::filter(year == wave, NUTS0_code %in% eu27, !is.na(EQI))
  if (nrow(w) == 0) stop("create_qog: no EQI rows for wave ", wave)

  # NUTS-2 region list per country (base grid, NUTS-2021)
  nuts2_grid <- readxl::read_xlsx(base_data_path) |>
    dplyr::filter(nchar(NUTS_ID) == 4,
                  substr(NUTS_ID, 1, 2) %in% eu27) |>
    dplyr::distinct(NUTS_ID) |>
    dplyr::mutate(Country_CD = substr(NUTS_ID, 1, 2),
                  NUTS1_CD   = substr(NUTS_ID, 1, 3))

  # ── Pass 1: countries surveyed at NUTS-2 — use directly ────────
  qog_nuts2 <- w |>
    dplyr::filter(nuts_level == 2, !is.na(NUTS2_code)) |>
    dplyr::transmute(NUTS_ID = NUTS2_code, EQI, EQI_level = "NUTS-2")

  # ── Pass 2: countries surveyed at NUTS-1 (BE, DE) — replicate ──
  qog_nuts1 <- nuts2_grid |>
    dplyr::inner_join(w |>
                        dplyr::filter(nuts_level == 1, !is.na(NUTS1_code)) |>
                        dplyr::transmute(NUTS1_CD = NUTS1_code, EQI),
                      by = "NUTS1_CD") |>
    dplyr::transmute(NUTS_ID, EQI, EQI_level = "NUTS-1 (replicated)")

  # ── Pass 3: countries surveyed nationally (single-NUTS-2 states) ─
  qog_nuts0 <- nuts2_grid |>
    dplyr::inner_join(w |>
                        dplyr::filter(nuts_level == 0) |>
                        dplyr::transmute(Country_CD = NUTS0_code, EQI),
                      by = "Country_CD") |>
    dplyr::transmute(NUTS_ID, EQI, EQI_level = "NUTS-0 (national, replicated)")

  combined <- dplyr::bind_rows(qog_nuts2, qog_nuts1, qog_nuts0)

  # One row per NUTS_ID with the most specific EQI level available.
  level_priority <- c("NUTS-2" = 1L,
                      "NUTS-1 (replicated)" = 2L,
                      "NUTS-0 (national, replicated)" = 3L)
  collapsed <- combined |>
    dplyr::mutate(.prio = level_priority[EQI_level]) |>
    dplyr::filter(!is.na(EQI)) |>
    dplyr::group_by(NUTS_ID) |>
    dplyr::slice_min(.prio, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-.prio)

  collapsed |>
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
      Value_Norm   = NA_real_,
      EQI_Level    = EQI_level
    ) |>
    tibble::as_tibble()
}


# ── 6) Climate Mitigation Laws ──────────────────────────────────────────────

#' Climate_Mitigation_Laws = QoG Environmental Indicators `ccl_nmitlp`
#' (cumulative mitigation laws count) for the latest year per country,
#' replicated to all NUTS-2 of the country.
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

#' RE_Potential per NUTS-2 = wind_onshore + solar + biomass (medium
#' scenario, TWh) from JRC ENSPRESO integrated NUTS-2 data.
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

  # ENSPRESO carries pre-2016 NUTS-2 codes. Without a crosswalk, all of
  # France, six Polish regions, HU1x, Ireland and Lithuania fail the join and
  # were silently country-median-imputed (bug found 2026-07-03). Two steps:
  # (1) 1:1 renames, verified geometrically against gisco NUTS-2013 vs
  # NUTS-2021 polygons (>=97% area overlap for every pair);
  # (2) area-share splits for the reorganised regions, with shares computed
  # at run time from gisco NUTS-2021 polygon areas (EPSG:3035) — a disclosed
  # approximation for an extensive land-based potential (TWh).
  # HR04 needs no mapping: ENSPRESO's (pre-split) HR04 equals the recombined
  # HR04 of the 230-region grid and now survives reshape (03_reshape.R).
  renames <- c(FR21 = "FRF2", FR22 = "FRE2", FR23 = "FRD2", FR24 = "FRB0",
               FR25 = "FRD1", FR26 = "FRC1", FR30 = "FRE1", FR41 = "FRF3",
               FR42 = "FRF1", FR43 = "FRC2", FR51 = "FRG0", FR52 = "FRH0",
               FR53 = "FRI3", FR61 = "FRI1", FR62 = "FRJ2", FR63 = "FRI2",
               FR71 = "FRK2", FR72 = "FRK1", FR81 = "FRJ1", FR82 = "FRL0",
               FR83 = "FRM0",
               PL11 = "PL71", PL31 = "PL81", PL32 = "PL82", PL33 = "PL72",
               PL34 = "PL84")
  splits <- list(list(old = "PL12",            new = c("PL91", "PL92")),
                 list(old = "HU10",            new = c("HU11", "HU12")),
                 list(old = "LT00",            new = c("LT01", "LT02")),
                 list(old = c("IE01", "IE02"), new = c("IE04", "IE05", "IE06")))

  v <- ensp_clean |>
    dplyr::mutate(NUTS_ID = dplyr::coalesce(renames[nuts2_code], nuts2_code)) |>
    dplyr::group_by(Country_ID, NUTS_ID) |>
    dplyr::summarise(RE_Total_TWh = sum(RE_Total_TWh, na.rm = TRUE),
                     .groups = "drop")

  n21 <- giscoR::gisco_get_nuts(nuts_level = "2", year = "2021",
                                resolution = "10") |>
    sf::st_transform(3035)
  areas <- tibble::tibble(NUTS_ID = n21$NUTS_ID,
                          area    = as.numeric(sf::st_area(n21)))
  split_rows <- purrr::map_dfr(splits, function(s) {
    tot <- sum(v$RE_Total_TWh[v$NUTS_ID %in% s$old], na.rm = TRUE)
    areas |>
      dplyr::filter(NUTS_ID %in% s$new) |>
      dplyr::transmute(Country_ID   = substr(NUTS_ID, 1, 2),
                       NUTS_ID,
                       RE_Total_TWh = tot * area / sum(area))
  })
  v <- v |>
    dplyr::filter(!NUTS_ID %in% unlist(lapply(splits, `[[`, "old"))) |>
    dplyr::bind_rows(split_rows)

  v |>
    dplyr::transmute(
      Country_CD   = Country_ID,
      Country_Name = NA_character_,
      NUTS_ID      = NUTS_ID,
      NUTS_Name    = NA_character_,
      Sector_CD    = NA_character_,
      Sector_ID    = NA_character_,
      Component    = "Vulnerability",
      Dimension    = "Energy",
      Variable     = "RE_Potential",
      Unit         = "TWh (technical potential, medium scenario)",
      Value        = round(RE_Total_TWh, 4),
      Value_Norm   = NA_real_
    ) |>
    tibble::as_tibble()
}


# ── 8) EU Cohesion Fund Payments Per Capita ────────────────────────────────

#' Cohesion_Fund per capita per NUTS-2 = regionalised ERDF + CF + ESF
#' payments for the 2014-2020 programming period (Cohesion Open Data Portal,
#' Socrata `tc55-7ysv`) divided by NUTS-2 population (demo_r_d2jan, 2020).
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


# NUTS-2 direct Eurostat indicators (no downscaling).


#' Pull a Eurostat NUTS-2 table and return the latest year with full
#' EU-27 NUTS-2 coverage: (Country_CD, NUTS_ID, Year, Value).
.fetch_nuts2_latest <- function(id, filters, base_data_path,
                                year_col = "time", value_col = "values",
                                max_years_back = 5L,
                                agg = c("mean", "sum")) {
  agg <- match.arg(agg)

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = id,
    filters     = filters,
    date_filter = seq(this_yr - max_years_back, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo),
                  values = as.numeric(values))

  # Recode NUTS-2024 geographies onto the pipeline grid BEFORE the grid
  # filter. Eurostat vintages from 2024 on arrive in NUTS-2024 codes
  # (Utrecht NL31->NL35, Zuid-Holland NL33->NL36; Portugal PT16/17/18 split
  # into PT19/PT1A-PT1D); the old `geo %in% base_d` filter silently dropped
  # them, so those regions were written as missing and country-median-imputed
  # downstream (bug found 2026-07-09). Merged codes are aggregated with the
  # indicator's rule: mean for intensive (labour rates), sum for extensive
  # (GFCF), mirroring `agg_rules`.
  nuts24_to_grid <- c(NL35 = "NL31", NL36 = "NL33",
                      PT19 = "PT16", PT1D = "PT16",
                      PT1A = "PT17", PT1B = "PT17", PT1C = "PT18")
  agg_fun <- if (agg == "sum") {
    function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  } else {
    function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }
  raw <- raw |>
    dplyr::mutate(geo = dplyr::coalesce(nuts24_to_grid[geo], geo)) |>
    dplyr::group_by(geo, time) |>
    dplyr::summarise(values = agg_fun(values), .groups = "drop") |>
    dplyr::mutate(country = substr(geo, 1, 2))

  # NUTS-2 only, EU-27
  base_d <- readxl::read_xlsx(base_data_path) |>
    dplyr::filter(nchar(NUTS_ID) == 4) |>
    dplyr::pull(NUTS_ID)

  df <- raw |>
    dplyr::filter(nchar(geo) == 4, country %in% eu27, geo %in% base_d)

  pick <- pick_latest_complete_year(
    df, geo_dim = "geo", value_col = value_col,
    expected_geos = base_d, max_years_back = max_years_back
  )

  out <- df |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year) |>
    dplyr::transmute(Country_CD = country, NUTS_ID = geo,
                     Year = pick$year, Value = values)

  # Per-cell fallback (METHODOLOGY §4): a region missing/NA in the picked
  # year keeps its most recent earlier non-NA value within the window
  # (e.g. DEB2 unemployment, PL43). Previously implemented only in
  # create_employment_weights — closed here 2026-07-09.
  filled <- out |> dplyr::filter(!is.na(Value)) |> dplyr::pull(NUTS_ID)
  gaps <- setdiff(intersect(base_d, unique(df$geo)), filled)
  if (length(gaps) > 0) {
    fb <- df |>
      dplyr::filter(geo %in% gaps, !is.na(values)) |>
      dplyr::mutate(.year = as.integer(as.character(time))) |>
      dplyr::group_by(geo) |>
      dplyr::slice_max(.year, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(Country_CD = country, NUTS_ID = geo,
                       Year = .year, Value = values)
    out <- out |> dplyr::filter(!NUTS_ID %in% fb$NUTS_ID) |>
      dplyr::bind_rows(fb)
    attr(out, "cell_year_fallback") <- fb$NUTS_ID
  }

  attr(out, "year_selected") <- pick$year
  attr(out, "year_coverage") <- pick$coverage
  attr(out, "missing_geos")  <- pick$missing_geos
  attr(out, "source_dataset") <- id
  out
}


#' Create GFCF (Gross Fixed Capital Formation, NUTS-2, manufacturing) from
#' Eurostat `nama_10r_2gfcf`.
create_gfcf <- function(base_data_path) {

  out <- .fetch_nuts2_latest(
    id      = "nama_10r_2gfcf",
    filters = list(nace_r2 = "C", sector = "S1", currency = "MIO_EUR"),
    base_data_path = base_data_path,
    agg     = "sum"   # extensive (MIO_EUR): merged PT codes are summed
  )

  result <- out |>
    dplyr::transmute(
      Country_CD = Country_CD,
      Country_Name = NA_character_,
      NUTS_ID = NUTS_ID,
      NUTS_Name = NA_character_,
      Sector_CD = NA_character_,
      Sector_ID = NA_character_,
      Component = "Vulnerability",
      Dimension = "Finance",
      Variable  = "GFCF",
      Year      = Year,
      Source    = "Eurostat nama_10r_2gfcf",
      Unit      = "Million euro",
      Value     = round(Value, 2),
      Value_Norm = NA_real_
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- attr(out, "year_selected")
  attr(result, "source_dataset") <- attr(out, "source_dataset")
  result
}


#' Create Unemployment rate (NUTS-2) from Eurostat `lfst_r_lfu3rt`.
create_unemployment <- function(base_data_path) {

  out <- .fetch_nuts2_latest(
    id      = "lfst_r_lfu3rt",
    filters = list(isced11 = "TOTAL", sex = "T", age = "Y20-64"),
    base_data_path = base_data_path
  )

  result <- out |>
    dplyr::transmute(
      Country_CD = Country_CD,
      Country_Name = NA_character_,
      NUTS_ID = NUTS_ID,
      NUTS_Name = NA_character_,
      Sector_CD = NA_character_,
      Sector_ID = NA_character_,
      Component = "Vulnerability",
      Dimension = "Labor",
      Variable  = "Unemployment",
      Year      = Year,
      Source    = "Eurostat lfst_r_lfu3rt",
      Unit      = "Percentage",
      Value     = round(Value, 2),
      Value_Norm = NA_real_
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- attr(out, "year_selected")
  attr(result, "source_dataset") <- attr(out, "source_dataset")
  result
}


#' Create Labour Market Slack (NUTS-2) from Eurostat `lfst_r_sla_ga`.
create_labour_slack <- function(base_data_path) {

  out <- .fetch_nuts2_latest(
    id      = "lfst_r_sla_ga",
    filters = list(unit = "PC_ELF", sex = "T", wstatus = "SLACK",
                   age = "Y15-74"),
    base_data_path = base_data_path
  )

  result <- out |>
    dplyr::transmute(
      Country_CD = Country_CD,
      Country_Name = NA_character_,
      NUTS_ID = NUTS_ID,
      NUTS_Name = NA_character_,
      Sector_CD = NA_character_,
      Sector_ID = NA_character_,
      Component = "Vulnerability",
      Dimension = "Labor",
      Variable  = "Labour_Market_Slack",
      Year      = Year,
      Source    = "Eurostat lfst_r_sla_ga",
      Unit      = "Percentage",
      Value     = round(Value, 2),
      Value_Norm = NA_real_
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- attr(out, "year_selected")
  attr(result, "source_dataset") <- attr(out, "source_dataset")
  result
}


#' Highly_Skilled_Workers per NUTS-2 = HRST as % of active population
#' (Eurostat `hrst_st_rcat`, category=HRST, unit=PC_ACT, sex=T).
create_highly_skilled <- function(base_data_path) {

  out <- .fetch_nuts2_latest(
    id      = "hrst_st_rcat",
    filters = list(category = "HRST", unit = "PC_ACT", sex = "T"),
    base_data_path = base_data_path
  )

  result <- out |>
    dplyr::transmute(
      Country_CD = Country_CD,
      Country_Name = NA_character_,
      NUTS_ID = NUTS_ID,
      NUTS_Name = NA_character_,
      Sector_CD = NA_character_,
      Sector_ID = NA_character_,
      Component = "Vulnerability",
      Dimension = "Labor",
      Variable  = "Highly_Skilled_Workers",
      Year      = Year,
      Source    = "Eurostat hrst_st_rcat",
      Unit      = "Percentage",
      Value     = round(Value, 2),
      Value_Norm = NA_real_
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- attr(out, "year_selected")
  attr(result, "source_dataset") <- attr(out, "source_dataset")
  result
}


# Extensive sector indicators (downscaled to NUTS-2 by employment share).


#' Pull a Eurostat national x NACE table for EU-27, pick the latest year
#' with full (Country x NACE) coverage, return (Country_ID, nace_r2,
#' Year, Value) ready for downscaling.
.fetch_national_nace_latest <- function(id, filters, sector_codes,
                                        year_col = "time",
                                        value_col = "values",
                                        max_years_back = 5L) {

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = id,
    filters     = filters,
    date_filter = seq(this_yr - max_years_back, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo),
                  values = as.numeric(values),
                  nace_r2 = as.character(nace_r2))

  df <- raw |>
    dplyr::filter(geo %in% eu27,
                  nace_r2 %in% sector_codes,
                  !is.na(values))

  pick <- pick_latest_complete_year(
    df, geo_dim = "geo", sector_dim = "nace_r2", value_col = value_col,
    expected_geos = eu27, expected_sectors = sector_codes,
    max_years_back = max_years_back
  )

  out <- df |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year) |>
    dplyr::transmute(Country_ID = geo, nace_r2 = nace_r2,
                     Year = pick$year, Value = values)

  attr(out, "year_selected") <- pick$year
  attr(out, "year_coverage") <- pick$coverage
  attr(out, "missing_geos")  <- pick$missing_geos
  attr(out, "missing_sectors") <- pick$missing_sectors
  attr(out, "source_dataset") <- id
  out
}


#' Map detailed NACE (env_ac_ainah_r2 style: C, C10-C12, ..., C31_C32, C33)
#' to the canonical 11-group + C-total schema used by the pipeline.
.nace_to_canonical_11 <- function(nace_r2) {
  dplyr::case_when(
    nace_r2 == "C"        ~ "C",
    nace_r2 == "C10-C12"  ~ "C10-C12",
    nace_r2 == "C13-C15"  ~ "C13-C15",
    nace_r2 %in% c("C16","C17","C18") ~ "C16-C18",
    nace_r2 %in% c("C19","C20")       ~ "C19-C20",
    nace_r2 %in% c("C21","C22")       ~ "C21-C22",
    nace_r2 == "C23"                  ~ "C23",
    nace_r2 == "C24"                  ~ "C24",
    nace_r2 %in% c("C25","C28")       ~ "C25+C28",
    nace_r2 %in% c("C26","C27")       ~ "C26-C27",
    nace_r2 %in% c("C29","C30")       ~ "C29-C30",
    nace_r2 %in% c("C31_C32","C33")   ~ "C31-C33",
    TRUE                              ~ NA_character_
  )
}


#' Scope1_Emissions per NUTS-2 x sector = national GHG by NACE
#' (env_ac_ainah_r2, airpol=GHG, THS_T) aggregated to the 11 canonical
#' manufacturing groups and downscaled by employment share.
create_scope1 <- function(base_data_path, empl_weights) {

  nace_codes <- c("C","C10-C12","C13-C15","C16","C17","C18","C19","C20",
                  "C21","C22","C23","C24","C25","C26","C27","C28","C29","C30",
                  "C31_C32","C33")

  national <- .fetch_national_nace_latest(
    id = "env_ac_ainah_r2",
    filters = list(airpol = "GHG", unit = "THS_T", nace_r2 = nace_codes),
    sector_codes = nace_codes
  )

  # Aggregate to 11 canonical sectors (sum kt within each)
  national_agg <- national |>
    dplyr::mutate(Sector_ID = .nace_to_canonical_11(nace_r2)) |>
    dplyr::filter(!is.na(Sector_ID)) |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::summarise(Scope1_kt = sum(Value, na.rm = TRUE), .groups = "drop")

  regional <- downscale_national_to_nuts2(
    national_df      = national_agg,
    empl_weights = empl_weights,
    value_cols       = "Scope1_kt"
  )

  result <- regional |>
    dplyr::transmute(
      Country_ID = Country_ID,
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = "Scope1_Emissions",
      Unit       = "kt CO2eq",
      Value      = round(Scope1_kt, 4)
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- attr(national, "year_selected")
  attr(result, "source_dataset") <- "env_ac_ainah_r2"
  result
}


#' Energy_Consumption per NUTS-2 x sector = national industrial final
#' energy use (nrg_bal_c, FC_IND_* per sub-sector, siec=TOTAL, GWh)
#' downscaled by employment share. FC_IND_NSP_E is split equally across
#' C21-C22, C26-C27, C31-C33.
create_energy_consumption <- function(base_data_path, empl_weights) {

  ind_codes <- c(
    "FC_IND_FBT_E", "FC_IND_TL_E", "FC_IND_WP_E", "FC_IND_PPP_E",
    "FC_IND_CPC_E", "FC_IND_IS_E", "FC_IND_NFM_E", "FC_IND_NMM_E",
    "FC_IND_MAC_E", "FC_IND_TE_E", "FC_IND_NSP_E", "FC_IND_E"
  )

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = "nrg_bal_c",
    filters     = list(siec = "TOTAL", unit = "GWH", geo = eu27,
                       nrg_bal = ind_codes),
    date_filter = seq(this_yr - 5L, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values))

  # Pick latest year using FC_IND_E (industry total) as coverage check
  pick <- pick_latest_complete_year(
    raw |> dplyr::filter(nrg_bal == "FC_IND_E"),
    geo_dim = "geo", value_col = "values",
    expected_geos = eu27, max_years_back = 5L
  )

  ene <- raw |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year, geo %in% eu27,
                  nrg_bal %in% ind_codes) |>
    dplyr::transmute(Country_ID = geo, nrg_bal, Elec_GWh = values)

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
    "FC_IND_MAC_E",  "C25+C28",
    "FC_IND_TE_E",   "C29-C30",
    "FC_IND_E",      "C"
  )

  ene_mapped <- ene |>
    dplyr::inner_join(sector_map, by = "nrg_bal") |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::summarise(Energy_GWh = sum(Elec_GWh, na.rm = TRUE), .groups = "drop")

  # Split FC_IND_NSP_E equally across the unmapped sectors (same as create_scope2)
  nsp <- ene |>
    dplyr::filter(nrg_bal == "FC_IND_NSP_E") |>
    dplyr::select(Country_ID, NSP_GWh = Elec_GWh)
  unmapped <- c("C21-C22", "C26-C27", "C31-C33")
  nsp_split <- nsp |>
    tidyr::crossing(Sector_ID = unmapped) |>
    dplyr::mutate(Energy_GWh = NSP_GWh / length(unmapped)) |>
    dplyr::select(Country_ID, Sector_ID, Energy_GWh)

  national <- dplyr::bind_rows(ene_mapped, nsp_split)

  regional <- downscale_national_to_nuts2(
    national_df      = national,
    empl_weights = empl_weights,
    value_cols       = "Energy_GWh"
  )

  result <- regional |>
    dplyr::transmute(
      Country_ID = Country_ID,
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = "Energy_Consumption",
      Unit       = "GWh",
      Value      = round(Energy_GWh, 4)
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- pick$year
  attr(result, "source_dataset") <- "nrg_bal_c"
  result
}


#' Extra-EU Import and Export per NUTS-2 x sector = national trade by NACE
#' (ext_tec01, partner=EXT_EU, sizeclas=TOTAL, THS_EUR) aggregated to the 11
#' canonical groups and downscaled by employment share. Returns a list with
#' `$import` and `$export` tibbles.
create_trade_extra_eu <- function(base_data_path, empl_weights) {

  # ext_tec01 NACE codes: detailed manufacturing letters
  nace_codes <- c("C","C10","C11","C12","C13","C14","C15","C16","C17","C18",
                  "C19","C20","C21","C22","C23","C24","C25","C26","C27","C28",
                  "C29","C30","C31","C32","C33")

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = "ext_tec01",
    filters     = list(partner = "EXT_EU", unit = "THS_EUR",
                       sizeclas = "TOTAL", geo = eu27,
                       nace_r2 = nace_codes),
    date_filter = seq(this_yr - 5L, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values),
                  nace_r2 = as.character(nace_r2),
                  stk_flow = as.character(stk_flow))

  pick <- pick_latest_complete_year(
    raw |> dplyr::filter(nace_r2 == "C", stk_flow == "IMP"),
    geo_dim = "geo", value_col = "values",
    expected_geos = eu27, max_years_back = 5L
  )

  trade <- raw |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year)

  # Aggregate detailed NACE divisions → 11 canonical sectors
  trade_agg <- trade |>
    dplyr::left_join(sector_aggregation, by = c("nace_r2" = "nace_detail")) |>
    dplyr::filter(!is.na(Sector_ID)) |>
    dplyr::group_by(stk_flow, Country_ID = geo, Sector_ID) |>
    dplyr::summarise(MIO_EUR = sum(values, na.rm = TRUE) / 1000,
                     .groups = "drop")

  national_imp <- trade_agg |> dplyr::filter(stk_flow == "IMP") |>
    dplyr::transmute(Country_ID, Sector_ID, Import = MIO_EUR)
  national_exp <- trade_agg |> dplyr::filter(stk_flow == "EXP") |>
    dplyr::transmute(Country_ID, Sector_ID, Export = MIO_EUR)

  reg_imp <- downscale_national_to_nuts2(
    national_df = national_imp, empl_weights = empl_weights,
    value_cols  = "Import"
  ) |>
    dplyr::transmute(
      Country_ID, NUTS_ID, Sector_ID,
      Indicator = "Import_ExtraEU", Unit = "Million euro",
      Value = round(Import, 2)
    )

  reg_exp <- downscale_national_to_nuts2(
    national_df = national_exp, empl_weights = empl_weights,
    value_cols  = "Export"
  ) |>
    dplyr::transmute(
      Country_ID, NUTS_ID, Sector_ID,
      Indicator = "Export_ExtraEU", Unit = "Million euro",
      Value = round(Export, 2)
    )

  result <- list(import = reg_imp |> tibble::as_tibble(),
                 export = reg_exp |> tibble::as_tibble())

  attr(result, "year_selected") <- pick$year
  attr(result, "source_dataset") <- "ext_tec01"
  result
}


#' BERD per NUTS-2 x sector = national BERD by NACE (rd_e_berdindr2,
#' MIO_EUR) downscaled by employment share. Returned as absolute MIO_EUR;
#' the per-employee step happens in `normalize_indicators` (to_per_empl).
create_berd <- function(base_data_path, empl_weights) {

  nace_codes <- c("C","C10-C12","C13-C15","C16","C17","C18","C19","C20",
                  "C21","C22","C23","C24","C25","C26","C27","C28","C29","C30",
                  "C31_C32","C33")

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = "rd_e_berdindr2",
    filters     = list(nace_r2 = nace_codes, unit = "MIO_EUR", geo = eu27),
    date_filter = seq(this_yr - 5L, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values),
                  nace_r2 = as.character(nace_r2))

  pick <- pick_latest_complete_year(
    raw |> dplyr::filter(nace_r2 == "C"),
    geo_dim = "geo", value_col = "values",
    expected_geos = eu27, max_years_back = 5L
  )

  berd <- raw |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year)

  # Aggregate to 11 canonical sectors (some NACE need merging)
  berd_agg <- berd |>
    dplyr::mutate(Sector_ID = .nace_to_canonical_11(nace_r2)) |>
    dplyr::filter(!is.na(Sector_ID)) |>
    dplyr::group_by(Country_ID = geo, Sector_ID) |>
    dplyr::summarise(BERD_MEUR = sum(values, na.rm = TRUE), .groups = "drop")

  reg_berd <- downscale_national_to_nuts2(
    national_df  = berd_agg,
    empl_weights = empl_weights,
    value_cols   = "BERD_MEUR"
  )

  result <- reg_berd |>
    dplyr::transmute(
      Country_ID = Country_ID,
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = "BERD",
      Unit       = "Million euro",
      Value      = round(BERD_MEUR, 4)
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- pick$year
  attr(result, "source_dataset") <- "rd_e_berdindr2"
  result
}


#' Regional business R&D intensity = genuinely-regional R&D-capacity indicator
#' from `rd_e_gerdreg` (sectperf = BES, R&D as % of regional GDP, NUTS-2),
#' replicated across the manufacturing sectors. SUPERSEDES `create_berd()`.
#'
#' Why: the old `create_berd()` (national BERD-by-NACE downscaled by employment,
#' then divided by employment in `04_normalize.R`) reduces algebraically to a
#' country x sector constant — the downscale weight and the per-employee divisor
#' cancel — so it carried NO within-country regional variation. The regional-
#' resilience literature measures the innovation / adaptive-capacity channel with
#' NUTS-2 R&D and patents (Bristow & Healy 2018; Filippetti et al. 2020; Toth et
#' al. 2020; Rocchetta et al. 2021; Panori 2025); `rd_e_gerdreg` gives genuine
#' regional business-R&D intensity. See LITERATURE_GATHERED.md section H.
#'
#' Kept under the Indicator name "BERD" so the Technology dimension, the reversed
#' orientation (higher R&D -> lower vulnerability) and harmonisation are unchanged.
#' Because it is now an INTENSITY (% of GDP), "BERD" is removed from the
#' per-employee list in `R/04_normalize.R`.
#'
#' NB Eurostat codes (sectperf = "BES", unit = "PC_GDP") follow the standard R&D
#' classification; confirm on the first networked run (the sandbox used for
#' development had no Eurostat egress).
#'
#' @param empl_weights tibble (Country_ID, NUTS_ID, Sector_ID, weight) — supplies
#'   the (region x sector) grid the region-level value is replicated across.
#' @return tibble (Country_ID, NUTS_ID, Sector_ID, Indicator, Unit, Value)
create_regional_berd <- function(empl_weights) {

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = "rd_e_gerdreg",
    filters     = list(sectperf = "BES", unit = "PC_GDP"),
    date_filter = seq(this_yr - 6L, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values))

  # NUTS-2 geographies only (4-char codes within the EU-27).
  reg <- raw |>
    dplyr::filter(nchar(geo) == 4, substr(geo, 1, 2) %in% eu27)

  pick <- pick_latest_complete_year(
    reg, geo_dim = "geo", value_col = "values",
    expected_geos = sort(unique(empl_weights$NUTS_ID)), max_years_back = 6L
  )

  rd_region <- reg |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year) |>
    dplyr::transmute(NUTS_ID = geo, rd_pc_gdp = values)

  # Fallback chain for regions absent in the picked year (found 2026-07-03:
  # PT16-18/PL43/PL62 publish only earlier years; Belgium publishes BES PC_GDP
  # at NUTS-1 only; the Netherlands only nationally). BERD is an intensity
  # (% of GDP), so replicating a coarser geography is the same §5 rule used
  # for other intensive indicators (and mirrors the QoG NUTS-1 handling).
  #   (i)  per-cell latest non-NA year within the window (§4 fallback);
  #   (ii) NUTS-1 parent value, replicated;
  #   (iii) national value, replicated.
  expected <- sort(unique(empl_weights$NUTS_ID))
  latest_of <- function(df) df |>
    dplyr::filter(!is.na(values)) |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::group_by(geo) |>
    dplyr::slice_max(.year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  miss1 <- setdiff(expected, rd_region$NUTS_ID[!is.na(rd_region$rd_pc_gdp)])
  fb_cell <- latest_of(reg |> dplyr::filter(geo %in% miss1)) |>
    dplyr::transmute(NUTS_ID = geo, rd_pc_gdp = values)

  miss2 <- setdiff(miss1, fb_cell$NUTS_ID)
  n1 <- latest_of(raw |> dplyr::filter(nchar(geo) == 3,
                                       substr(geo, 1, 2) %in% eu27)) |>
    dplyr::transmute(n1 = geo, rd_pc_gdp = values)
  fb_n1 <- tibble::tibble(NUTS_ID = miss2, n1 = substr(miss2, 1, 3)) |>
    dplyr::inner_join(n1, by = "n1") |>
    dplyr::select(NUTS_ID, rd_pc_gdp)

  miss3 <- setdiff(miss2, fb_n1$NUTS_ID)
  n0 <- latest_of(raw |> dplyr::filter(nchar(geo) == 2, geo %in% eu27)) |>
    dplyr::transmute(n0 = geo, rd_pc_gdp = values)
  fb_n0 <- tibble::tibble(NUTS_ID = miss3, n0 = substr(miss3, 1, 2)) |>
    dplyr::inner_join(n0, by = "n0") |>
    dplyr::select(NUTS_ID, rd_pc_gdp)

  rd_region <- rd_region |>
    dplyr::filter(!is.na(rd_pc_gdp)) |>
    dplyr::bind_rows(fb_cell, fb_n1, fb_n0)

  # Replicate the region-level value across the (Country x NUTS x Sector) grid.
  result <- empl_weights |>
    dplyr::distinct(Country_ID, NUTS_ID, Sector_ID) |>
    dplyr::left_join(rd_region, by = "NUTS_ID") |>
    dplyr::transmute(
      Country_ID, NUTS_ID, Sector_ID,
      Indicator = "BERD",
      Unit      = "R&D, % of regional GDP",
      Value     = round(rd_pc_gdp, 4)
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected")  <- pick$year
  attr(result, "source_dataset") <- "rd_e_gerdreg (sectperf=BES, PC_GDP)"
  attr(result, "fallbacks") <- list(cell_year = fb_cell$NUTS_ID,
                                    nuts1 = fb_n1$NUTS_ID, nuts0 = fb_n0$NUTS_ID)
  result
}


# Intensive sector indicators (national ratios/indices replicated to NUTS-2).


#' Renewables_Share and Fossil_Share per NUTS-2 x sector from `nrg_bal_c`:
#'   Renewables_Share = siec=RA000 / siec=TOTAL
#'   Fossil_Share     = siec=FE    / siec=TOTAL
#' Computed at country x NACE level and replicated to all NUTS-2 of the
#' country (intensive, scale-invariant).
create_energy_shares <- function(base_data_path) {

  ind_codes <- c(
    "FC_IND_FBT_E", "FC_IND_TL_E", "FC_IND_WP_E", "FC_IND_PPP_E",
    "FC_IND_CPC_E", "FC_IND_IS_E", "FC_IND_NFM_E", "FC_IND_NMM_E",
    "FC_IND_MAC_E", "FC_IND_TE_E", "FC_IND_NSP_E", "FC_IND_E"
  )
  all_siec <- c("TOTAL", "RA000", "FE")

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = "nrg_bal_c",
    filters     = list(siec = all_siec, unit = "GWH", geo = eu27,
                       nrg_bal = ind_codes),
    date_filter = seq(this_yr - 5L, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values),
                  siec = as.character(siec),
                  nrg_bal = as.character(nrg_bal))

  pick <- pick_latest_complete_year(
    raw |> dplyr::filter(nrg_bal == "FC_IND_E", siec == "TOTAL"),
    geo_dim = "geo", value_col = "values",
    expected_geos = eu27, max_years_back = 5L
  )

  ene <- raw |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year)

  # Map nrg_bal → canonical sector (same map as create_energy_consumption)
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
    "FC_IND_MAC_E",  "C25+C28",
    "FC_IND_TE_E",   "C29-C30",
    "FC_IND_E",      "C"
  )

  ene_sec <- ene |>
    dplyr::inner_join(sector_map, by = "nrg_bal") |>
    dplyr::group_by(Country_ID = geo, Sector_ID, siec) |>
    dplyr::summarise(Energy_GWh = sum(values, na.rm = TRUE), .groups = "drop")

  # NSP split (same logic as create_energy_consumption)
  nsp <- ene |>
    dplyr::filter(nrg_bal == "FC_IND_NSP_E") |>
    dplyr::group_by(Country_ID = geo, siec) |>
    dplyr::summarise(NSP_GWh = sum(values, na.rm = TRUE), .groups = "drop")
  unmapped <- c("C21-C22", "C26-C27", "C31-C33")
  nsp_split <- nsp |>
    tidyr::crossing(Sector_ID = unmapped) |>
    dplyr::mutate(Energy_GWh = NSP_GWh / length(unmapped)) |>
    dplyr::select(Country_ID, Sector_ID, siec, Energy_GWh)

  ene_all <- dplyr::bind_rows(ene_sec, nsp_split)

  # Pivot wider on siec to compute ratios
  shares_nat <- ene_all |>
    tidyr::pivot_wider(names_from = siec, values_from = Energy_GWh,
                       values_fill = 0) |>
    dplyr::mutate(
      Renewables_Share = dplyr::if_else(TOTAL > 0, RA000 / TOTAL * 100, NA_real_),
      Fossil_Share     = dplyr::if_else(TOTAL > 0, FE / TOTAL * 100, NA_real_)
    ) |>
    dplyr::select(Country_ID, Sector_ID, Renewables_Share, Fossil_Share)

  # Replicate to all NUTS-2 of country
  regional <- replicate_national_to_nuts2(
    national_df    = shares_nat,
    base_data_path = base_data_path,
    value_cols     = c("Renewables_Share", "Fossil_Share")
  )

  result <- regional |>
    tidyr::pivot_longer(c(Renewables_Share, Fossil_Share),
                        names_to = "Indicator", values_to = "Value") |>
    dplyr::transmute(
      NUTS_ID    = NUTS_ID,
      Sector_ID  = Sector_ID,
      Indicator  = Indicator,
      Value      = round(Value, 4),
      Unit       = "Percentage"
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- pick$year
  attr(result, "source_dataset") <- "nrg_bal_c"
  result
}


#' Capital_Stock_Based_Prod per NUTS-2 = national index from
#' `nama_10_cp_a21` (NCS_HW, N11N, I20 = net fixed assets per hour worked,
#' Index 2020=100) replicated to every NUTS-2 of the country.
create_capital_stock_prod <- function(base_data_path) {

  this_yr <- as.integer(format(Sys.Date(), "%Y"))
  raw <- restatapi::get_eurostat_data(
    id          = "nama_10_cp_a21",
    filters     = list(nace_r2 = "C", na_item = "NCS_HW",
                       asset10 = "N11N", unit = "I20", geo = eu27),
    date_filter = seq(this_yr - 5L, this_yr),
    exact_match = TRUE, label = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(geo = as.character(geo), values = as.numeric(values))

  pick <- pick_latest_complete_year(
    raw, geo_dim = "geo", value_col = "values",
    expected_geos = eu27, max_years_back = 5L
  )

  national <- raw |>
    dplyr::mutate(.year = as.integer(as.character(time))) |>
    dplyr::filter(.year == pick$year) |>
    dplyr::transmute(Country_ID = geo, Capital_Stock_Prod = values)

  regional <- replicate_national_to_nuts2(
    national_df    = national,
    base_data_path = base_data_path,
    value_cols     = "Capital_Stock_Prod"
  )

  result <- regional |>
    dplyr::transmute(
      Country_CD = Country_ID,
      Country_Name = NA_character_,
      NUTS_ID    = NUTS_ID,
      NUTS_Name  = NA_character_,
      Sector_CD  = NA_character_,
      Sector_ID  = NA_character_,
      Component  = "Vulnerability",
      Dimension  = "Finance",
      Variable   = "Capital_Stock_Based_Prod",
      Year       = pick$year,
      Source     = "Eurostat nama_10_cp_a21",
      Unit       = "Index (2020=100)",
      Value      = round(Capital_Stock_Prod, 4),
      Value_Norm = NA_real_
    ) |>
    tibble::as_tibble()

  attr(result, "year_selected") <- pick$year
  attr(result, "source_dataset") <- "nama_10_cp_a21"
  result
}
