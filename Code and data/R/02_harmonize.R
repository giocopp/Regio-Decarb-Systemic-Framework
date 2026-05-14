# 02_harmonize.R — read per-indicator xlsx and combine into one long tibble.

# ── helpers (internal) ────────────────────────────────────────────────────────

#' Standardise dimension strings to canonical names
.harmonize_dimension <- function(x) {

  x <- str_trim(tolower(x))
  dplyr::case_when(
    x %in% c("tech", "technology")                         ~ "Technology",
    x %in% c("labor", "labour")                            ~ "Labor",
    x %in% c("supply chain", "supplychain", "supply")      ~ "Supply Chain",
    x == "finance"                                          ~ "Finance",
    x %in% c("institutions", "institution", "inst")        ~ "Institutions",
    x == "energy"                                           ~ "Energy",
    x %in% c("diversification", "divers")                  ~ "Diversification",
    TRUE                                                    ~ x
  )
}

#' Split legacy "C25+C28-C30" rows into "C25+C28" and "C29-C30".
#' Defensive: kept for back-compatibility with older xlsx vintages — the
#' current pipeline emits the post-split schema directly so this is usually
#' a no-op. Sum-rule indicators use the regional share; mean-rule indicators
#' are duplicated.
.split_C25_C28_C30 <- function(sector_data, empl_weights) {

  to_split <- sector_data |>
    dplyr::filter(Sector_ID == "C25+C28-C30")
  if (nrow(to_split) == 0) return(sector_data)

  ew_split <- empl_weights |>
    dplyr::filter(Sector_ID %in% c("C25+C28", "C29-C30")) |>
    dplyr::select(NUTS_ID, Sector_ID, pers_employed) |>
    tidyr::pivot_wider(names_from = Sector_ID,
                       values_from = pers_employed,
                       values_fill = 0) |>
    dplyr::rename(emp_C25C28 = `C25+C28`,
                  emp_C29C30 = `C29-C30`) |>
    dplyr::mutate(
      total = emp_C25C28 + emp_C29C30,
      share_C25C28 = dplyr::if_else(total > 0, emp_C25C28 / total, 0.5),
      share_C29C30 = dplyr::if_else(total > 0, emp_C29C30 / total, 0.5)
    ) |>
    dplyr::select(NUTS_ID, share_C25C28, share_C29C30)

  to_split <- to_split |>
    dplyr::left_join(agg_rules, by = "Indicator") |>
    dplyr::mutate(agg_fun = dplyr::coalesce(agg_fun, "mean")) |>
    dplyr::left_join(ew_split, by = "NUTS_ID") |>
    dplyr::mutate(
      share_C25C28 = dplyr::coalesce(share_C25C28, 0.5),
      share_C29C30 = dplyr::coalesce(share_C29C30, 0.5)
    )

  new_C25 <- to_split |>
    dplyr::mutate(
      Value     = dplyr::if_else(agg_fun == "sum",
                                  Value * share_C25C28, Value),
      Sector_ID = "C25+C28",
      Sector_Name = sector_name_map["C25+C28"]
    )

  new_C29 <- to_split |>
    dplyr::mutate(
      Value     = dplyr::if_else(agg_fun == "sum",
                                  Value * share_C29C30, Value),
      Sector_ID = "C29-C30",
      Sector_Name = sector_name_map["C29-C30"]
    )

  drop_cols <- c("agg_fun", "share_C25C28", "share_C29C30")
  new_C25 <- new_C25 |> dplyr::select(-dplyr::any_of(drop_cols))
  new_C29 <- new_C29 |> dplyr::select(-dplyr::any_of(drop_cols))

  sector_data |>
    dplyr::filter(Sector_ID != "C25+C28-C30") |>
    dplyr::bind_rows(new_C25, new_C29)
}


#' Pick the first column whose name matches a regex, or return NA
.pick_col <- function(df, pattern) {
  hits <- names(df)[grepl(pattern, names(df), ignore.case = TRUE)]
  if (length(hits) == 0) return(rep(NA_character_, nrow(df)))
  # Prefer the column that has fewer NAs
  na_counts <- vapply(hits, function(h) sum(is.na(df[[h]])), integer(1))
  df[[hits[which.min(na_counts)]]]
}


# ── 1.  harmonize_non_sector() ───────────────────────────────────────────────

#' Read & harmonise non-sector xlsx files into one long tibble.
#'
#' @param file_paths Character vector of full paths to non-sector xlsx files.
#' @param base_data_path Path to base_data_plus.xlsx (official NUTS-2 region list).
#' @return Tibble with columns:
#'   Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name,
#'   Component, Dimension, Indicator, Unit, Value, Value_N
harmonize_non_sector <- function(file_paths, base_data_path) {

  # ── read base region list (NUTS-2 only) ──
  base_ids <- read_excel(base_data_path) |>
    clean_names() |>
    filter(nchar(nuts_id) == 4) |>
    select(cntr_code, nuts_id, nuts_name) |>
    distinct() |>
    rename(Country_ID = cntr_code,
           NUTS_ID    = nuts_id,
           NUTS_Name  = nuts_name)

  # ── per-file reader ──
  read_one <- function(file_path) {

    # Check for metadata header ("Dimension: ...; Indicator: ...")
    title_cell    <- read_excel(file_path, range = "A1", col_names = FALSE)[[1, 1]]
    has_metadata  <- grepl("Dimension:", title_cell)

    data <- if (has_metadata) read_excel(file_path, skip = 1) else read_excel(file_path)
    data <- clean_names(data)

    # -- extract Dimension & Indicator --
    if (has_metadata) {
      dimension <- str_extract(title_cell, "(?<=Dimension: )[^;]+") |> str_trim()
      indicator <- str_extract(title_cell, "(?<=Indicator: )[^;]+") |> str_trim()
    } else {
      parts     <- strsplit(tools::file_path_sans_ext(basename(file_path)), "-")[[1]]
      dimension <- toupper(parts[1])
      indicator <- parts[2]
    }
    dimension <- .harmonize_dimension(dimension)

    # -- standardise column names --
    # NUTS_ID
    if (!any(grepl("nuts.*id", names(data))) && "region_code" %in% names(data)) {
      data <- rename(data, nuts_id = region_code)
    } else {
      cand <- names(data)[grepl("nuts.*id", names(data))]
      if (length(cand) > 0) data <- rename(data, nuts_id = all_of(cand[1]))
      else                       data <- mutate(data, nuts_id = NA_character_)
    }

    # NUTS_Name
    nm_cand <- names(data)[grepl("nuts.*name", names(data))]
    if (length(nm_cand) > 0) data <- rename(data, nuts_name = all_of(nm_cand[1]))
    else                         data <- mutate(data, nuts_name = NA_character_)

    # Country_ID
    if ("country_cd" %in% names(data)) {
      data <- rename(data, country_id = country_cd)
    } else if (!"country_id" %in% names(data)) {
      data <- mutate(data, country_id = if_else(!is.na(nuts_id),
                                                 substr(nuts_id, 1, 2),
                                                 NA_character_))
    }

    # Value
    if (!"value" %in% names(data)) {
      if ("gfcf" %in% names(data)) data <- rename(data, value = gfcf)
      else                              data <- mutate(data, value = NA_real_)
    }

    # Unit
    if (!"unit" %in% names(data)) data <- mutate(data, unit = NA_character_)

    # -- assemble output --
    data |>
      mutate(
        sector_id   = NA_character_,
        sector_name = NA_character_,
        component   = "Vulnerability",
        dimension   = dimension,
        indicator   = indicator,
        value_n     = NA_real_
      ) |>
      select(country_id, nuts_id, nuts_name, sector_id, sector_name,
             component, dimension, indicator, unit, value, value_n) |>
      set_names(c("Country_ID", "NUTS_ID", "NUTS_Name", "Sector_ID", "Sector_Name",
                   "Component", "Dimension", "Indicator", "Unit", "Value", "Value_N"))
  }

  # ── process all files ──
  raw <- purrr::map(file_paths, read_one) |> bind_rows()

  # ── harmonise regions against base list ──
  harmonised <- raw |>
    select(-Country_ID, -NUTS_Name) |>
    left_join(base_ids, by = "NUTS_ID") |>
    select(Country_ID, NUTS_ID, NUTS_Name, everything())

  # ── complete grid: every region x every indicator ──
  indicators_info <- harmonised |>
    select(Indicator, Dimension, Component, Unit) |>
    distinct()

  out <- crossing(base_ids, indicators_info) |>
    left_join(harmonised,
              by = c("Country_ID", "NUTS_ID", "NUTS_Name",
                     "Indicator", "Dimension", "Component", "Unit")) |>
    select(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name,
           Component, Dimension, Indicator, Unit, Value, Value_N)

  # ── final clean-up: rename indicators, fix units ──
  out <- out |>
    mutate(
      Indicator = if_else(Indicator == "0 Summary Innovation Index",
                          "Regional_Innovation", Indicator)
    ) |>
    mutate(Unit = case_when(
      tolower(Unit) == "percentage"              ~ "Percentage",
      Indicator == "GFCF"                        ~ "Million euro",
      Indicator == "Capital_Stock_Based_Prod"    ~ "Index",
      Indicator == "Regional_Innovation"         ~ "Index",
      Indicator == "Unemployment"                ~ "Percentage",
      TRUE                                       ~ Unit
    )) |>
    filter(!(Dimension == "Technology" & is.na(Indicator))) |>
    mutate(Indicator = case_when(
      Indicator == "Unemployment" ~ "Unemployment_rate",
      Indicator == "GFCF"        ~ "Gross_Fixed_Capital_Formation",
      Indicator == "RE_Potential" ~ "RE_Potential",
      Indicator == "QoG"         ~ "QoG_Index",
      Indicator == "Climate_Laws"~ "Climate_Mitigation_Laws",
      TRUE                       ~ Indicator
    ))

  tibble::as_tibble(out)
}


# ── 2.  harmonize_sector() ──────────────────────────────────────────────────

#' Read & harmonise sector xlsx files into one long tibble.
#'
#' @param file_paths Character vector of full paths to sector xlsx files.
#' @param non_sector_data Tibble produced by harmonize_non_sector() (used for
#'   region lookup and NUTS_Name fill).
#' @param empl_weights Tibble from create_employment_weights() — used by
#'   .split_C25_C28_C30() to allocate legacy aggregate values to the two
#'   new C25+C28 and C29-C30 sectors.
#' @return Tibble with the same 11 columns as non_sector_data.
harmonize_sector <- function(file_paths, non_sector_data, empl_weights) {

  read_one <- function(file_path) {

    data <- read_excel(file_path) |> clean_names()

    # -- handle value column --
    if (!"value" %in% names(data)) {
      if (all(c("tang_inv", "intang_inv") %in% names(data))) {
        data <- data |>
          pivot_longer(cols = c(tang_inv, intang_inv),
                       names_to = "value_type", values_to = "value") |>
          mutate(value_n = NA_real_)
        if (!"indicator" %in% names(data)) {
          data <- mutate(data, indicator = value_type)
        }
      } else {
        data <- mutate(data, value = NA_real_, value_n = NA_real_)
      }
    } else {
      if (!"value_n" %in% names(data)) data <- mutate(data, value_n = NA_real_)
    }

    # -- build standardised columns --
    out <- tibble(
      Country_ID = .pick_col(data, "^country_cd$|^country_id$|^cntr_code$"),
      NUTS_ID    = .pick_col(data, "^nuts_id$|^region_code$"),
      NUTS_Name  = if ("nuts_name" %in% names(data)) data$nuts_name
                   else rep(NA_character_, nrow(data)),
      Sector_ID  = .pick_col(data, "^sector_id$|^sector_cd$"),
      Sector_Name = if ("sector_name" %in% names(data)) data$sector_name
                    else rep(NA_character_, nrow(data)),
      Component  = if ("component"  %in% names(data)) data$component
                   else rep(NA_character_, nrow(data)),
      Dimension  = if ("dimension"  %in% names(data)) data$dimension
                   else rep(NA_character_, nrow(data)),
      Indicator  = .pick_col(data, "^indicator$|^variable$"),
      Unit       = if ("unit" %in% names(data)) data$unit
                   else rep(NA_character_, nrow(data)),
      Value      = data$value,
      Value_N    = data$value_n
    )

    # Derive Indicator from filename if entirely missing
    if (all(is.na(out$Indicator))) {
      fname <- tools::file_path_sans_ext(basename(file_path))
      parts <- strsplit(fname, "-")[[1]]
      out$Indicator <- if (length(parts) >= 2) parts[2] else fname
    }

    out
  }

  sector_data <- purrr::map(file_paths, read_one) |> bind_rows()

  # ── fill NUTS_Name from non-sector region mapping ──
  region_mapping <- non_sector_data |>
    select(Country_ID, NUTS_ID, NUTS_Name) |>
    distinct()

  sector_data <- sector_data |>
    left_join(region_mapping, by = c("Country_ID", "NUTS_ID"),
              suffix = c("", ".ns")) |>
    mutate(NUTS_Name = coalesce(NUTS_Name, NUTS_Name.ns)) |>
    select(-NUTS_Name.ns)

  # ── collapse C31-C32 + C33 into C31-C33 ──
  sector_data <- sector_data |>
    mutate(Sector_ID = if_else(Sector_ID %in% c("C31-C32", "C33"),
                               "C31-C33", Sector_ID)) |>
    group_by(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name,
             Component, Dimension, Indicator, Unit) |>
    summarise(Value   = sum(Value, na.rm = TRUE),
              Value_N = NA_real_,
              .groups = "drop")

  # ── map sector names ──
  sector_data <- sector_data |>
    mutate(Sector_Name = sector_name_map[Sector_ID])

  # ── rename indicators ──
  sector_data <- sector_data |>
    mutate(Indicator = case_when(
      Indicator == "intang_inv"        ~ "Intangible_Investments",
      Indicator == "tang_inv"          ~ "Tangible_Investments",
      Indicator == "Emissions"         ~ "Scope1_Emissions",
      Indicator == "Import"            ~ "Import_ExtraEU",
      Indicator == "Export"            ~ "Export_ExtraEU",
      Indicator == "Scope2_Emissions"  ~ "Scope2_Emissions",
      Indicator == "Scope3_Emissions"  ~ "Scope3_Emissions",
      Indicator == "Policy_Pressure"   ~ "Policy_Pressure",
      TRUE                             ~ Indicator
    ))

  # ── set Component & Dimension ──
  sector_data <- sector_data |>
    mutate(
      Component = if_else(
        Indicator %in% c("Scope1_Emissions", "Scope2_Emissions",
                         "Scope3_Emissions", "Policy_Pressure"),
        "Exposure", "Vulnerability"
      ),
      Dimension = case_when(
        Indicator == "Share_of_Employment"                          ~ "Labor",
        Indicator %in% c("Tangible_Investments",
                         "Intangible_Investments")                 ~ "Finance",
        Indicator %in% c("Scope1_Emissions", "Scope2_Emissions",
                         "Scope3_Emissions", "Policy_Pressure")    ~ "Exposure",
        Indicator %in% c("Import_ExtraEU", "Export_ExtraEU")       ~ "Supply_Chain",
        Indicator == "BERD"                                        ~ "Technology",
        Indicator == "Sector_Concentration"                        ~ "Diversification",
        Indicator %in% c("Energy_Consumption", "Fossil_Share",
                         "Renewables_Share")                       ~ "Energy",
        TRUE                                                       ~ NA_character_
      )
    )

  # ── split legacy C25+C28-C30 rows into C25+C28 and C29-C30 ──
  sector_data <- .split_C25_C28_C30(sector_data, empl_weights)

  tibble::as_tibble(sector_data)
}


# ── 3.  combine_all() ───────────────────────────────────────────────────────

#' Bind sector and non-sector data into a single long tibble.
#'
#' @param sector_data     Tibble from harmonize_sector().
#' @param non_sector_data Tibble from harmonize_non_sector().
#' @return Combined long tibble (Country_ID filled, indicator names harmonised).
combine_all <- function(sector_data, non_sector_data) {

  # Drop sectors A and H (Agriculture, Transport) from sector data
  sector_data <- sector_data |>
    filter(!is.na(Sector_ID), !Sector_ID %in% c("A", "H"))

  all_data <- bind_rows(
    sector_data     |> select(-Value_N),
    non_sector_data |> select(-Value_N)
  )

  # Fill Country_ID from NUTS_ID where missing
  all_data <- all_data |>
    mutate(Country_ID = if_else(
      is.na(Country_ID) & !is.na(NUTS_ID),
      substr(NUTS_ID, 1, 2),
      Country_ID
    ))

  # Fill NUTS_Name within each NUTS_ID group
  all_data <- all_data |>
    group_by(NUTS_ID) |>
    fill(NUTS_Name, .direction = "downup") |>
    ungroup()

  # Set NUTS_ID / NUTS_Name to NA when they equal the country code
  all_data <- all_data |>
    mutate(
      NUTS_ID   = if_else(NUTS_ID == Country_ID, NA_character_, NUTS_ID),
      NUTS_Name = if_else(NUTS_Name == NUTS_ID,  NA_character_, NUTS_Name)
    )

  # Harmonise indicator & dimension names
  all_data <- all_data |>
    mutate(
      Indicator = case_when(
        Indicator == "Energy consumption"  ~ "Energy_Consumption",
        Indicator == "Unemployment_rate"   ~ "Unemployment_Rate",
        TRUE                               ~ Indicator
      ),
      Dimension = case_when(
        Indicator == "Energy_Consumption"                             ~ "Energy",
        Indicator %in% c("HHI_Employment", "RE_Potential")           ~ "Diversification",
        Indicator %in% c("QoG_Index", "Climate_Mitigation_Laws")     ~ "Institutions",
        Indicator %in% c("Scope2_Emissions", "Scope3_Emissions",
                         "Policy_Pressure")                          ~ "Exposure",
        TRUE                                                          ~ Dimension
      )
    )

  tibble::as_tibble(all_data)
}
