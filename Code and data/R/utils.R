# utils.R — shared constants and helpers used across the pipeline.

#' Min-max rescale to [0.01, 0.99], preserving true zeros
#'
#' @param x Numeric vector
#' @return Numeric vector scaled to [0.01, 0.99]; zeros stay at 0
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.50, length(x)))
  scaled <- 0.01 + 0.98 * (x - rng[1]) / diff(rng)
  scaled[x == 0] <- 0
  scaled
}

#' Winsorize at upper percentile (cap outliers)
#'
#' @param x Numeric vector
#' @param p Upper percentile threshold (default 0.95)
#' @return Numeric vector with values above the p-th percentile capped
winsorize_upper <- function(x, p = 0.95) {
  cap <- quantile(x, probs = p, na.rm = TRUE)
  pmin(x, cap)
}

#' Impute NA values with country x sector median
#'
#' @param df Data frame with Country_ID and Sector_ID columns
#' @param col Character: column name to impute
#' @return Data frame with NAs filled
impute_with_median <- function(df, col) {
  df |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::mutate(
      .donor = median(.data[[col]], na.rm = TRUE),
      !!rlang::sym(col) := dplyr::if_else(
        is.na(.data[[col]]), .donor, .data[[col]]
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".donor")
}

#' EU-27 country codes (2-letter ISO)
eu27 <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)

#' ISO 3-letter to 2-letter mapping (for QoG Environmental data)
iso3_to_iso2 <- tibble::tribble(
  ~iso3, ~iso2,
  "AUT", "AT", "BEL", "BE", "BGR", "BG", "HRV", "HR", "CYP", "CY",
  "CZE", "CZ", "DNK", "DK", "EST", "EE", "FIN", "FI", "FRA", "FR",
  "DEU", "DE", "GRC", "EL", "HUN", "HU", "IRL", "IE", "ITA", "IT",
  "LVA", "LV", "LTU", "LT", "LUX", "LU", "MLT", "MT", "NLD", "NL",
  "POL", "PL", "PRT", "PT", "ROU", "RO", "SVK", "SK", "SVN", "SI",
  "ESP", "ES", "SWE", "SE"
)

#' EU-27 country full names (keyed by Eurostat 2-letter codes; EL=Greece)
country_names <- c(
  AT = "Austria",  BE = "Belgium",  BG = "Bulgaria", HR = "Croatia",
  CY = "Cyprus",   CZ = "Czechia",  DK = "Denmark",  EE = "Estonia",
  FI = "Finland",  FR = "France",   DE = "Germany",  EL = "Greece",
  HU = "Hungary",  IE = "Ireland",  IT = "Italy",    LV = "Latvia",
  LT = "Lithuania",LU = "Luxembourg", MT = "Malta",  NL = "Netherlands",
  PL = "Poland",   PT = "Portugal", RO = "Romania",  SK = "Slovakia",
  SI = "Slovenia", ES = "Spain",    SE = "Sweden"
)

#' Sector name lookup
sector_name_map <- c(
  "C"           = "Total Manufacturing",
  "C10-C12"     = "Manufacturing of Food, Beverage and Tobacco Products",
  "C13-C15"     = "Manufacturing of Textiles, Leather and Wearing Products",
  "C16-C18"     = "Manufacturing of Wood, Paper and Printing Products",
  "C19-C20"     = "Manufacturing of Chemical and Petrolchemical",
  "C21-C22"     = "Manufacturing of Pharmaceutical and Plastic Products",
  "C23"         = "Manufacturing of Non Metallic Mineral Products",
  "C24"         = "Manufacturing of Basic Metal Products",
  "C26-C27"     = "Manufacturing of Electronic and Electrical Products",
  "C25+C28"     = "Manufacturing of Fabricated Metal Products and Machinery",
  "C29-C30"     = "Manufacturing of Motor Vehicles and Transport Equipment",
  "C31-C33"     = "Other Manufacturing and Repairing"
)

#' Sector aggregation mapping (detailed NACE -> 11 groups)
sector_aggregation <- tibble::tribble(
  ~nace_detail, ~Sector_ID,
  "C",    "C",
  "C10",  "C10-C12", "C11", "C10-C12", "C12", "C10-C12",
  "C13",  "C13-C15", "C14", "C13-C15", "C15", "C13-C15",
  "C16",  "C16-C18", "C17", "C16-C18", "C18", "C16-C18",
  "C19",  "C19-C20", "C20", "C19-C20",
  "C21",  "C21-C22", "C22", "C21-C22",
  "C23",  "C23",
  "C24",  "C24",
  "C25",  "C25+C28", "C28", "C25+C28",
  "C29",  "C29-C30", "C30", "C29-C30",
  "C26",  "C26-C27", "C27", "C26-C27",
  "C31",  "C31-C33", "C32", "C31-C33", "C33", "C31-C33"
)

#' NUTS-2 regions excluded from the analysis (EU ultraperipheral).
excluded_nuts <- c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5",
                    "ES63", "ES64", "PT20", "PT30", "FI20")

#' Aggregation rules for NUTS recombination (sum vs mean)
agg_rules <- tibble::tribble(
  ~Indicator,                        ~agg_fun,
  "Scope1_Emissions",                   "sum",
  "Scope2_Emissions",                "sum",
  "Scope3_Emissions",                "sum",
  "Energy_Consumption",              "sum",
  "Fossil_Share",                    "mean",
  "Renewables_Share",                "mean",
  "Capital_Stock_Based_Prod",        "mean",
  "Gross_Fixed_Capital_Formation",   "sum",
  "Cohesion_Fund",                   "mean",
  "Highly_Skilled_Workers",          "mean",
  "Labour_Market_Slack",             "mean",
  "Unemployment_Rate",               "mean",
  "Export_ExtraEU",                  "sum",
  "Import_ExtraEU",                  "sum",
  "BERD",                            "sum",
  "Regional_Innovation",             "mean",
  "Policy_Pressure",                 "mean",
  "QoG_Index",                       "mean",
  "Climate_Mitigation_Laws",         "mean",
  "Sector_Concentration",            "mean",
  "RE_Potential",                    "sum",
  "Share_of_Employment",             "mean",
  "Intangible_Investments",          "sum",
  "Tangible_Investments",            "sum"
)

#' Validate that all file paths exist; stop with informative error if not
#'
#' @param paths Character vector of file paths
#' @return paths (invisibly), or stops with an error listing missing files
validate_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Missing input files:\n",
         paste("  -", missing, collapse = "\n"),
         call. = FALSE)
  }
  paths
}


# Helpers used by 01_create_data.R for the scripted Eurostat pipeline.


#' Latest year with EU-27 coverage >= min_coverage in a Eurostat slice.
#' Walks the year column from most recent to oldest (up to max_years_back);
#' returns the year that first clears the coverage threshold. If none does,
#' returns the year with the highest observed coverage.
#'
#' @return list(year, coverage, missing_geos, missing_sectors).
pick_latest_complete_year <- function(df,
                                      geo_dim,
                                      sector_dim = NULL,
                                      year_col = "time",
                                      value_col = "values",
                                      expected_geos = eu27,
                                      expected_sectors = NULL,
                                      max_years_back = 5L,
                                      min_coverage = 0.95) {

  d <- df |>
    dplyr::mutate(.year = as.integer(as.character(.data[[year_col]])),
                  .value = as.numeric(.data[[value_col]]))

  years <- sort(unique(d$.year), decreasing = TRUE)
  years <- head(years, max_years_back)

  # Walk years from latest to earliest, collect coverage stats per year.
  stats <- list()
  for (yr in years) {

    slice <- d |> dplyr::filter(.year == yr, !is.na(.value))
    have_geos <- unique(slice[[geo_dim]])
    miss_g <- setdiff(expected_geos, have_geos)

    if (!is.null(sector_dim) && !is.null(expected_sectors)) {
      have_cells <- slice |>
        dplyr::distinct(.data[[geo_dim]], .data[[sector_dim]])
      expected <- tidyr::expand_grid(g = expected_geos, s = expected_sectors)
      names(expected) <- c(geo_dim, sector_dim)
      have_set <- paste(have_cells[[geo_dim]], have_cells[[sector_dim]], sep = "|")
      exp_set  <- paste(expected[[geo_dim]],  expected[[sector_dim]],  sep = "|")
      miss_cells <- setdiff(exp_set, have_set)
      miss_s <- unique(sub(".*\\|", "", miss_cells))
      cov <- 1 - length(miss_cells) / nrow(expected)
    } else {
      miss_s <- character(0)
      cov <- 1 - length(miss_g) / length(expected_geos)
    }

    stats[[length(stats) + 1]] <- list(year = as.integer(yr), coverage = cov,
                                        missing_geos = miss_g,
                                        missing_sectors = miss_s)
  }

  # Latest year above the coverage threshold wins (recency over completeness).
  acceptable <- Filter(function(s) s$coverage >= min_coverage, stats)
  if (length(acceptable) > 0) return(acceptable[[1]])

  # Fallback: no year reaches the threshold; report the best one observed.
  best_idx <- which.max(vapply(stats, function(s) s$coverage, numeric(1)))
  if (length(best_idx) == 0) {
    return(list(year = NA_integer_, coverage = -1,
                missing_geos = expected_geos,
                missing_sectors = expected_sectors))
  }
  stats[[best_idx]]
}


#' Employment-share downscaler for extensive national indicators.
#' Multiplies each national (Country_ID, Sector_ID) value by the region's
#' share of national manufacturing employment in that sector (the `weight`
#' column of `empl_weights`). Falls back to the Sector-C weight when a
#' specific sector weight is missing.
#'
#' Takes the in-memory `empl_weights` tibble, NOT the EMPL_Region.xlsx
#' file (which holds within-region shares that would collapse Sector C).
#'
#' @param national_df Tibble with Country_ID, Sector_ID, and value columns.
#' @param empl_weights Tibble (Country_ID, NUTS_ID, Sector_ID, weight).
#' @param value_cols Character vector of column names to downscale.
#' @return Tibble keyed by Country_ID x NUTS_ID x Sector_ID.
downscale_national_to_nuts2 <- function(national_df,
                                        empl_weights,
                                        value_cols) {

  required <- c("Country_ID", "NUTS_ID", "Sector_ID", "weight")
  missing <- setdiff(required, names(empl_weights))
  if (length(missing) > 0) {
    stop("downscale_national_to_nuts2: empl_weights is missing columns: ",
         paste(missing, collapse = ", "),
         ". Pass the empl_weights tibble, not the EMPL_Region.xlsx path.",
         call. = FALSE)
  }

  weights <- empl_weights |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, weight)

  # Sector-C weight as fallback when a specific sector weight is missing.
  fallback <- weights |>
    dplyr::filter(Sector_ID == "C") |>
    dplyr::select(Country_ID, NUTS_ID, weight_fallback = weight)

  # One row per NUTS-2 region per (Country_ID, Sector_ID) cell.
  countries <- unique(national_df$Country_ID)
  region_grid <- weights |>
    dplyr::filter(Country_ID %in% countries, Sector_ID == "C") |>
    dplyr::select(Country_ID, NUTS_ID)

  out <- national_df |>
    dplyr::inner_join(region_grid, by = "Country_ID",
                      relationship = "many-to-many") |>
    dplyr::left_join(weights, by = c("Country_ID", "NUTS_ID", "Sector_ID")) |>
    dplyr::left_join(fallback, by = c("Country_ID", "NUTS_ID")) |>
    dplyr::mutate(weight = dplyr::coalesce(weight, weight_fallback))

  for (vc in value_cols) {
    out[[vc]] <- out[[vc]] * out$weight
  }

  out |>
    dplyr::filter(!is.na(weight)) |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(value_cols), ~ !is.na(.))) |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, dplyr::all_of(value_cols))
}


#' Uniform replicator for intensive national indicators (ratios, indices,
#' percentages). Each NUTS-2 of a country receives the same national value.
replicate_national_to_nuts2 <- function(national_df,
                                        base_data_path,
                                        value_cols) {

  nuts2 <- readxl::read_xlsx(base_data_path) |>
    dplyr::filter(nchar(NUTS_ID) == 4) |>
    dplyr::transmute(Country_ID = substr(NUTS_ID, 1, 2), NUTS_ID)

  key_cols <- "Country_ID"
  if ("Sector_ID" %in% names(national_df)) key_cols <- c(key_cols, "Sector_ID")

  nuts2 |>
    dplyr::inner_join(national_df, by = "Country_ID", relationship = "many-to-many") |>
    dplyr::select(dplyr::any_of(c("Country_ID", "NUTS_ID", "Sector_ID")),
                  dplyr::all_of(value_cols))
}


#' Write a tibble to xlsx and return the path (for `tar_target` file targets).
write_indicator_xlsx <- function(df, out_path) {
  writexl::write_xlsx(df, out_path)
  out_path
}


#' QA helper: compare a new scripted xlsx against a legacy file, cell by cell.
#' Not wired into `tar_make()`; intended for interactive sanity checks.
#' Returns a per-column diff summary and warns on tolerance breaches.
compare_xlsx_to_legacy <- function(new_path, legacy_path, key_cols, value_cols,
                                   rtol = 0.05, atol = 0.01) {

  new_df <- readxl::read_xlsx(new_path) |> tibble::as_tibble()
  leg_df <- readxl::read_xlsx(legacy_path) |> tibble::as_tibble()

  rows_new_only <- dplyr::anti_join(new_df, leg_df, by = key_cols) |> nrow()
  rows_leg_only <- dplyr::anti_join(leg_df, new_df, by = key_cols) |> nrow()

  joined <- dplyr::inner_join(new_df, leg_df, by = key_cols,
                              suffix = c(".new", ".leg"))

  out <- purrr::map_df(value_cols, function(vc) {
    new_col <- paste0(vc, ".new"); leg_col <- paste0(vc, ".leg")
    if (!new_col %in% names(joined) || !leg_col %in% names(joined)) {
      return(tibble::tibble(value_col = vc, n_compared = 0L,
                            max_abs_diff = NA_real_, max_rel_diff = NA_real_,
                            within_tol = NA))
    }
    diffs <- abs(joined[[new_col]] - joined[[leg_col]])
    rel_diffs <- diffs / pmax(abs(joined[[leg_col]]), 1e-12)
    tibble::tibble(
      value_col   = vc,
      n_compared  = sum(!is.na(diffs)),
      max_abs_diff = max(diffs, na.rm = TRUE),
      max_rel_diff = max(rel_diffs, na.rm = TRUE),
      within_tol  = max(diffs, na.rm = TRUE) <= atol |
                    max(rel_diffs, na.rm = TRUE) <= rtol
    )
  })

  message("compare_xlsx_to_legacy: new=", new_path, "  legacy=", legacy_path)
  message("  rows only in new:    ", rows_new_only)
  message("  rows only in legacy: ", rows_leg_only)
  print(out)

  if (any(!out$within_tol & !is.na(out$within_tol))) {
    warning("Some value columns exceed tolerance (rtol=", rtol,
            ", atol=", atol, ")", call. = FALSE)
  }

  invisible(out)
}
