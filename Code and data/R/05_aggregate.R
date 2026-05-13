# ── 05_aggregate.R ── Risk aggregation pipeline step ───────────────
# Computes Exposure, Vulnerability (7 dimensions), and TRI
# Input:  normalised wide tibble (from 04_normalise)
# Output: tibble with Exposure, Vuln_*, Vulnerability, Risk_norm, Risk_Band


#' Aggregate normalised indicators into the Transition Risk Index
#'
#' @param norm_wide Tibble with NUTS_ID, Country_ID, Sector_ID, Sector_Name,
#'   and normalised indicator columns
#' @return Tibble with all original columns plus Exposure, Vuln_* dimension
#'   scores, Vulnerability, Risk_norm, and Risk_Band
aggregate_risk <- function(norm_wide) {

  # ── 1. Define indicator groups ──────────────────────────────────
  exposure_vars <- c("GHG_Emissions", "Scope2_Emissions",
                     "Scope3_Emissions", "Policy_Pressure")

  dimensions <- list(
    Energy          = c("Energy_Consumption", "Fossil_Share",
                        "Renewables_Share", "RE_Potential"),
    Labour          = c("Unemployment_Rate", "Labour_Market_Slack",
                        "Highly_Skilled_Workers"),
    Finance         = c("Gross_Fixed_Capital_Formation", "Cohesion_Fund"),
    Supply_Chain    = c("Import_ExtraEU"),
    Technology      = c("BERD", "Regional_Innovation"),
    Institutions    = c("QoG_Index", "Climate_Mitigation_Laws"),
    Diversification = c("HHI_Employment")
  )

  # ── 2. Gracefully drop missing indicators ───────────────────────
  available <- names(norm_wide)
  missing <- setdiff(c(exposure_vars, unlist(dimensions)), available)
  if (length(missing) > 0) {
    message("aggregate_risk: missing indicators (ignored): ",
            paste(missing, collapse = ", "))
  }


  exposure_vars <- intersect(exposure_vars, available)
  stopifnot("No exposure variables found in data" = length(exposure_vars) > 0)

  dimensions <- purrr::map(dimensions, \(vars) intersect(vars, available))
  empty_dims <- purrr::map_lgl(dimensions, \(v) length(v) == 0)
  if (any(empty_dims)) {
    message("  Dropping empty dimensions: ",
            paste(names(dimensions)[empty_dims], collapse = ", "))
    dimensions <- dimensions[!empty_dims]
  }

  # ── 3. Impute NAs with country x sector median ─────────────────
  all_vars <- c(exposure_vars, unlist(dimensions))
  df <- norm_wide
  for (v in all_vars) {
    df <- impute_with_median(df, v)
  }

  # ── 4. Composite Exposure (rowMeans, then range01 by sector) ────
  df <- df |>
    dplyr::mutate(
      Exposure = rowMeans(dplyr::pick(dplyr::all_of(exposure_vars)), na.rm = TRUE)
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Exposure = range01(Exposure)) |>
    dplyr::ungroup()

  # ── 5. Dimension scores: Vuln_<dim> ────────────────────────────
  for (dim_name in names(dimensions)) {
    col_name <- paste0("Vuln_", dim_name)
    dim_vars <- dimensions[[dim_name]]
    df <- df |>
      dplyr::mutate(
        !!col_name := rowMeans(dplyr::pick(dplyr::all_of(dim_vars)), na.rm = TRUE)
      )
  }

  # ── 6. Normalise each Vuln dimension by sector ─────────────────
  df <- df |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Vuln_"), range01)) |>
    dplyr::ungroup()

  # ── 7. Vulnerability = mean of all Vuln_* columns, range01 ─────
  df <- df |>
    dplyr::mutate(
      Vulnerability = rowMeans(
        dplyr::pick(dplyr::starts_with("Vuln_")), na.rm = TRUE
      )
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Vulnerability = range01(Vulnerability)) |>
    dplyr::ungroup()

  # ── 8. TRI = Exposure^0.5 x Vulnerability^0.5, range01 ────────
  alpha <- 0.50
  df <- df |>
    dplyr::mutate(
      Exposure  = dplyr::if_else(Exposure == 0, NA_real_, Exposure),
      Risk_raw  = Exposure^alpha * Vulnerability^(1 - alpha)
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Risk_norm = range01(Risk_raw)) |>
    dplyr::ungroup() |>
    dplyr::select(-Risk_raw)

  # ── 9. Risk bands (quintile-style 0.2 breaks) ──────────────────
  band_labels <- c("Very Low", "Low", "Medium", "High", "Very High")
  df <- df |>
    dplyr::mutate(
      Risk_Band = cut(
        Risk_norm,
        breaks        = seq(0, 1, by = 0.20),
        include.lowest = TRUE,
        labels        = band_labels
      ),
      Risk_Band = dplyr::case_when(
        is.na(Risk_norm) ~ "Zero Risk",
        TRUE             ~ as.character(Risk_Band)
      )
    )

  # ── 10. Return ─────────────────────────────────────────────────
  dplyr::as_tibble(df)
}
