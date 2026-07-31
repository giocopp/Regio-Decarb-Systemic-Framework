# 04_normalize.R — per-employee intensities, winsorisation, min-max scaling.


#' Filter excluded NUTS, divide intensive indicators by employment, min-max
#' normalise to [0.01, 0.99], and reverse negative-direction indicators.
#'
#' @param data_long    Tibble from `reshape_to_grid()`.
#' @param empl_weights Either a path to `EMPL_Region.xlsx` or the in-memory
#'   `empl_weights` tibble (Country_ID, NUTS_ID, Sector_ID, pers_employed, weight).
#' @param pool If TRUE, min-max scale non-Policy indicators across the whole
#'   panel (group by Indicator only) instead of within (Indicator x Sector_ID).
#'   Used by the pooled Vulnerability of the headline TRI (R/exposure.R).
#'   Default FALSE preserves the within-sector baseline.
#' @return List with `$long` (long normalised tibble) and `$wide`
#'   (one column per indicator using Value_N).
normalize_indicators <- function(data_long, empl_weights, pool = FALSE) {

  # Drop ultraperipheral / obsolete NUTS-2 regions and the helper indicator.
  data_ready <- data_long |>
    dplyr::filter(!NUTS_ID %in% excluded_nuts) |>
    dplyr::filter(Indicator != "Share_of_Employment")

  # Accept either xlsx path or tibble for empl_weights.
  if (is.character(empl_weights)) {
    empl_data <- readxl::read_xlsx(empl_weights) |> dplyr::filter(nchar(NUTS_ID) == 4)
  } else {
    empl_data <- empl_weights |> dplyr::filter(nchar(NUTS_ID) == 4)
  }

  # NUTS-2013 -> NUTS-2021 recombinations for employment data (mirroring the
  # reshape step in 03_reshape.R). Shared helper in utils.R.
  empl_data <- recombine_empl_nuts(empl_data)

  # Per-employee division for GFCF only: the other extensive indicators are
  # downscaled by employment shares upstream, and BERD is already an
  # intensity (% of regional GDP).
  to_per_empl <- c("Gross_Fixed_Capital_Formation")

  # Pooled (headline) path only: Energy_Consumption becomes GWh per employee
  # — the downscaled volume is collinear with employment (pure region size).
  # The legacy within-sector baseline (pool = FALSE) keeps the volume (§7).
  if (pool) to_per_empl <- c(to_per_empl, "Energy_Consumption")

  data_ready <- data_ready |>
    dplyr::select(-dplyr::any_of(c("n_enterprises", "pers_employed"))) |>
    dplyr::left_join(
      empl_data |> dplyr::distinct(NUTS_ID, Sector_ID, pers_employed),
      by = c("NUTS_ID", "Sector_ID")
    ) |>
    dplyr::mutate(
      Value = dplyr::if_else(
        !is.na(pers_employed) & pers_employed == 0,
        NA_real_,
        Value
      )
    ) |>
    dplyr::mutate(
      Value = dplyr::if_else(
        Indicator %in% to_per_empl & !is.na(pers_employed) & pers_employed > 0,
        Value / pers_employed,
        Value
      ),
      # A per-employee indicator with no employment must be NA, not the
      # undivided volume — mixing scales would fake regional variation
      # (affected cells later take the country x sector median intensity).
      Value = dplyr::if_else(
        Indicator %in% to_per_empl &
          (is.na(pers_employed) | pers_employed <= 0),
        NA_real_,
        Value
      ),
      Notes = dplyr::if_else(
        Indicator %in% to_per_empl,
        dplyr::if_else(is.na(Notes) | Notes == "",
                "per employee",
                paste(Notes, "per employee", sep = "; ")),
        Notes
      )
    )

  # Winsorise GFCF (only) at p99 within (Indicator x Sector) to cap the
  # Ireland multinational profit-shifting outlier (§11). BERD is
  # deliberately NOT winsorised: its top regions are bona fide R&D hubs.
  data_ready <- data_ready |>
    dplyr::group_by(Indicator, Sector_ID) |>
    dplyr::mutate(
      Value = dplyr::if_else(
        Indicator == "Gross_Fixed_Capital_Formation",
        winsorize_upper(Value, p = 0.99),
        Value
      )
    ) |>
    dplyr::ungroup()

  # Min-max scaling to [0.01, 0.99]. Most indicators are scaled within
  # (Indicator, Sector_ID); Policy_Pressure is a sector-level constant so it
  # is scaled across the whole panel.
  positive_indicators <- c(
    "Scope1_Emissions", "Scope2_Emissions", "Scope3_Emissions", "Policy_Pressure",
    "Energy_Consumption", "Fossil_Share",
    "Unemployment_Rate", "Labour_Market_Slack",
    "Export_ExtraEU", "Import_ExtraEU",
    "Sector_Concentration"
  )

  pp_data    <- data_ready |> dplyr::filter(Indicator == "Policy_Pressure")
  other_data <- data_ready |> dplyr::filter(Indicator != "Policy_Pressure")

  .compute_value_n <- function(df, group_vars) {
    df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::mutate(
        min_val = min(Value, na.rm = TRUE),
        max_val = max(Value, na.rm = TRUE),
        norm0_1 = dplyr::if_else(
          max_val - min_val == 0,
          0.5,
          (Value - min_val) / (max_val - min_val)
        ),
        Value_N = dplyr::case_when(
          is.na(Value)                                 ~ NA_real_,
          # true-zero rule precedes the constant rule (all-zero group -> 0)
          Indicator == "Scope1_Emissions" & Value == 0 ~ 0.00,
          max_val - min_val == 0                       ~ 0.5,
          Value == min_val                             ~ 0.01,
          Value == max_val                             ~ 0.99,
          TRUE                                         ~ 0.01 + norm0_1 * 0.98
        )
      ) |>
      dplyr::ungroup()
  }

  pp_norm    <- .compute_value_n(pp_data,    c("Indicator"))
  other_norm <- .compute_value_n(
    other_data,
    if (pool) c("Indicator") else c("Indicator", "Sector_ID"))

  # Reverse "negative" indicators (higher raw -> lower vulnerability).
  data_long_norm <- dplyr::bind_rows(pp_norm, other_norm) |>
    dplyr::mutate(
      Value_N = dplyr::if_else(Indicator %in% positive_indicators, Value_N, 1 - Value_N),
      Value_N = round(Value_N, 3)
    ) |>
    dplyr::relocate(Value_N, .after = Value) |>
    dplyr::select(-min_val, -max_val, -norm0_1)

  data_wide_norm <- data_long_norm |>
    dplyr::select(-c(Value, Component, Dimension, Unit, Notes)) |>
    dplyr::group_by(Country_ID, NUTS_ID, NUTS_Name,
             Sector_ID, Sector_Name,
             pers_employed, Indicator) |>
    dplyr::summarise(Value_N = mean(Value_N, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Indicator, values_from = Value_N)

  list(
    long = tibble::as_tibble(data_long_norm),
    wide = tibble::as_tibble(data_wide_norm)
  )
}
