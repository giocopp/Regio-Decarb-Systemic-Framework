# ── 04_normalize.R ── Per-employee adjustment & min-max normalization ─────────
#
# Exported function:
#   normalize_indicators()
#
# Packages loaded via tar_option_set() in _targets.R

# ── normalize_indicators() ──────────────────────────────────────────────────

#' Filter excluded NUTS, divide intensive indicators by employment, min-max
#' normalise to [0.01, 0.99], and reverse negative-direction indicators.
#'
#' @param data_long    Tibble from reshape_to_grid().
#' @param empl_weights Path to Regional_Employment_Weights.xlsx **or** a tibble
#'                     with columns NUTS_ID, Sector_ID, pers_employed.
#' @return Named list:
#'   \describe{
#'     \item{$long}{Long normalised tibble (one row per region x sector x indicator)}
#'     \item{$wide}{Wide normalised tibble (one column per indicator, using Value_N)}
#'   }
normalize_indicators <- function(data_long, empl_weights) {

  # ── 1. Filter excluded NUTS regions & drop Share_of_Employment ──
  data_ready <- data_long |>
    filter(!NUTS_ID %in% excluded_nuts) |>
    filter(Indicator != "Share_of_Employment")

  # ── 2. Prepare employment weights ──
  if (is.character(empl_weights)) {
    empl_data <- read_xlsx(empl_weights) |>
      filter(nchar(NUTS_ID) == 4)
  } else {
    empl_data <- empl_weights |>
      filter(nchar(NUTS_ID) == 4)
  }

  # Handle NUTS recombinations in employment data

  # Croatia: HR04 = HR02 + HR05 + HR06
  hr04_empl <- empl_data |>
    filter(NUTS_ID %in% c("HR02", "HR05", "HR06")) |>
    group_by(Country_ID, Sector_ID) |>
    summarise(pers_employed = sum(pers_employed, na.rm = TRUE),
              weight        = sum(weight, na.rm = TRUE),
              .groups = "drop") |>
    mutate(NUTS_ID = "HR04")

  # Netherlands: NL35 -> NL31, NL36 -> NL33
  nl_remap <- empl_data |>
    filter(NUTS_ID %in% c("NL35", "NL36")) |>
    mutate(NUTS_ID = case_when(
      NUTS_ID == "NL35" ~ "NL31",
      NUTS_ID == "NL36" ~ "NL33",
      TRUE              ~ NUTS_ID
    ))

  # Portugal: PT19+PT1D -> PT16, PT1A+PT1B -> PT17, PT1C -> PT18
  pt_remap <- empl_data |>
    filter(NUTS_ID %in% c("PT19", "PT1A", "PT1B", "PT1C", "PT1D")) |>
    mutate(target = case_when(
      NUTS_ID %in% c("PT19", "PT1D") ~ "PT16",
      NUTS_ID %in% c("PT1A", "PT1B") ~ "PT17",
      NUTS_ID == "PT1C"              ~ "PT18",
      TRUE                           ~ NUTS_ID
    )) |>
    group_by(Country_ID, Sector_ID, target) |>
    summarise(pers_employed = sum(pers_employed, na.rm = TRUE),
              weight        = sum(weight, na.rm = TRUE),
              .groups = "drop") |>
    rename(NUTS_ID = target)

  # Remove obsolete NUTS codes before binding aggregated replacements
  obsolete_nuts <- c("HR02", "HR05", "HR06", "NL35", "NL36",
                     "PT19", "PT1A", "PT1B", "PT1C", "PT1D")
  empl_data <- empl_data |>
    filter(!NUTS_ID %in% obsolete_nuts) |>
    bind_rows(hr04_empl, nl_remap, pt_remap)

  # ── 3. Join employment counts & divide intensive indicators ──
  # Note: GHG_Emissions, Scope2_Emissions, Scope3_Emissions, and
  # Energy_Consumption are already downscaled to regions via employment
  # weights in the Create scripts. Dividing again by pers_employed would
  # double-count and inflate small regions. Only divide indicators that
  # are NOT already employment-weighted.
  to_per_empl <- c("Gross_Fixed_Capital_Formation", "BERD")

  data_ready <- data_ready |>
    select(-any_of(c("n_enterprises", "pers_employed"))) |>
    left_join(
      empl_data |> distinct(NUTS_ID, Sector_ID, pers_employed),
      by = c("NUTS_ID", "Sector_ID")
    ) |>
    # Set values to NA for region-sectors with zero employment
    # (sector doesn't meaningfully exist there)
    mutate(
      Value = if_else(
        !is.na(pers_employed) & pers_employed == 0,
        NA_real_,
        Value
      )
    ) |>
    mutate(
      Value = if_else(
        Indicator %in% to_per_empl & !is.na(pers_employed) & pers_employed > 0,
        Value / pers_employed,
        Value
      ),
      Notes = if_else(
        Indicator %in% to_per_empl,
        if_else(is.na(Notes) | Notes == "",
                "per employee",
                paste(Notes, "per employee", sep = "; ")),
        Notes
      )
    )

  # ── 3b. Winsorize GFCF at 95th percentile (by sector) ──
  # Caps the Ireland outlier (multinational profit-shifting inflates GFCF).
  # Following OECD/JRC Handbook on Constructing Composite Indicators (2008).
  data_ready <- data_ready |>
    group_by(Sector_ID) |>
    mutate(
      Value = if_else(
        Indicator == "Gross_Fixed_Capital_Formation",
        winsorize_upper(Value, p = 0.95),
        Value
      )
    ) |>
    ungroup()

  # ── 4. Min-max normalise to [0.01, 0.99] ──────────────────────
  # Most indicators are normalised WITHIN (Indicator, Sector_ID) so each
  # sector's regional ranking is comparable. Policy_Pressure is a
  # sector-level constant (every NUTS-2 region in sector S has the same
  # raw value), so within-sector grouping collapses min == max and
  # destroys the signal. We therefore normalise Policy_Pressure across
  # all (Sector x NUTS-2) values together (group_by Indicator only).
  # NA values propagate as NA (case_when's first branch).
  positive_indicators <- c(
    "GHG_Emissions", "Scope2_Emissions", "Scope3_Emissions", "Policy_Pressure",
    "Energy_Consumption", "Fossil_Share",
    "Unemployment_Rate", "Labour_Market_Slack",
    "Export_ExtraEU", "Import_ExtraEU",
    "HHI_Employment"
  )

  pp_data    <- data_ready |> filter(Indicator == "Policy_Pressure")
  other_data <- data_ready |> filter(Indicator != "Policy_Pressure")

  .compute_value_n <- function(df, group_vars) {
    df |>
      group_by(across(all_of(group_vars))) |>
      mutate(
        min_val = min(Value, na.rm = TRUE),
        max_val = max(Value, na.rm = TRUE),
        norm0_1 = if_else(
          max_val - min_val == 0,
          0.5,
          (Value - min_val) / (max_val - min_val)
        ),
        Value_N = case_when(
          is.na(Value)                              ~ NA_real_,
          max_val - min_val == 0                    ~ 0.5,
          Indicator == "GHG_Emissions" & Value == 0 ~ 0.00,
          Value == min_val                          ~ 0.01,
          Value == max_val                          ~ 0.99,
          TRUE                                      ~ 0.01 + norm0_1 * 0.98
        )
      ) |>
      ungroup()
  }

  pp_norm    <- .compute_value_n(pp_data,    c("Indicator"))
  other_norm <- .compute_value_n(other_data, c("Indicator", "Sector_ID"))

  data_long_norm <- bind_rows(pp_norm, other_norm) |>
    # 5. Reverse negative indicators (higher raw value = lower vulnerability)
    mutate(
      Value_N = if_else(Indicator %in% positive_indicators, Value_N, 1 - Value_N),
      Value_N = round(Value_N, 3)
    ) |>
    relocate(Value_N, .after = Value) |>
    select(-min_val, -max_val, -norm0_1)

  # ── 6. Build wide version ──
  data_wide_norm <- data_long_norm |>
    select(-c(Value, Component, Dimension, Unit, Notes)) |>
    group_by(Country_ID, NUTS_ID, NUTS_Name,
             Sector_ID, Sector_Name,
             pers_employed, Indicator) |>
    summarise(Value_N = mean(Value_N, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = Indicator, values_from = Value_N)

  list(
    long = tibble::as_tibble(data_long_norm),
    wide = tibble::as_tibble(data_wide_norm)
  )
}
