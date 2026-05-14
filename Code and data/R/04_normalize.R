# 04_normalize.R — per-employee intensities, winsorisation, min-max scaling.


#' Filter excluded NUTS, divide intensive indicators by employment, min-max
#' normalise to [0.01, 0.99], and reverse negative-direction indicators.
#'
#' @param data_long    Tibble from `reshape_to_grid()`.
#' @param empl_weights Either a path to `EMPL_Region.xlsx` or the in-memory
#'   `empl_weights` tibble (Country_ID, NUTS_ID, Sector_ID, pers_employed, weight).
#' @return List with `$long` (long normalised tibble) and `$wide`
#'   (one column per indicator using Value_N).
normalize_indicators <- function(data_long, empl_weights) {

  # Drop ultraperipheral / obsolete NUTS-2 regions and the helper indicator.
  data_ready <- data_long |>
    filter(!NUTS_ID %in% excluded_nuts) |>
    filter(Indicator != "Share_of_Employment")

  # Accept either xlsx path or tibble for empl_weights.
  if (is.character(empl_weights)) {
    empl_data <- read_xlsx(empl_weights) |> filter(nchar(NUTS_ID) == 4)
  } else {
    empl_data <- empl_weights |> filter(nchar(NUTS_ID) == 4)
  }

  # NUTS-2013 -> NUTS-2021 recombinations for employment data (mirroring the
  # reshape step in 03_reshape.R).
  hr04_empl <- empl_data |>
    filter(NUTS_ID %in% c("HR02", "HR05", "HR06")) |>
    group_by(Country_ID, Sector_ID) |>
    summarise(pers_employed = sum(pers_employed, na.rm = TRUE),
              weight        = sum(weight, na.rm = TRUE),
              .groups = "drop") |>
    mutate(NUTS_ID = "HR04")

  nl_remap <- empl_data |>
    filter(NUTS_ID %in% c("NL35", "NL36")) |>
    mutate(NUTS_ID = case_when(
      NUTS_ID == "NL35" ~ "NL31",
      NUTS_ID == "NL36" ~ "NL33",
      TRUE              ~ NUTS_ID
    ))

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

  obsolete_nuts <- c("HR02", "HR05", "HR06", "NL35", "NL36",
                     "PT19", "PT1A", "PT1B", "PT1C", "PT1D")
  empl_data <- empl_data |>
    filter(!NUTS_ID %in% obsolete_nuts) |>
    bind_rows(hr04_empl, nl_remap, pt_remap)

  # Per-employee normalisation for GFCF and BERD only — the other extensive
  # indicators are already downscaled by employment shares upstream.
  to_per_empl <- c("Gross_Fixed_Capital_Formation", "BERD")

  data_ready <- data_ready |>
    select(-any_of(c("n_enterprises", "pers_employed"))) |>
    left_join(
      empl_data |> distinct(NUTS_ID, Sector_ID, pers_employed),
      by = c("NUTS_ID", "Sector_ID")
    ) |>
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

  # Winsorise GFCF at p99 within (Indicator x Sector) to cap the
  # Ireland multinational profit-shifting outlier. GFCF is no longer in
  # the composite (see METHODOLOGY §11) but the cap keeps the reference
  # tables from being dominated by Ireland's MNC accounting. BERD is not
  # winsorised: top BERD regions are bona fide R&D hubs (Stuttgart,
  # Munich, Rhône-Alpes, Düsseldorf), no Ireland inflation.
  data_ready <- data_ready |>
    group_by(Indicator, Sector_ID) |>
    mutate(
      Value = if_else(
        Indicator == "Gross_Fixed_Capital_Formation",
        winsorize_upper(Value, p = 0.99),
        Value
      )
    ) |>
    ungroup()

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
          is.na(Value)                                 ~ NA_real_,
          max_val - min_val == 0                       ~ 0.5,
          Indicator == "Scope1_Emissions" & Value == 0 ~ 0.00,
          Value == min_val                             ~ 0.01,
          Value == max_val                             ~ 0.99,
          TRUE                                         ~ 0.01 + norm0_1 * 0.98
        )
      ) |>
      ungroup()
  }

  pp_norm    <- .compute_value_n(pp_data,    c("Indicator"))
  other_norm <- .compute_value_n(other_data, c("Indicator", "Sector_ID"))

  # Reverse "negative" indicators (higher raw -> lower vulnerability).
  data_long_norm <- bind_rows(pp_norm, other_norm) |>
    mutate(
      Value_N = if_else(Indicator %in% positive_indicators, Value_N, 1 - Value_N),
      Value_N = round(Value_N, 3)
    ) |>
    relocate(Value_N, .after = Value) |>
    select(-min_val, -max_val, -norm0_1)

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
