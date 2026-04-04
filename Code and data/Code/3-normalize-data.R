library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(writexl)

###

data_ready <- read_excel("Code and data/Derived data/2_All_data_long_READY.xlsx") |>
  filter(!NUTS_ID %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "ES63", "ES64", "PT20", "PT30", "FI20", "CY00")) |>
  filter(Indicator != "Share_of_Employment")

# ── Per-employee normalization (replaces per-enterprise) ─────────
# Read employment data (absolute employee counts by region x sector)
empl_data <- read_xlsx("Code and data/Initial data/Regional_Employment_Weights.xlsx") |>
  filter(nchar(NUTS_ID) == 4)  # NUTS-2 only

# ── Handle recombined NUTS codes (HR, NL, PT) ───────────────────
# HR04 = HR02 + HR05 + HR06 (step 2 aggregates these, but empl_data doesn't have HR04)
# NL31 from NL35, NL33 from NL36 (already mapped in step 2)
hr04_empl <- empl_data %>%
  filter(NUTS_ID %in% c("HR02", "HR05", "HR06")) %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(pers_employed = sum(pers_employed, na.rm = TRUE),
            weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(NUTS_ID = "HR04")

nl_remap <- empl_data %>%
  filter(NUTS_ID %in% c("NL35", "NL36")) %>%
  mutate(NUTS_ID = case_when(NUTS_ID == "NL35" ~ "NL31",
                              NUTS_ID == "NL36" ~ "NL33",
                              TRUE ~ NUTS_ID))

# PT recombinations (PT19+PT1D->PT16, PT1A+PT1B->PT17, PT1C->PT18)
pt_remap <- empl_data %>%
  filter(NUTS_ID %in% c("PT19", "PT1A", "PT1B", "PT1C", "PT1D")) %>%
  mutate(target = case_when(NUTS_ID %in% c("PT19", "PT1D") ~ "PT16",
                             NUTS_ID %in% c("PT1A", "PT1B") ~ "PT17",
                             NUTS_ID == "PT1C" ~ "PT18",
                             TRUE ~ NUTS_ID)) %>%
  group_by(Country_ID, Sector_ID, target) %>%
  summarise(pers_employed = sum(pers_employed, na.rm = TRUE),
            weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  rename(NUTS_ID = target)

empl_data <- bind_rows(empl_data, hr04_empl, nl_remap, pt_remap) %>%
  # Remove duplicates (keep the recombined rows over originals for affected codes)
  group_by(NUTS_ID, Sector_ID) %>%
  slice_max(pers_employed, n = 1, with_ties = FALSE) %>%
  ungroup()

# Drop any prior denominator columns to avoid .x/.y suffixing
data_ready <- data_ready %>%
  select(-any_of(c("n_enterprises", "pers_employed")))

# Join employment counts
data_ready <- data_ready %>%
  left_join(
    empl_data %>% distinct(NUTS_ID, Sector_ID, pers_employed),
    by = c("NUTS_ID", "Sector_ID")
  )

# Indicators to convert to "per employee"
to_per_empl <- c("Energy_Consumption", "GHG_Emissions", "Scope2_Emissions",
                  "Gross_Fixed_Capital_Formation", "BERD")

data_ready <- data_ready %>%
  mutate(
    Value = if_else(Indicator %in% to_per_empl & !is.na(pers_employed) & pers_employed > 0,
                    Value / pers_employed, Value),
    Notes = if_else(Indicator %in% to_per_empl,
                    if_else(is.na(Notes) | Notes == "", "per employee",
                            paste(Notes, "per employee", sep = "; ")),
                    Notes))

# ── Convert to wide format ───────────────────────────────────────
data_wide <- data_ready %>%
  select(-c(Component, Dimension, Unit, Notes)) %>%
  group_by(Country_ID, NUTS_ID, NUTS_Name,
           Sector_ID,  Sector_Name, Indicator) %>%
  summarise(
    Value = mean(Value, na.rm = TRUE),
    pers_employed = dplyr::first(pers_employed),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from  = Indicator,
    values_from = Value,
    id_cols = c(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name, pers_employed)
  )

# ── Min-max normalization to [0.01, 0.99] ────────────────────────
#
# Positive relationships: higher value --> higher vulnerability/exposure
# Negative relationships: higher value --> lower vulnerability (reversed)

positive_indicators <- c("GHG_Emissions", "Scope2_Emissions", "Policy_Pressure",
                         "Energy_Consumption", "Fossil_Share",
                         "Unemployment_Rate", "Labour_Market_Slack",
                         "Export_ExtraEU", "Import_ExtraEU",
                         "HHI_Employment")

data_long_norm <- data_ready %>%
  group_by(Indicator, Sector_ID) %>%
  mutate(
    min_val  = min(Value, na.rm = TRUE),
    max_val  = max(Value, na.rm = TRUE),
    norm0_1  = if_else(max_val - min_val == 0, 0.5,
                       (Value - min_val)/(max_val - min_val)),
    Value_N  = case_when(
      Indicator == "GHG_Emissions" & Value == 0 ~ 0.00,
      Value == min_val ~ 0.01,
      Value == max_val ~ 0.99,
      TRUE ~ 0.01 + norm0_1 * 0.98
    )
  ) %>%
  ungroup() %>%
  # reverse scales where necessary, tidy-up & relocate
  mutate(
    Value_N = if_else(Indicator %in% positive_indicators, Value_N, 1 - Value_N),
    Value_N = round(Value_N, 3)
  ) %>%
  relocate(Value_N, .after = Value) %>%
  select(-min_val, -max_val, -norm0_1)

# ── Wide version (one column per Indicator, using Value_N) ───────
data_wide_norm <- data_long_norm %>%
  select(-c(Value, Component, Dimension, Unit, Notes)) %>%
  group_by(Country_ID, NUTS_ID, NUTS_Name,
           Sector_ID,  Sector_Name,
           pers_employed, Indicator) %>%
  summarise(Value_N = mean(Value_N, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = Indicator,
    values_from = Value_N
  )

###
# ── Check: within each Indicator x Sector_ID, Value_N has min=0.01 and max=0.99 ──
check_bounds <- data_long_norm %>%
  dplyr::group_by(Indicator, Sector_ID) %>%
  dplyr::summarise(
    minN = if (all(is.na(Value_N))) NA_real_ else min(Value_N, na.rm = TRUE),
    maxN = if (all(is.na(Value_N))) NA_real_ else max(Value_N, na.rm = TRUE),
    n_non_na = sum(!is.na(Value_N)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    ok_min = dplyr::near(minN, 0.01),
    ok_max = dplyr::near(maxN, 0.99),
    ok_strict = ok_min & ok_max
  )

# Adjusted rule: allow GHG_Emissions to have min 0.00 or 0.01 (due to forced zeros)
check_bounds_adj <- check_bounds %>%
  dplyr::mutate(
    ok_min_adj = dplyr::if_else(
      Indicator == "GHG_Emissions",
      dplyr::near(minN, 0.00) | dplyr::near(minN, 0.01),
      dplyr::near(minN, 0.01)
    ),
    ok_adj = ok_min_adj & ok_max
  )

# ── Write both datasets ─────────────────────────────────────────
writexl::write_xlsx(data_long_norm, "Code and data/Derived data/3_Normalized_data_long.xlsx")
writexl::write_xlsx(data_wide_norm, "Code and data/Derived data/3_Normalized_data_wide.xlsx")
