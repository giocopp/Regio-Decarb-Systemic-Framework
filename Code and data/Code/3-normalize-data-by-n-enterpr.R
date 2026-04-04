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

# Divide indicators by the number of enterprises per region per sector
# Read Excel file with number of enterprises
n_enterpr <- read_xlsx("Code and data/Raw data/Sector data/N_Enterpr.xlsx") |> 
  filter(nchar(NUTS_ID) > 2)

# drop the unwanted rows in the enterprise file 
n_enterpr <- n_enterpr %>% 
  semi_join(data_ready %>% dplyr::select(NUTS_ID),          
            by = "NUTS_ID") %>% 
  left_join(data_ready %>%                                   
              dplyr::distinct(NUTS_ID, NUTS_Name),
            by = "NUTS_ID") |>
  dplyr::select(NUTS_ID, NUTS_Name, Sector_ID, n_enterprises)

### Convert some indicators to "per enterprise" 
# indicators to convert to "per enterprise"
to_per_ent <- c("Energy_Consumption", 
                "GHG_Emissions", "Gross_Fixed_Capital_Formation", "BERD")

data_ready <- data_ready %>%
  select(-any_of("n_enterprises")) %>%             # avoid .x/.y suffixing
  left_join(
    n_enterpr %>%
      rename(NUTS_ID = NUTS_ID) %>%
      distinct(NUTS_ID, Sector_ID, n_enterprises), # ensure 1 row per key
    by = c("NUTS_ID", "Sector_ID")
  ) %>%
  mutate(
    Value = if_else(Indicator %in% to_per_ent & !is.na(n_enterprises) & n_enterprises > 0,
                    Value / n_enterprises, Value),
    Notes = if_else(Indicator %in% to_per_ent,
                    if_else(is.na(Notes) | Notes == "", "per enterprise",
                            paste(Notes, "per enterprise", sep = "; ")),
                    Notes))

# Convert to wide format: one row per region-sector-indicator, values spread by year
data_wide <- data_ready %>%
  select(-c(Component, Dimension, Unit, Notes)) %>%
  group_by(Country_ID, NUTS_ID, NUTS_Name,
           Sector_ID,  Sector_Name, Indicator) %>%          # <- full long key
  summarise(
    Value = mean(Value, na.rm = TRUE),
    n_enterprises = dplyr::first(n_enterprises),   # <- keep it
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from  = Indicator,
    values_from = Value,
    id_cols = c(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name, n_enterprises)
  )

### Normalize the data: min-max normalization between 0.01 and 0.99
### 
### The min-max normalization is done by grouping the data by Indicator and Sector_Group (Sector group should include A, H, C and C-sub should include the subsectors of C).
#
# Adjust the direction of normalization:
# some indicators should be normalized in the opposite direction as higher value means lower vulnerability
# Positive relationships: higher value --> higher vulnerability
# - GHG_Emissions (exposure: higher value --> higher exposure)
# - Energy_Consumption 
# - Fossil_Share
# - Unemployment_Rate
# - Labour_Market_Slack
# - Export_ExtraEU
# - Import_ExtraEU
# - Corruption
# Negative relationships: higher value --> lower vulnerability are all the others
# - Intangible_Investments
# - Tangible_Investments
# - BERD
# - Renewables_Share
# - Agricultural_Subsidies
# - Regional_Innovation
# - Accountability
# - Capital_Stock_Based_Prod
# - Highly_Skilled_Workers
# - Impartiality
# - Wage_Per_h

library(stringr)

positive_indicators <- c("GHG_Emissions", "Energy_Consumption", "Fossil_Share", 
                         "Unemployment_Rate", "Labour_Market_Slack", "Export_ExtraEU", 
                         "Import_ExtraEU", "Corruption")

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

# wide version (one column per Indicator, using Value_N)
data_wide_norm <- data_long_norm %>% 
  select(-c(Value, Component, Dimension, Unit, Notes)) %>% 
  group_by(Country_ID, NUTS_ID, NUTS_Name,
           Sector_ID,  Sector_Name,
           n_enterprises, Indicator) %>%          # <- full long key
  summarise(Value_N = mean(Value_N, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(
    names_from  = Indicator,
    values_from = Value_N
  )

###
# ── Check: within each Indicator × Sector_ID, Value_N has min=0.01 and max=0.99 ──
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

# Failing groups under the strict rule (min must be 0.01, max must be 0.99)
fail_strict <- check_bounds %>%
  dplyr::filter(is.na(minN) | is.na(maxN) | !ok_strict) %>%
  dplyr::arrange(Indicator, Sector_ID)

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

# ── write both datasets ─────────────────────────────────────
writexl::write_xlsx(data_long_norm, "Code and data/Derived data/3_Normalized_data_long.xlsx")

writexl::write_xlsx(data_wide_norm, "Code and data/Derived data/3_Normalized_data_wide.xlsx")
