### INSTALL PACKAGES

remotes::install_github(
  "eurostat/restatapi"
)

libs <- c(
  "restatapi",
  "tidyverse",
  "readxl",
  "giscoR",
  "sf",
  "classInt"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == FALSE)) {
  install.packages(
    libs[!installed_libs],
    dependencies = TRUE
  )
}

invisible(
  lapply(
    libs, library,
    character.only = TRUE
  )
)

### GET DATA
### 

indicator_df <- restatapi::get_eurostat_data(
  id = "env_ac_ainah_r2",
  filters = c("GHG", "THS_T"),
  date_filter = c(2022),
  exact_match = F,
  label = F,
  cflags = T,
  keep_flags = T
)

indicator_df_f <- indicator_df |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "Emissions" = "values",
    "Sector" = "nace_r2",
    "Time" = "time",
    "Flags" = "flags"
  ) 

indicator_df_f <- indicator_df_f |> 
  dplyr::filter(substr(Sector, 1, 1) == "C") 

base_data <- readxl::read_xlsx(
  "Code and data/Initial data/base_data_plus.xlsx"
) |> 
  dplyr::select(1:3) |> 
  filter(nchar(NUTS_ID) == 2)

indicator_df_f <- base_data |> 
  dplyr::left_join(
    indicator_df_f,
    by = "NUTS_ID"
  )

indicator_df_f <- indicator_df_f |> 
  dplyr::select(c(NUTS_ID, dplyr::everything()))

Emissions_raw_data <- indicator_df_f |>
  mutate(Sector = str_replace(Sector, "C31_C32", "C31-C32")) |> 
  dplyr::filter(!str_detect(NUTS_ID, "ZZ"))

unique(Emissions_raw_data$Sector)

### RECALCULATE WITH AGGREGATED SECTORS
# Step 1: Create a mapping table for aggregation
sector_mapping <- data.frame(
  Original_Sector = c("C", "C10-C12", "C13-C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31-C32", "C33"),
  Aggregated_Sector = c("C", "C10-C12", "C13-C15", "C16-C18", "C16-C18", "C16-C18", "C19-C20", "C19-C20", "C21-C22", "C21-C22", "C23", "C24", "C25+C28-C30", "C26-C27", "C26-C27", "C25+C28-C30", "C25+C28-C30", "C25+C28-C30", "C31-C33", "C31-C33")
)

# Step 2: Map the original sectors to the aggregated sectors
Emissions_raw_data <- Emissions_raw_data %>%
  left_join(sector_mapping, by = c("Sector" = "Original_Sector"))

# Step 3: Aggregate employment data by `Country`, `NUTS_ID`, and `Aggregated_Sector`
aggregated_data <- Emissions_raw_data %>%
  group_by(NUTS_ID, Aggregated_Sector) %>%
  summarise(Aggregated_Emissions = sum(Emissions, na.rm = TRUE), .groups = "drop")

Emissions_raw_data <- aggregated_data %>%
  rename(Sector = Aggregated_Sector, Emissions = Aggregated_Emissions)

### DOWNSCALE EMSSIONS

# Load the datasets
shares <- readxl::read_excel("Code and data/Initial data/Regional_Employment_Weights.xlsx")

# Ensure types (and numeric emissions)
emissions <- Emissions_raw_data %>%
  mutate(
    NUTS_ID   = as.character(NUTS_ID),
    Sector_ID = as.character(Sector),
    Emissions = as.numeric(Emissions)   # THS_T (thousand tonnes)
  )

shares <- shares %>%
  mutate(NUTS_ID = as.character(NUTS_ID),
         Sector_ID = as.character(Sector_ID))

# Fallback: C weights
shares_C <- shares %>%
  filter(Sector_ID == "C") %>%
  select(Country_ID, NUTS_ID, weight_C = weight)

# Filter emissions to keep only national-level data
national_emissions <- emissions %>%
  filter(nchar(NUTS_ID) == 2) %>%
  rename(Country_ID = NUTS_ID)

# Merge national emissions with regional shares
regional_emissions <- national_emissions %>%
  left_join(shares,   by = c("Country_ID", "Sector_ID")) %>%
  left_join(shares_C, by = c("Country_ID", "NUTS_ID")) %>%
  group_by(Country_ID, Sector_ID) %>%
  mutate(
    weight = coalesce(weight, weight_C),
    wsum   = sum(weight, na.rm = TRUE),
    weight = if_else(wsum > 0, weight / wsum, 0),
    # THS_T → tCO2e (×1000), then apply weights
    Value      = Emissions * 1000 * weight,
    Indicator  = "GHG Emissions",
    Unit       = "tCO2eq"
  ) %>%
  ungroup() %>%
  select(Country_ID, NUTS_ID, Sector_ID, Indicator, Unit, Value)

# Check if the sum of regional emissions matches national emissions
ghg_chk <- regional_emissions %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(reg_sum_t = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    national_emissions %>% mutate(nat_t = Emissions * 1000),
    by = c("Country_ID", "Sector_ID")
  ) %>%
  mutate(diff = reg_sum_t - nat_t)

summary(abs(ghg_chk$diff))  # should be ~0 (floating-point noise)

View(regional_emissions)

# Save the updated dataset
writexl::write_xlsx(
  regional_emissions,
  "Code and data/Initial data/Sector Data/EXP-Emissions.xlsx")
