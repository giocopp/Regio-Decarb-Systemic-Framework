
### INSTALL PACKAGES

remotes::install_github(
  "eurostat/restatapi"
)

libs <- c(
  "restatapi",
  "stringr",
  "tidyverse",
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

### Labour market slack

indicator_df <- restatapi::get_eurostat_data(
  id = "lfst_r_sla_ga",
  filters = c("Y15-74", "PC_ELF", "T"), 
  date_filter = c(2022),
  exact_match = T,
  label = F,
  cflags = T,
  keep_flags = T
)

indicator_df_f <- indicator_df |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "Lab_M_Slack" = "values",
    "Year" = "time"
  ) |> 
  select(NUTS_ID, Year, Lab_M_Slack)

base_data <- readxl::read_excel("Code and data/Initial data/base_data_plus.xlsx") |> 
  dplyr::select(1, 3) |> 
  filter(nchar(NUTS_ID) != 2)

Eurostat_Lab_M_Slack <- base_data |> 
  dplyr::left_join(
    indicator_df_f,
    by = "NUTS_ID")

Eurostat_Lab_M_Slack <- Eurostat_Lab_M_Slack |> 
  mutate(
    Country_CD = NA_character_,
    Country_Name = NA_character_,
    NUTS_Name = NUTS_NAME,
    Sector_CD = NA_character_,
    Sector_ID = NA_character_,
    Sector_Name = NA_character_,
    Component = "Vulnerability",
    Dimension = "Labor",
    Source = "Eurostat",
    Unit = "percentage",
    Value_Norm = NA_real_,
    Variable = "Labor_Market_Slack"
  ) |> 
  rename(Value = Lab_M_Slack) |> 
  select(Country_CD, Country_Name, NUTS_ID, NUTS_Name, Sector_CD, Sector_ID, Sector_Name, Component, Dimension, Variable, Year, Source, Value, Unit, Value_Norm)

# Extract country code from NUTS_ID
Eurostat_Lab_M_Slack <- Eurostat_Lab_M_Slack |> 
  mutate(Country_CD = substr(NUTS_ID, 1, 2))  # First two characters

# Identify countries with missing values
missing_countries <- Eurostat_Lab_M_Slack |> 
  filter(is.na(Value)) |> 
  distinct(Country_CD)

# Compute median Value for each country, ignoring NAs
country_medians <- Eurostat_Lab_M_Slack |> 
  group_by(Country_CD) |> 
  summarise(median_value = median(Value, na.rm = TRUE)) |> 
  ungroup()

# Impute missing values with the country-specific median
Eurostat_Lab_M_Slack <- Eurostat_Lab_M_Slack |> 
  left_join(country_medians, by = "Country_CD") |> 
  mutate(Value = ifelse(is.na(Value), median_value, Value)) |> 
  select(-median_value)  # Remove helper column

# Check if all missing values are imputed
sum(is.na(Eurostat_Lab_M_Slack$Value))

###
### HIGH SKILLED WORKERS DATA
### 

EU_High_Skilled_W <- readxl::read_xlsx("Code and data/Initial data/eurostat-extraction-highly-skilled-employed-people-2022.xlsx") |> 
  rename(NUTS_ID = "NUTS",
         NUTS_Name = "Region name") |> 
  select(NUTS_ID, NUTS_Name, Value) |> 
  mutate(
    Country_CD = NA_character_,
    Country_Name = NA_character_,
    NUTS_Name = NUTS_Name,
    Sector_CD = NA_character_,
    Sector_ID = NA_character_,
    Sector_Name = NA_character_,
    Component = "Vulnerability",
    Dimension = "Labor",
    Variable = "Highly_Skilled_Workers",
    Year = as.factor(2022),
    Source = "Eurostat (elaboration)",
    Value = as.numeric(Value),
    Unit = "percentage",
    Value_Norm = NA_real_
  ) |> 
  select(Country_CD, Country_Name, NUTS_ID, NUTS_Name, Sector_CD, Sector_ID, Sector_Name, Component, Dimension, Variable, Year, Source, Value, Unit, Value_Norm)


### 
### Write
### 

# Install writer if needed
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")

# Ensure output folder exists
out_dir <- "Code and data/Initial data/Non sector data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save files
writexl::write_xlsx(EU_High_Skilled_W,
                    file.path(out_dir, "LABOUR-Highly_Skilled_Workers.xlsx"))

writexl::write_xlsx(Eurostat_Lab_M_Slack,
                    file.path(out_dir, "LABOUR-Labour_Market_Slack.xlsx"))
