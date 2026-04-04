library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)

### 

folder_path <- "Code and data/Initial data/Non sector data/"

# Process individual files
process_file <- function(file_path) {
  # Read cell A1 to check for metadata (e.g., "Dimension:" and "Indicator:")
  title_cell <- read_excel(file_path, range = "A1", col_names = FALSE)[[1,1]]
  metadata_exists <- grepl("Dimension:", title_cell)
  
  # If metadata exists, skip the first row; otherwise, read normally.
  if (metadata_exists) {
    data <- read_excel(file_path, skip = 1)
  } else {
    data <- read_excel(file_path)
  }
  
  data <- clean_names(data)  # Clean up messy column names
  
  # Extract metadata from A1 if present; otherwise, derive from file name
  if (metadata_exists) {
    dimension <- str_extract(title_cell, "(?<=Dimension: )[^;]+") %>% str_trim()
    indicator <- str_extract(title_cell, "(?<=Indicator: )[^;]+") %>% str_trim()
  } else {
    file_name <- basename(file_path)
    split_name <- strsplit(file_name, "-")[[1]]
    dimension <- toupper(split_name[1])
    indicator <- gsub("\\.xlsx", "", split_name[2])
  }
  
  # Harmonize Dimension names 
  # Convert to lowercase and then standardize to one of:
  # Energy, Labor, Supply Chain, Technology, Finance, or Institutions.
  dimension <- tolower(dimension) %>% str_trim()
  dimension <- case_when(
    dimension %in% c("tech", "technology") ~ "Technology",
    dimension %in% c("labor", "labour") ~ "Labor",
    dimension %in% c("supply chain", "supplychain", "supply") ~ "Supply Chain",
    dimension %in% c("finance") ~ "Finance",
    dimension %in% c("institutions", "institution", "inst") ~ "Institutions",
    dimension %in% c("energy") ~ "Energy",
    dimension %in% c("diversification", "divers") ~ "Diversification",
    TRUE ~ dimension
  )
  
  # NUTS_ID 
  # If no column matching "nuts.*id" is found but a "region_code" exists, use it.
  if (!any(grepl("nuts.*id", names(data), ignore.case = TRUE)) && "region_code" %in% names(data)) {
    data <- data %>% rename(nuts_id = region_code)
  } else {
    nuts_id_candidates <- names(data)[grepl("nuts.*id", names(data), ignore.case = TRUE)]
    if (length(nuts_id_candidates) > 0) {
      data <- data %>% rename(nuts_id = all_of(nuts_id_candidates[1]))
    } else {
      data <- data %>% mutate(nuts_id = NA)
    }
  }
  
  # NUTS_Name 
  # Look for a column resembling NUTS_Name; if not found, set to NA.
  nuts_name_candidates <- names(data)[grepl("nuts.*name", names(data), ignore.case = TRUE)]
  if (length(nuts_name_candidates) > 0) {
    data <- data %>% rename(nuts_name = all_of(nuts_name_candidates[1]))
  } else {
    data <- data %>% mutate(nuts_name = NA)
  }
  
  # Country_ID
  # If "country_cd" exists, rename it; otherwise, derive from the first two characters of nuts_id.
  if ("country_cd" %in% names(data)) {
    data <- data %>% rename(country_id = country_cd)
  } else if (!"country_id" %in% names(data)) {
    data <- data %>% mutate(country_id = ifelse(!is.na(nuts_id), substr(nuts_id, 1, 2), NA))
  }
  
  # Value 
  # If no "value" column, check for "gfcf"; otherwise, fill with NA.
  if (!"value" %in% names(data)) {
    if ("gfcf" %in% names(data)) {
      data <- data %>% rename(value = gfcf)
    } else {
      data <- data %>% mutate(value = NA)
    }
  }
  
  # Unit
  if (!"unit" %in% names(data)) {
    data <- data %>% mutate(unit = NA)
  }
  
  # For non‑sector data, add missing columns:
  #   Sector_ID, Sector_Name: NA
  #   Component: fixed as "Vulnerability" (adjust if needed)
  #   Dimension and Indicator: from metadata (or file name)
  #   Value_N: NA (to be computed later)
  data <- data %>%
    mutate(
      sector_id = NA,
      sector_name = NA,
      component = "Vulnerability",
      dimension = dimension,
      indicator = indicator,
      value_n = NA
    )
  
  # Reorder columns to the desired structure:
  # Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name, Component,
  # Dimension, Indicator, Unit, Value, Value_N
  data <- data %>% 
    select(country_id, nuts_id, nuts_name, sector_id, sector_name,
           component, dimension, indicator, unit, value, value_n)
  
  # Rename columns exactly as required
  colnames(data) <- c("Country_ID", "NUTS_ID", "NUTS_Name", "Sector_ID", "Sector_Name",
                      "Component", "Dimension", "Indicator", "Unit", "Value", "Value_N")
  
  return(data)
}

# List of non‑sector Excel files (full paths)
files <- c(
  paste0(folder_path, "FINANCE-Capital_Stock_Based_Prod.xlsx"),
  paste0(folder_path, "FINANCE-GFCF.xlsx"),
  paste0(folder_path, "LABOUR-Highly_Skilled_Workers.xlsx"),
  paste0(folder_path, "LABOUR-Labour_Market_Slack.xlsx"),
  paste0(folder_path, "LABOUR-Unemployment.xlsx"),
  paste0(folder_path, "LABOUR-Wages.xlsx"),
  paste0(folder_path, "TECH-RIS.xlsx"),
  paste0(folder_path, "DIVERS-HHI.xlsx"),
  paste0(folder_path, "DIVERS-RE_Potential.xlsx")
)

# Add institutional indicators if available
qog_path <- paste0(folder_path, "INST-QoG.xlsx")
if (file.exists(qog_path)) files <- c(files, qog_path)

ccl_path <- paste0(folder_path, "INST-Climate_Laws.xlsx")
if (file.exists(ccl_path)) files <- c(files, ccl_path)

# Process each file and combine them into one dataset
non_sector_data <- lapply(files, process_file) %>% bind_rows()

# 2. Harmonize region information using base_data

# Read the base_data file that contains the full official list of regions.
# (Assume it has columns: cntr_code, nuts_id, nuts_name)
base_data_path <- "Code and data/Initial data/base_data_plus.xlsx"
base_data <- read_excel(base_data_path) %>% clean_names()

# Keep only rows with NUTS_ID having 4 characters (NUTS2 level)
base_data <- base_data %>% filter(nchar(nuts_id) == 4)

base_ids <- base_data %>% 
  select(cntr_code, nuts_id, nuts_name) %>% 
  distinct() %>%
  rename(Country_ID = cntr_code, 
         NUTS_ID = nuts_id, 
         NUTS_Name = nuts_name)

# For each processed dataset row, replace its Country_ID and NUTS_Name with the official ones,
# matching on NUTS_ID.
non_sector_data_harmonized <- non_sector_data %>%
  select(-Country_ID, -NUTS_Name) %>%       # drop the current (possibly incomplete) region info
  left_join(base_ids, by = "NUTS_ID") %>%    # join official region data by NUTS_ID
  select(Country_ID, NUTS_ID, NUTS_Name, everything())

# 3. Create final dataset: complete grid of (region x indicator)

# Extract unique indicator metadata from the harmonized dataset
indicators_info <- non_sector_data_harmonized %>% 
  select(Indicator, Dimension, Component, Unit) %>% 
  distinct()

# Create a complete grid: every official region (from base_ids) paired with every indicator
complete_grid <- crossing(base_ids, indicators_info)

# Merge the harmonized non‑sector data onto the complete grid so that for each (region, indicator)
# we get the available values (or NA if missing)
non_sector_data <- complete_grid %>%
  left_join(non_sector_data_harmonized, 
            by = c("Country_ID", "NUTS_ID", "NUTS_Name", "Indicator", "Dimension", "Component", "Unit"))

# Reorder columns to match the desired final structure:
non_sector_data <- non_sector_data %>%
  select(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name, Component,
         Dimension, Indicator, Unit, Value, Value_N)

# Inspect the final harmonized dataset
non_sector_data <- non_sector_data %>%
mutate(Indicator = if_else(Indicator == "0 Summary Innovation Index", "Regional_Innovation", Indicator))

non_sector_data <- non_sector_data %>% 
  mutate(Unit = case_when(
    tolower(Unit) == "percentage" ~ "Percentage",
    Indicator == "GFCF" ~ "Million euro",
    Indicator == "Capital_Stock_Based_Prod" ~ "Index",
    Indicator == "Regional_Innovation" ~ "Index",
    Indicator == "Unemployment" ~ "Percentage",
    Indicator == "Wages" ~ "Euro per hour",
    TRUE ~ Unit
  )) %>% 
  filter(!(Dimension == "Technology" & is.na(Indicator))) %>% 
  mutate(Indicator = case_when(
    Indicator == "Wages" ~ "Wage_per_h",
    Indicator == "Unemployment" ~ "Unemployment_rate",
    Indicator == "GFCF" ~ "Gross_Fixed_Capital_Formation",
    Indicator == "HHI" ~ "HHI_Employment",
    Indicator == "RE_Potential" ~ "RE_Potential",
    Indicator == "QoG" ~ "QoG_Index",
    Indicator == "Climate_Laws" ~ "Climate_Mitigation_Laws",
    TRUE ~ Indicator
  ))

# Write
writexl::write_xlsx(non_sector_data, "Code and data/Derived data/1A_Non_sector_data.xlsx")
