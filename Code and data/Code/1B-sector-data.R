library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(writexl)

###

folder_path <- "Code and data/Initial data/Sector data/"

files <- c(
  paste0(folder_path, "TECH-BERD.xlsx"),
  paste0(folder_path, "SUPCH-Import.xlsx"),
  paste0(folder_path, "SUPCH-Export.xlsx"),
  paste0(folder_path, "EXP-Emissions-Correct.xlsx"),
  paste0(folder_path, "ENERGY-Shares_Eurostat_nat.xlsx"),
  paste0(folder_path, "ENERGY-Energy-Correct.xlsx"),
  paste0(folder_path, "EMPL_Region.xlsx"),
  paste0(folder_path, "EXP-Scope2_Emissions.xlsx"),
  paste0(folder_path, "EXP-Policy_Pressure.xlsx")
)

process_sector_file <- function(file_path) {
  data <- read_excel(file_path)
  data <- clean_names(data)
  
  # Handle value columns
  # If "value" is missing but both "tang_inv" and "intang_inv" exist,
  # pivot them into one "value" column. (This applies to FINANCE-Investm.xlsx.)
  if (!("value" %in% names(data))) {
    if (all(c("tang_inv", "intang_inv") %in% names(data))) {
      data <- data %>% 
        pivot_longer(
          cols = c(tang_inv, intang_inv),
          names_to = "value_type",
          values_to = "value"
        ) %>% 
        mutate(value_n = NA_character_)
      # If there is no indicator column, use the pivoted value_type.
      if (!("indicator" %in% names(data))) {
        data <- data %>% mutate(indicator = value_type)
      }
    } else {
      data <- data %>% 
        mutate(value = NA_real_,
               value_n = NA_character_)
    }
  } else {
    if (!("value_n" %in% names(data))) {
      data <- data %>% mutate(value_n = NA_character_)
    }
  }
  
  # Standardize and create final columns
  # (clean_names() makes all names lower-case)
  
  # Country_ID: use any available candidate
  data$Country_ID <- if ("country_cd" %in% names(data)) {
    data$country_cd
  } else if ("country_id" %in% names(data)) {
    data$country_id
  } else if ("cntr_code" %in% names(data)) {
    data$cntr_code
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # NUTS_ID: if available, use "nuts_id"; otherwise, try "region_code"
  data$NUTS_ID <- if ("nuts_id" %in% names(data)) {
    data$nuts_id
  } else if ("region_code" %in% names(data)) {
    data$region_code
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # NUTS_Name: use if available
  data$NUTS_Name <- if ("nuts_name" %in% names(data)) {
    data$nuts_name
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # Sector_ID: use "sector_id" if available; otherwise "sector_cd"
  data$Sector_ID <- if ("sector_id" %in% names(data)) {
    data$sector_id
  } else if ("sector_cd" %in% names(data)) {
    data$sector_cd
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # Sector_Name: use if available
  data$Sector_Name <- if ("sector_name" %in% names(data)) {
    data$sector_name
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # Component and Dimension: if missing, fill with NA
  data$Component <- if ("component" %in% names(data)) {
    data$component
  } else {
    rep(NA_character_, nrow(data))
  }
  
  data$Dimension <- if ("dimension" %in% names(data)) {
    data$dimension
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # Indicator: use indicator or variable if available.
  # (For files that lack an indicator, the column will be all NA.)
  data$Indicator <- if ("indicator" %in% names(data)) {
    data$indicator
  } else if ("variable" %in% names(data)) {
    data$variable
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # Unit: use if available
  data$Unit <- if ("unit" %in% names(data)) {
    data$unit
  } else {
    rep(NA_character_, nrow(data))
  }
  
  # Derive Indicator from file name if entire column is NA
  if (all(is.na(data$Indicator))) {
    fname <- basename(file_path)
    fname <- tools::file_path_sans_ext(fname)
    # If a dash is present, take the second part; otherwise use the full file name.
    if (grepl("-", fname)) {
      parts <- strsplit(fname, "-")[[1]]
      if (length(parts) >= 2) {
        data$Indicator <- parts[2]
      } else {
        data$Indicator <- fname
      }
    } else {
      data$Indicator <- fname
    }
  }
  
  # Reorder and rename columns to final structure
  data <- data %>%
    select(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name,
           Component, Dimension, Indicator, Unit, value, value_n) %>%
    rename(
      Value = value,
      Value_N = value_n
    )
  
  return(data)
}

# Process all files and combine the results
sector_data <- lapply(files, process_sector_file) %>% bind_rows()

non_sector <- read_xlsx("Code and Data/Derived data/1A_Non_sector_data.xlsx")
unique(non_sector$Dimension)

# Create a mapping from non_sector: unique combinations of Country_ID, NUTS_ID and NUTS_Name
region_mapping <- non_sector %>%
  select(Country_ID, NUTS_ID, NUTS_Name) %>%
  distinct()

# Join the region mapping into your sector_data using Country_ID and NUTS_ID
sector_data <- sector_data %>%
  left_join(region_mapping, by = c("Country_ID", "NUTS_ID"), suffix = c("", ".ns")) %>%
  mutate(NUTS_Name = coalesce(NUTS_Name, NUTS_Name.ns)) %>%  # use non-sector NUTS_Name if missing
  select(-NUTS_Name.ns)

sector_data <- sector_data %>%
  mutate(
    Sector_ID = if_else(Sector_ID %in% c("C31-C32", "C33"), "C31-C33", Sector_ID),
  ) %>%
  group_by(Country_ID, NUTS_ID, NUTS_Name, Sector_ID, Sector_Name, Component, Dimension, Indicator, Unit) %>%
  summarise(
    Value = sum(Value, na.rm = TRUE),
    Value_N = NA_character_,
    .groups = "drop"
  )

# Define the mapping (named vector)
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
  "C25+C28-C30" = "Manufacturing of Fabricated Metal Products, Machinery, Vehicles and Transport Equipment", 
  "C31-C33"     = "Other Manufacturing and Repearing",
  "A"           = "Agriculture, Forestry and Fishing",
  "H"           = "Transportation and Storage"
)

# Update your aggregated_sector_data by mapping Sector_ID to Sector_Name:
sector_data <- sector_data %>%
  mutate(Sector_Name = sector_name_map[Sector_ID])

# Step 1. Rename indicators 
sector_data <- sector_data %>%
  mutate(Indicator = case_when(
    Indicator == "intang_inv" ~ "Intangible_Investments",
    Indicator == "tang_inv" ~ "Tangible_Investments",
    Indicator == "Emissions" ~ "GHG_Emissions",
    Indicator == "Import" ~ "Import_ExtraEU",
    Indicator == "Export" ~ "Export_ExtraEU",
    Indicator == "Scope2_Emissions" ~ "Scope2_Emissions",
    Indicator == "Policy_Pressure" ~ "Policy_Pressure",
    TRUE ~ Indicator
  ))

# Step 3. Add Component and Dimension information:
sector_data <- sector_data %>%
  mutate(
    Component = if_else(
      Indicator %in% c("GHG_Emissions", "Scope2_Emissions", "Policy_Pressure"),
      "Exposure", "Vulnerability"
    ),
    Dimension = case_when(
      Indicator == "Share_of_Employment" ~ "Labor",
      Indicator %in% c("Tangible_Investments", "Intangible_Investments") ~ "Finance",
      Indicator %in% c("GHG_Emissions", "Scope2_Emissions", "Policy_Pressure") ~ "Exposure",
      Indicator %in% c("Import_ExtraEU", "Export_ExtraEU") ~ "Supply_Chain",
      Indicator == "BERD" ~ "Technology",
      Indicator %in% c("Energy_Consumption", "Fossil_Share", "Renewables_Share") ~ "Energy",
      TRUE ~ NA_character_
    )
  )

# Write 
writexl::write_xlsx(sector_data, "Code and data/Derived data/1B_Sector_data.xlsx")
