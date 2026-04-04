library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(writexl)

### 

folder_path <- "Code and data/Derived data/"

non_sector_data <- read_excel(paste0(folder_path, "1A_Non_sector_data.xlsx")) |> 
  select(-Value_N)

sector_data <- read_excel(paste0(folder_path, "1B_Sector_data.xlsx")) |> 
  filter(!is.na(Sector_ID)) |> 
  filter(!Sector_ID %in% c("A", "H")) |> 
  select(-Value_N)

### Merge them
all_data <- bind_rows(sector_data, non_sector_data) %>%
  mutate(Country_ID = if_else(is.na(Country_ID) & !is.na(NUTS_ID),
                              substr(NUTS_ID, 1, 2),
                              Country_ID))

all_data <- all_data %>%
  group_by(NUTS_ID) %>%
  fill(NUTS_Name, .direction = "downup") %>%
  ungroup()

all_data <- all_data %>%
  mutate(NUTS_ID = if_else(NUTS_ID == Country_ID, NA_character_, NUTS_ID),
         NUTS_Name = if_else(NUTS_Name == NUTS_ID, NA_character_, NUTS_Name))

all_data <- all_data %>%
  mutate(Indicator = if_else(Indicator == "Energy consumption", "Energy_Consumption", Indicator),
         Indicator = if_else(Indicator == "Unemployment_rate", "Unemployment_Rate", Indicator),
         Indicator = if_else(Indicator == "Wage_per_h", "Wage_Per_h", Indicator),
         Indicator = if_else(Indicator == "HHI_Employment", "HHI_Employment", Indicator),
         Indicator = if_else(Indicator == "RE_Potential", "RE_Potential", Indicator),
         Indicator = if_else(Indicator == "QoG_Index", "QoG_Index", Indicator),
         Dimension = if_else(Indicator == "Energy_Consumption", "Energy", Dimension),
         Dimension = if_else(Indicator %in% c("HHI_Employment", "RE_Potential"), "Diversification", Dimension),
         Indicator = if_else(Indicator == "Climate_Mitigation_Laws", "Climate_Mitigation_Laws", Indicator),
         Dimension = if_else(Indicator %in% c("QoG_Index", "Climate_Mitigation_Laws"), "Institutions", Dimension),
         Dimension = if_else(Indicator %in% c("Scope2_Emissions", "Policy_Pressure"), "Exposure", Dimension))

# Wirte
writexl::write_xlsx(all_data, "Code and data/Derived data/1C_All_data_long.xlsx")

