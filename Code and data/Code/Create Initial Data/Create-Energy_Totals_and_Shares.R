### INSTALL PACKAGES

remotes::install_github(
  "eurostat/restatapi"
)

libs <- c(
  "restatapi",
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

indicator_df <- restatapi::get_eurostat_data(
  id = "nrg_bal_c",
  filters = c("FC_IND_E", "FC_IND_IS_E", "FC_IND_CPC_E", "FC_IND_NFM_E", "FC_IND_NMM_E", "FC_IND_TE_E", "FC_IND_MAC_E",
              "FC_IND_MQ_E", "FC_IND_FBT_E", "FC_IND_PPP_E", "FC_IND_WP_E", "FC_IND_CON_E", "FC_IND_TL_E", "FC_IND_NSP_E",
              "TOTAL", "FE", "RA000", 
              "GWH"), 
  date_filter = c(2022),
  exact_match = F,
  label = F,
  cflags = T,
  keep_flags = T
)

indicator_df_f <- indicator_df |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "Values" = "values",
    "Sector" = "nrg_bal",
    "Time" = "time",
    "Source" = "siec",
    "Flags" = "flags"
  ) 

Energy_c_raw_data <- indicator_df_f |> 
  mutate(Source = case_when(
    Source == "TOTAL" ~ "Total",
    Source == "RA000" ~ "Renewables",
    Source == "FE" ~ "Fossil",
    TRUE ~ Source
  ))

Energy_c_raw_data

### Codes Mapping
code_map <- c(
  "FC_IND_E"     = "C",
  "FC_IND_FBT_E" = "C10-C12",
  "FC_IND_TL_E"  = "C13-C15",
  "FC_IND_WP_E"  = "C16-C18",
  "FC_IND_PPP_E" = "C16-C18",
  "FC_IND_CPC_E" = "C19-C20",
  "FC_IND_NMM_E" = "C23",
  "FC_IND_IS_E"  = "C24",
  "FC_IND_NFM_E" = "C24",
  "FC_IND_MAC_E" = "C28",
  "FC_IND_TE_E"  = "C30",
  "FC_IND_NSP_E" = "Other"
)

### Apply Mapping to Aggregate Sectors
Energy_c_raw_data <- Energy_c_raw_data %>%
  mutate(Sector = recode(Sector, !!!code_map)) %>%
  # This will split rows where Sector contains the delimiter "|" into separate rows
  tidyr::separate_rows(Sector, sep = "\\|") 

Energy_c_raw_data <- Energy_c_raw_data |>
  filter(!Sector %in% c("FC_IND_CON_E", "FEC2020-2030", "FC_IND_MQ_E", "FEC_EED")) |>
  group_by(NUTS_ID, Source, unit, Sector, Time) |>
  summarise(Values = sum(Values, na.rm = TRUE), .groups = "drop")

Energy_c_raw_data <- Energy_c_raw_data |> 
  filter(NUTS_ID %in% c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", 
                        "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", 
                        "NL", "PL", "PT", "RO", "SE", "SI", "SK"))

Energy_c_raw_data

# Create vector for new sectors
new_sectors <- c("C21", "C22", "C25", "C26", "C27", "C29", "C31", "C32", "C33")

Energy_c_raw_data_x <- Energy_c_raw_data %>%
  filter(Source == "Total") |> 
  # Adjust "Other" sector consumption by dividing by 9
  mutate(Values = if_else(Sector == "Other", Values / 9, Values)) %>%
  # Split into "Other" and non-"Other" rows and allocate "Other" values to new sectors
  { 
    other_rows <- filter(., Sector == "Other")
    non_other_rows <- filter(., Sector != "Other")
    expanded_rows <- other_rows %>%
      tidyr::crossing(new_sector = new_sectors) %>%  # Create one row per new sector
      mutate(Sector = new_sector) %>%                # Assign the new sector label
      select(-new_sector)
    dplyr::bind_rows(non_other_rows, expanded_rows)
  }

Energy_c_raw_data_total <- Energy_c_raw_data_x 

Energy_c_raw_data_total <- Energy_c_raw_data_total %>%
  mutate(new_sector = case_when(
    Sector == "C" ~ "C",
    Sector == "C10-C12" ~ "C10-C12",
    Sector == "C13-C15" ~ "C13-C15",
    Sector == "C16-C18" ~ "C16-C18",
    Sector == "C19-C20" ~ "C19-C20",
    Sector %in% c("C21", "C22") ~ "C21-C22",
    Sector == "C23" ~ "C23",
    Sector == "C24" ~ "C24",
    Sector %in% c("C26", "C27") ~ "C26-C27",
    Sector %in% c("C25", "C28", "C29", "C30") ~ "C25+C28-C30",
    Sector %in% c("C31", "C32", "C33") ~ "C31-C33",
    TRUE ~ NA_character_
  )) %>%
  group_by(NUTS_ID, Source, unit, new_sector, Time) %>%
  summarise(Values = sum(Values, na.rm = TRUE), .groups = "drop") %>%
  rename(Sector = new_sector)

# Load the datasets
shares <- readxl::read_excel("Code and data/Initial data/Regional_Employment_Weights.xlsx")

# Ensure data types match for merging
energy <- Energy_c_raw_data_total %>%
  mutate(NUTS_ID = as.character(NUTS_ID),
         Sector_ID = as.character(Sector)) |>  
  rename("Value" = "Values")

shares <- shares %>%
  mutate(NUTS_ID = as.character(NUTS_ID),
         Sector_ID = as.character(Sector_ID))

# Filter emissions to keep only national-level data
national_energy <- energy %>% 
  filter(nchar(NUTS_ID) == 2) |> 
  rename("Country_ID" = "NUTS_ID")

# Merge national emissions with regional shares
regional_energy <- national_energy %>%
  left_join(shares, by = c("Country_ID" = "Country_ID", "Sector_ID" = "Sector_ID")) %>%
  rename(Regional_Share = weight) %>%
  mutate(Value = Value * Regional_Share) %>%
  mutate(Indicator = "Energy_Consumption",
         Unit = "MWh") %>%
  select(Country_ID, NUTS_ID, Sector_ID, Indicator, Unit, Value)

### regional_energy is at regional level
writexl::write_xlsx(regional_energy, "Code and data/Initial data/Sector Data/ENERGY-Energy-Correct.xlsx")


###
### Download renewable and fossil energy shares
###


Energy_c_raw_perc <- Energy_c_raw_data %>%
  group_by(NUTS_ID, unit, Sector, Time) %>%
  mutate(
    Total = Values[Source == "Total"],
    Percentage = if_else(Source %in% c("Renewables", "Fossil") & Total > 0,
                         (Values / Total) * 100,
                         NA_real_)
  ) %>%
  ungroup()

Energy_c_raw_data <- Energy_c_raw_perc %>%
  mutate(
    Values = if_else(Source %in% c("Renewables", "Fossil"), Percentage, Values),
    unit = if_else(Source %in% c("Renewables", "Fossil"), "Percentage", unit)
  ) |> 
  select(-c(Total, Percentage))

View(Energy_c_raw_data)

Energy_ren_fos_raw_data <- Energy_c_raw_data |> 
  filter(Source %in% c("Renewables", "Fossil")) 

Energy_ren_fos_raw_data

new_sectors <- c("C21", "C22", "C25", "C26", "C27", "C29", "C31", "C32", "C33")

other_rows <- Energy_ren_fos_raw_data %>%
  filter(Sector == "Other") %>%
  tidyr::crossing(new_sector = new_sectors) %>%
  mutate(Sector = new_sector) %>%
  select(-new_sector)

Energy_ren_fos_raw_data <- Energy_ren_fos_raw_data %>%
  filter(Sector != "Other") %>%
  bind_rows(other_rows)

# Create a new aggregated sector variable
Energy_ren_fos_raw_data <- Energy_ren_fos_raw_data %>%
  mutate(new_sector = case_when(
    Sector == "C" ~ "C",
    Sector == "C10-C12" ~ "C10-C12",
    Sector == "C13-C15" ~ "C13-C15",
    Sector == "C16-C18" ~ "C16-C18",
    Sector == "C19-C20" ~ "C19-C20",
    Sector %in% c("C21", "C22") ~ "C21-C22",
    Sector == "C23" ~ "C23",
    Sector == "C24" ~ "C24",
    Sector %in% c("C26", "C27") ~ "C26-C27",
    Sector %in% c("C25", "C28", "C29", "C30") ~ "C25+C28-C30",
    Sector %in% c("C31", "C32", "C33") ~ "C31-C33",
    TRUE ~ NA_character_
  )) 

# Aggregate by the new sector grouping using the average of the values
Energy_ren_fos_raw_data <- Energy_ren_fos_raw_data %>%
  group_by(NUTS_ID, Source, unit, Time, new_sector) %>%
  summarise(Values = mean(Values, na.rm = TRUE), .groups = "drop") %>%
  rename(Sector = new_sector)

Energy_ren_fos_raw_data

# Allocate national shares to all regions (NUTS-2) and format as requested
library(readxl)
countries_in_data <- unique(Energy_ren_fos_raw_data$NUTS_ID)

nuts2_codes <- readxl::read_xlsx(
  "Code and data/Initial data/base_data_plus.xlsx"
) |> 
  dplyr::select(CNTR_CODE, NUTS_ID, NUTS_NAME) |>
  dplyr::filter(nchar(NUTS_ID) == 4, CNTR_CODE %in% countries_in_data) |>
  dplyr::distinct()

Energy_shares_regions <- Energy_ren_fos_raw_data |>
  dplyr::rename(CNTR_CODE = NUTS_ID) |>
  dplyr::mutate(
    Indicator = dplyr::case_when(
      Source == "Fossil" ~ "Fossil_Share",
      Source == "Renewables" ~ "Renewables_Share"
    )
  ) |>
  dplyr::select(CNTR_CODE, Sector_ID = Sector, Indicator, Value = Values, Unit = unit) |>
  dplyr::left_join(nuts2_codes, by = "CNTR_CODE") |>
  dplyr::transmute(NUTS_ID = .data$NUTS_ID, Sector_ID, Indicator, Value, Unit) |>
  dplyr::arrange(NUTS_ID, Sector_ID, Indicator)

writexl::write_xlsx(regional_energy, "Code and data/Initial data/Sector Data/ENERGY-Shares_Eurostat_nat.xlsx")










