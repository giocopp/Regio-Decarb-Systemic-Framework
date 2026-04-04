# ── libraries ──────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(eurostat)
library(writexl)

# ── 1. read Export & Import datasets separately ───────────────────
indicator_df <- restatapi::get_eurostat_data(
  id = "rd_e_berdindr2",
  filters = c("C", "C10-C12", "C13-C15", "C16-C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31", "C32", "C33" ,"BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "EUR_HAB"), 
  date_filter = c(2022, 2021, 2020),
  exact_match = T,
  label = F,
  cflags = T,
  keep_flags = T,
)

indicator_df_f <- indicator_df |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "Values" = "values",
    "Sector" = "nace_r2",
    "Year" = "time"
  ) |> 
  dplyr::select(NUTS_ID, Sector, Year, Values)

# Impute missing 2022 values with previous years
# Step 1: Ensure the data includes rows for all years in the range (2021–2023)
Eurostat_BERD <- indicator_df_f |> 
  dplyr::mutate(Year = as.integer(as.character(Year))) |>  # Convert to integer
  tidyr::complete(NUTS_ID, Sector, Year = 2020:2022)  # Fill missing rows for 2021-2022

# Step 2: Impute missing values for 2023 using previous years
Eurostat_BERD <- Eurostat_BERD |> 
  dplyr::group_by(NUTS_ID, Sector) |> 
  dplyr::mutate(
    Values = case_when(
      # Impute missing 2023 values using 2022 or 2021
      Year == 2022 & is.na(Values) ~ coalesce(
        Values[Year == 2021 & !is.na(Values)][1], 
        Values[Year == 2020 & !is.na(Values)][1]
      ),
      # Impute missing 2022 values using 2021
      Year == 2022 & is.na(Values) ~ Values[Year == 2021 & !is.na(Values)][1],
      Year == 2021 & is.na(Values) ~ Values[Year == 2020 & !is.na(Values)][1],
      # Keep existing values for other cases
      TRUE ~ Values
    )
  ) |> 
  dplyr::ungroup()

# Step 3: Retain only rows for 2022 and drop the Year column
Eurostat_BERD <- Eurostat_BERD |> 
  dplyr::filter(Year == 2022) |> 
  dplyr::select(-Year)

# EU median if there are still NAs
sector_eu_avg <- Eurostat_BERD |> 
  dplyr::group_by(Sector) |> 
  dplyr::summarize(
    EU_Avg = mean(Values, na.rm = TRUE),  # Calculate the EU median per sector
    .groups = "drop"
  )

# Step 2: Replace NAs in the `Values` column with the sector EU average
Eurostat_BERD <- Eurostat_BERD |> 
  dplyr::left_join(sector_eu_avg, by = "Sector") |>  # Join to get EU averages
  dplyr::mutate(
    Values = ifelse(is.na(Values), EU_Avg, Values)  # Replace NAs with EU average
  ) |> 
  dplyr::select(-EU_Avg)

base_data <- readxl::read_excel("/Users/giocopp/Desktop/LOCALISED-7.1-Paper/Base Data/base_data_plus.xlsx") |> 
  dplyr::select(2, 3) |> 
  dplyr::filter(stringr::str_length(NUTS_ID) == 2)

Eurostat_BERD <- base_data |> 
  dplyr::left_join(Eurostat_BERD, by = "NUTS_ID")

Eurostat_BERD <- Eurostat_BERD |> 
  mutate(
    Country_CD = NUTS_ID,
    Country_Name = NUTS_NAME,
    NUTS_ID = NA_character_,
    NUTS_Name = NA_character_,
    Sector_CD = NA_character_,
    Sector_ID = Sector,
    Sector_Name = NA_character_,
    Component = "Vulnerability",
    Dimension = "Technology",
    Variable = "BERD",
    Year = as.factor(2022),
    Source = "Eurostat",
    Value = as.numeric(Values),
    Unit = "€/inhabitant",
    Value_Norm = NA_real_
  ) |> 
  select(Country_CD, Country_Name, NUTS_ID, NUTS_Name, Sector_CD, Sector_ID, Sector_Name, Component, Dimension, Variable, Year, Source, Value, Unit, Value_Norm)

berd <- Eurostat_BERD %>%
  # map detailed NACE -> trade Sector groups
  mutate(
    Sector_ID = case_when(
      Sector_ID == "C"                               ~ "C",
      Sector_ID %in% c("C10-C12")            ~ "C10-C12",
      Sector_ID %in% c("C13-C15")            ~ "C13-C15",
      Sector_ID %in% c("C16-C18")            ~ "C16-C18",
      Sector_ID %in% c("C19","C20")                  ~ "C19-C20",
      Sector_ID %in% c("C21","C22")                  ~ "C21-C22",
      Sector_ID == "C23"                             ~ "C23",
      Sector_ID == "C24"                             ~ "C24",
      Sector_ID %in% c("C25","C28","C29","C30")      ~ "C25+C28-C30",
      Sector_ID %in% c("C26","C27")                  ~ "C26-C27",
      Sector_ID %in% c("C31","C32")                  ~ "C31-C32",
      Sector_ID == "C33"                             ~ "C33",
      TRUE                                         ~ NA_character_
    )
  ) %>%
  filter(!is.na(Sector_ID)) %>%
  group_by(Country_ID, NUTS_ID, Sector_ID) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")


# ── 2. Persons employed in local units for 2022, NUTS-2, EU-27 ────────────────────────
pop_reg <- restatapi::get_eurostat_data(
  id          = "demo_r_d2jan",
  filters     = c("TOTAL", "T"),
  date_filter = 2022,
  exact_match = TRUE,
  label       = FALSE,
  cflags      = TRUE,
  keep_flags  = TRUE
)

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT",
          "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

pop_reg <- pop_reg %>% 
  mutate(geo = as.character(geo)) %>% 
  filter(substr(geo, 1, 2) %in% eu27, !grepl("ZZ$", geo)) %>% 
  filter(nchar(geo) %in% c(2, 4))

pop_reg <- pop_reg %>% 
  mutate(country = substr(geo, 1, 2))

pop_weights <- pop_reg %>%
  filter(nchar(geo) == 4) %>%
  transmute(
    Country_ID = country,
    NUTS_ID    = geo,
    region_pop = values
  ) %>%
  group_by(Country_ID) %>%
  mutate(
    country_pop = sum(region_pop, na.rm = TRUE),
    pop_share   = if_else(country_pop > 0, region_pop / country_pop, 0)
  ) %>%
  ungroup()

# ── down-scale BERD ─────────────────────────
# national €/inhab -> regional € totals
berd_reg <- berd %>% 
  rename(Country_ID = Country_ID,
         nat_percap = Value) %>%          # national €/inhab
  select(-NUTS_ID) %>%        # drop NA-only placeholders
  left_join(pop_weights,
            by = "Country_ID",
            relationship = "many-to-many") %>% 
  mutate(
    Country_CD = Country_ID,
    NUTS_Name  = NA_character_,           # names not in pop_weights → keep NA
    Unit       = "Euro per inhabitant",                  # regional totals are in euro
    Value      = nat_percap * pop_share  # € total in each NUTS‑2
  ) %>% 
  select(Country_CD, NUTS_ID, Sector_ID, Unit, Value)  # same order

# ── save  ────────────────────────────
write_xlsx(berd_reg,
           "Code and data/Initial data/Sector data/TECH-BERD.xlsx")
