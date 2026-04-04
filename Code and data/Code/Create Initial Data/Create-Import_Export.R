# ── libraries ──────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

### Install Necessary Packages ###
remotes::install_github("eurostat/restatapi")

libs <- c(
  "restatapi",
  "tidyverse",
  "giscoR",
  "sf",
  "classInt",
  "mice",
  "visdat",
  "VIM"
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
### IMPORT EXPORT DATA

indicator_df <- restatapi::get_eurostat_data(
  id = "ext_tec09",
  filters = c("EXT_EU", "C", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31", "C32", "C33", "IMP", "EXP", "THS_EUR"), 
  date_filter = c(2022, 2021, 2019, 2018),
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
    "Stk_Flow" = "stk_flow",
    "Year" = "time"
  ) |> 
  dplyr::select(NUTS_ID, Sector, Year, Stk_Flow, Values)

# Load base data
base_data <- readxl::read_excel("Code and data/Raw data/base_data_plus.xlsx") |> 
  dplyr::select(2, 3) |> 
  dplyr::filter(stringr::str_length(NUTS_ID) == 2)

Eurostat_Imp_Exp <- base_data |> 
  dplyr::left_join(indicator_df_f, by = "NUTS_ID") |> 
  dplyr::select(-NUTS_NAME)

# Impute missing 2022 values with previous years and EU avg
Eurostat_Imp_Exp <- Eurostat_Imp_Exp |> 
  dplyr::group_by(NUTS_ID, Sector, Stk_Flow) |> 
  dplyr::mutate(
    # Impute missing values by cascading from 2021 to earlier years
    Values = ifelse(Year == 2022 & is.na(Values), Values[Year == 2021], Values),
    Values = ifelse(Year == 2021 & is.na(Values), Values[Year == 2019], Values),
    Values = ifelse(Year == 2019 & is.na(Values), Values[Year == 2018], Values)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(Year == 2022) |> 
  dplyr::group_by(Sector, Stk_Flow) |> 
  # dplyr::mutate(
  # Impute remaining missing values with EU average for each subsector
  #   Values = ifelse(is.na(Values), median(Values, na.rm = TRUE), Values)
  #  ) |> 
  dplyr::ungroup() |>
  select(-Year)

sum(is.na(Eurostat_Imp_Exp$Values))

# EU sector shares from countries with data
eu_share <- Eurostat_Imp_Exp |>
  dplyr::filter(Sector != "C") |>
  dplyr::left_join(
    Eurostat_Imp_Exp |>
      dplyr::filter(Sector == "C") |>
      dplyr::select(NUTS_ID, Stk_Flow, total_C = Values),
    by = c("NUTS_ID","Stk_Flow")
  ) |>
  dplyr::mutate(share = Values / total_C) |>
  dplyr::group_by(Sector, Stk_Flow) |>
  dplyr::summarise(eu_share = median(share, na.rm = TRUE), .groups = "drop")

# Impute missing sector values from EU shares × country total_C
Eurostat_Imp_Exp <- Eurostat_Imp_Exp |>
  dplyr::left_join(
    Eurostat_Imp_Exp |>
      dplyr::filter(Sector == "C") |>
      dplyr::select(NUTS_ID, Stk_Flow, total_C = Values),
    by = c("NUTS_ID","Stk_Flow")
  ) |>
  dplyr::left_join(eu_share, by = c("Sector","Stk_Flow")) |>
  dplyr::mutate(
    Values = dplyr::if_else(
      is.na(Values) & Sector != "C" & !is.na(total_C) & !is.na(eu_share),
      eu_share * total_C, Values
    )
  ) |>
  dplyr::select(-total_C, -eu_share)

sum(is.na(Eurostat_Imp_Exp$Values))

Eurostat_Imp_Exp <- Eurostat_Imp_Exp |>
  dplyr::group_by(Sector, Stk_Flow) |>
  dplyr::mutate(
    Values = ifelse(is.na(Values), median(Values, na.rm = TRUE), Values)
  ) |>
  dplyr::ungroup()

### Adapt the Sectors
### Step 1: Create a mapping table for aggregation
sector_mapping <- data.frame(
  Original_Sector = c("C","C10","C11","C12","C13","C14","C15","C16","C17","C18",
                      "C19","C20","C21","C22","C23","C24","C25","C26","C27",
                      "C28","C29","C30","C31","C32","C33"),
  Aggregated_Sector = c("C","C10-C12","C10-C12","C10-C12","C13-C15","C13-C15","C13-C15",
                        "C16-C18","C16-C18","C16-C18","C19-C20","C19-C20","C21-C22","C21-C22",
                        "C23","C24","C25+C28-C30","C26-C27","C26-C27","C25+C28-C30",
                        "C25+C28-C30","C25+C28-C30","C31-C33","C31-C33","C31-C33")
)

### Step 2: Map the original sectors to the aggregated sectors
Eurostat_Imp_Exp <- Eurostat_Imp_Exp %>%
  left_join(sector_mapping, by = c("Sector" = "Original_Sector"))

### Step 3: Aggregate data by NUTS_ID, Stk_Flow, and Aggregated_Sector
Eurostat_Imp_Exp_aggregated <- Eurostat_Imp_Exp %>%
  group_by(NUTS_ID, Stk_Flow, Aggregated_Sector) %>%
  summarise(
    Aggregated_Values = sum(Values, na.rm = TRUE),
    .groups = "drop"
  )

### Step 4: Rename columns for clarity
Eurostat_Imp_Exp <- Eurostat_Imp_Exp_aggregated %>%
  rename(Sector = Aggregated_Sector)

# Divide
Eurostat_Exp <- Eurostat_Imp_Exp |> 
  filter(Stk_Flow == "EXP") |> 
  select(-Stk_Flow) |> 
  rename(Exp = Aggregated_Values)

Eurostat_Imp <- Eurostat_Imp_Exp |> 
  filter(Stk_Flow == "IMP") |> 
  select(-Stk_Flow) |> 
  rename(Imp = Aggregated_Values)

# ── 0. helpers ─────────────────────────────────────────────────────
safe_resid <- function(country_emp, sum_known) {
  r <- country_emp - sum_known
  ifelse(r < 0, 0, r)
}

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT",
          "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# ── 1. read Export & Import datasets separately ───────────────────
exp <- Eurostat_Exp %>%
  mutate(
    NUTS_ID = as.character(NUTS_ID),
    Sector  = as.character(Sector),
    Exp     = as.numeric(Exp)
  )

imp <- Eurostat_Imp %>%
  mutate(
    NUTS_ID = as.character(NUTS_ID),
    Sector  = as.character(Sector),
    Imp     = as.numeric(Imp)
  )

### Employment shares
shares <- readxl::read_excel("Code and data/Initial data/Regional_Employment_Weights.xlsx")

shares <- shares %>%
  mutate(
    Country_ID = as.character(Country_ID),
    NUTS_ID    = as.character(NUTS_ID),
    Sector_ID  = as.character(Sector_ID)
  )

shares_C <- shares %>%
  filter(Sector_ID == "C") %>%
  select(Country_ID, NUTS_ID, weight_C = weight)


## No downscaling
# --- choose behavior: TRUE = split equally; FALSE = replicate national value ---
equal_split <- TRUE

# regional NUTS-2 list per country (use your shares file)
regions <- shares %>%
  dplyr::filter(nchar(NUTS_ID) == 4) %>%
  dplyr::distinct(Country_ID, NUTS_ID)

# ── Exports: replicate or equal-split national value to all regions ──
exp_reg <- exp %>%
  dplyr::rename(Country_ID = NUTS_ID, Sector_ID = Sector) %>%
  dplyr::mutate(Exp_mn = Exp / 1000) %>%  # THS_EUR → Million euro
  dplyr::left_join(regions, by = "Country_ID") %>%
  dplyr::group_by(Country_ID, Sector_ID) %>%
  dplyr::mutate(n_reg = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Value     = if (equal_split) Exp_mn / n_reg else Exp_mn,
    Unit      = "Million euro",
    Dimension = "Supply_Chain"
  ) %>%
  dplyr::select(Country_ID, NUTS_ID, Sector_ID, Dimension, Value, Unit)

# ── Imports: replicate or equal-split national value to all regions ──
imp_reg <- imp %>%
  dplyr::rename(Country_ID = NUTS_ID, Sector_ID = Sector) %>%
  dplyr::mutate(Imp_mn = Imp / 1000) %>%  # THS_EUR → Million euro
  dplyr::left_join(regions, by = "Country_ID") %>%
  dplyr::group_by(Country_ID, Sector_ID) %>%
  dplyr::mutate(n_reg = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Value     = if (equal_split) Imp_mn / n_reg else Imp_mn,
    Unit      = "Million euro",
    Dimension = "Supply_Chain"
  ) %>%
  dplyr::select(Country_ID, NUTS_ID, Sector_ID, Dimension, Value, Unit)

# EXP: regional sums should match national (unchanged)
exp_chk <- exp_reg %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(reg_sum_mn = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    exp %>%
      rename(Country_ID = NUTS_ID, Sector_ID = Sector) %>%
      mutate(Exp_mn = Exp / 1000) %>%           # THS_EUR → Million euro
      select(Country_ID, Sector_ID, Exp_mn),
    by = c("Country_ID","Sector_ID")
  ) %>%
  mutate(diff = reg_sum_mn - Exp_mn)

print(summary(abs(exp_chk$diff)))

# IMP: same check (unchanged)
imp_chk <- imp_reg %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(reg_sum_mn = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    imp %>%
      rename(Country_ID = NUTS_ID, Sector_ID = Sector) %>%
      mutate(Imp_mn = Imp / 1000) %>%           # THS_EUR → Million euro
      select(Country_ID, Sector_ID, Imp_mn),
    by = c("Country_ID","Sector_ID")
  ) %>%
  mutate(diff = reg_sum_mn - Imp_mn)

print(summary(abs(imp_chk$diff)))

# ── 8. Save outputs ───────────────────────────────────────────────
write_xlsx(exp_reg,
           "Code and data/Initial data/Sector data/SUPCH-Export.xlsx")
write_xlsx(imp_reg,
           "Code and data/Initial data/Sector data/SUPCH-Import.xlsx")