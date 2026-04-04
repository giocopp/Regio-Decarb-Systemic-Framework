library(tidyverse)
library(readxl)       
library(writexl)      

###

all_data_long <- read_xlsx("Code and data/Derived data/1C_All_data_long.xlsx")

sectors <- all_data_long |>
  filter(!is.na(Sector_ID)) |>
  distinct(Sector_ID, Sector_Name)

regions <- all_data_long |>
  filter(!is.na(NUTS_ID)) |>
  distinct(Country_ID, NUTS_ID, NUTS_Name)

n_regions <- regions |>
  count(Country_ID, name = "n_regions")

# 1. already Region × Sector rows  → unchanged
reg_sec <- all_data_long |>
  filter(!is.na(NUTS_ID), !is.na(Sector_ID)) |>
  mutate(Notes = "data originally at region–sector level")

# 2. sector–national rows  → replicate to every region
sector_nat <- all_data_long |>
  filter(is.na(NUTS_ID) & !is.na(Sector_ID)) |>
  select(-NUTS_ID, -NUTS_Name) |>
  left_join(n_regions, by = "Country_ID") |>
  mutate(
    Value = Value / n_regions,
    Notes = "sector-national value averaged across regions"
  ) |>
  select(-n_regions) |>
  left_join(regions, by = "Country_ID")   # adds region IDs

# 3. region-only rows  → replicate to every sector
#    (cartesian join)  → add note
region_only <- all_data_long |>
  filter(!is.na(NUTS_ID) & is.na(Sector_ID)) |>
  select(-Sector_ID, -Sector_Name) |>
  crossing(sectors) |>
  mutate(Notes = "region-only value duplicated across sectors")

# 4. combine & collapse duplicates  (keep first non-NA note)
all_data_long_ref <- bind_rows(reg_sec, sector_nat, region_only) |>
  group_by(
    Country_ID, NUTS_ID, NUTS_Name,
    Sector_ID,  Sector_Name,
    Component,  Dimension,         
    Indicator,  Unit               
  ) |>
  summarise(
    Value = mean(Value, na.rm = TRUE),
    Notes = Notes[which.max(!is.na(Notes))],
    .groups = "drop"
  ) |>
  filter(!(Indicator == "Energy_Consumption" & Unit != "MWh"))

###

# Fix Croatia NUTS codes overlapping

all_data_long_ref$NUTS_Name[all_data_long_ref$NUTS_ID == "HR04"] <- "Continentalna Hrvatska"

# ---- add *after* reading `nzbc_data` and defining `agg_rules` ----
hr_parts <- c("HR02", "HR05", "HR06")          # regions that made up old HR04

agg_rules <- tibble::tribble(
  ~Indicator,                         ~agg_fun,
  "GHG_Emissions",                   "sum",
  "Energy_Consumption",              "sum",
  "Fossil_Share",                    "mean",
  "Renewables_Share",                "mean",
  "Capital_Stock_Based_Prod",        "mean",
  "Gross_Fixed_Capital_Formation",   "sum",
  "Accountability",                  "mean",
  "Corruption",                      "mean",
  "Impartiality",                    "mean",
  "Highly_Skilled_Workers",          "mean",
  "Labour_Market_Slack",             "mean",
  "Unemployment_Rate",               "mean",
  "Wage_Per_h",                      "mean",
  "Export_ExtraEU",                  "sum",
  "Import_ExtraEU",                  "sum",
  "BERD",                            "mean",
  "Regional_Innovation",             "mean",
  "Intangible_Investments",          "sum",
  "Tangible_Investments",            "sum",
  "Share_of_Employment",             "mean",
  "Scope2_Emissions",                "sum",
  "Policy_Pressure",                 "mean",
  "QoG_Index",                       "mean",
  "HHI_Employment",                  "mean",
  "RE_Potential",                    "mean",
  "Climate_Mitigation_Laws",         "mean"
)

# 1. build the missing HR04 rows -----------------------------------------------
hr04_new <- all_data_long_ref %>% 
  filter(NUTS_ID %in% hr_parts) %>%            # only the three successors
  left_join(agg_rules, by = "Indicator") %>%   # fetch the correct reducer
  group_by(Country_ID, Sector_ID, Sector_Name,
           Component, Dimension, Indicator, Unit, agg_fun) %>% 
  summarise(
    Value = if (first(agg_fun) == "sum") {
      if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE)
    } else {
      if (all(is.na(Value))) NA_real_ else mean(Value, na.rm = TRUE)
    },
    Notes = dplyr::first(Notes[!is.na(Notes)], default = NA_character_),
    .groups = "drop"
  ) %>% 
  mutate(
    NUTS_ID   = "HR04",
    NUTS_Name = "Continentalna Hrvatska"
  ) %>% 
  select(all_of(names(all_data_long_ref)))

# --- copy HR03 values to HR04 *by sector* for the three indicators -------------
to_copy <- all_data_long_ref %>% 
  filter(NUTS_ID == "HR03",
         Indicator %in% c("Unemployment_Rate",
                          "Wage_Per_h",
                          "Capital_Stock_Based_Prod")) %>% 
  mutate(
    NUTS_ID   = "HR04",
    NUTS_Name = "Continentalna Hrvatska"
  ) %>% 
  select(all_of(names(all_data_long_ref)))               # keep identical columns

hr04_new <- dplyr::rows_update(
  hr04_new,                         # data to update
  to_copy,                          # new rows with correct values
  by = c("Country_ID", "NUTS_ID", "Sector_ID", "Indicator", "Unit")
)

# ── push the finished HR04 rows back into the main dataset ──
all_data_long_ref <- dplyr::rows_upsert(
  all_data_long_ref, hr04_new,
  by = c("Country_ID", "NUTS_ID", "Sector_ID", "Indicator", "Unit")
) %>%
  filter(Country_ID != "HR" | NUTS_ID %in% c("HR03", "HR04"))

### Check other NAs in NUTS_ID and Indicator
all_data_long_ref %>% 
  filter(is.na(Value)) %>% 
  distinct(Country_ID, NUTS_ID, Indicator) %>% 
  arrange(Country_ID, NUTS_ID, Indicator)

### Arrange also PT and NL data
### Read Excel file with number of enterprises
n_enterpr <- read_xlsx("Code and data/Initial data/Sector data/N_Enterpr.xlsx") |>
  filter(nchar(NUTS_ID) > 2)

# find the non‑matching NUTS_ID codes ---------------------------------------
enterpr_only <- setdiff(unique(n_enterpr$NUTS_ID),  unique(all_data_long_ref$NUTS_ID))   
data_only    <- setdiff(unique(all_data_long_ref$NUTS_ID),   unique(n_enterpr$NUTS_ID))  
list(only_in_enterpr = enterpr_only, only_in_data = data_only)

# For nzbc_data: all NUTS codes + names for NL and PT
all_data_long_ref %>%
  filter(substr(NUTS_ID, 1, 2) %in% c("NL", "PT")) %>%
  distinct(NUTS_ID, NUTS_Name) %>%
  arrange(NUTS_ID) |> 
  print(n = 26)

# For n_enterpr: NUTS codes (no NUTS_Name column here)
n_enterpr %>%
  filter(substr(NUTS_ID, 1, 2) %in% c("NL", "PT")) %>%
  distinct(NUTS_ID) %>%
  arrange(NUTS_ID)

### Arrange NL and PT data
# --- 1. Netherlands: NL35→NL31, NL36→NL33 -------------------------------------
nl_lookup <- tibble::tribble(
  ~old,  ~new,  ~new_name,
  "NL35","NL31","Utrecht",
  "NL36","NL33","Zuid‑Holland"
)

all_data_long_ref <- all_data_long_ref %>% 
  left_join(nl_lookup, by = c("NUTS_ID" = "old")) %>% 
  mutate(
    NUTS_ID   = coalesce(new, NUTS_ID),
    NUTS_Name = coalesce(new_name, NUTS_Name)
  ) %>% 
  select(-new, -new_name)

# --- 2. Portugal: aggregate & relabel -----------------------------------------
pt_lookup <- tibble::tribble(
  ~old,   ~new,
  "PT19", "PT16",   # Centro
  "PT1D", "PT16",
  "PT1A", "PT17",   # Lisboa
  "PT1B", "PT17",
  "PT1C", "PT18"    # Alentejo
)

pt_new <- all_data_long_ref %>% 
  filter(NUTS_ID %in% c(pt_lookup$old, pt_lookup$new)) %>%   # old + future targets
  left_join(pt_lookup, by = c("NUTS_ID" = "old")) %>% 
  mutate(target = coalesce(new, NUTS_ID)) %>%                # final code
  left_join(agg_rules, by = "Indicator") %>%                 # get sum/mean rule
  group_by(Country_ID, target, Sector_ID, Sector_Name,
           Component, Dimension, Indicator, Unit, agg_fun) %>% 
  summarise(
    Value = if (first(agg_fun) == "sum")
      sum(Value, na.rm = TRUE)        # sum‑type indicators
    else
      mean(Value, na.rm = TRUE),      # mean‑type indicators
    Notes = dplyr::first(Notes[!is.na(Notes)], default = NA_character_),
    NUTS_Name = dplyr::first(NUTS_Name[!is.na(NUTS_Name)],
                             default = NA_character_),
    .groups = "drop"
  ) %>% 
  rename(NUTS_ID = target) %>% 
  select(all_of(names(all_data_long_ref)))             # keep identical columns/order

# --- 3. Upsert aggregated PT rows & drop obsolete codes -----------------------
obsolete_pt <- pt_lookup$old                   # PT19, PT1A‑D

all_data_long_ref <- all_data_long_ref %>% 
  filter(!NUTS_ID %in% obsolete_pt) %>%        # remove old codes
  rows_upsert(pt_new,
              by = c("Country_ID","NUTS_ID",
                     "Sector_ID","Indicator","Unit"))

obsolete_nl <- c("NL35", "NL36")
obsolete_pt <- c("PT19", "PT1A", "PT1B", "PT1C", "PT1D")

all_data_long_ref <- all_data_long_ref %>% 
  filter(!NUTS_ID %in% c(obsolete_nl, obsolete_pt))

n_enterpr <- n_enterpr %>% 
  filter(!NUTS_ID %in% c(obsolete_nl, obsolete_pt))

all_data_long_ref <- all_data_long_ref %>% 
  group_by(NUTS_ID) %>% 
  mutate(NUTS_Name = coalesce(NUTS_Name, first(NUTS_Name[!is.na(NUTS_Name)]))) %>% 
  ungroup()

# Write
writexl::write_xlsx(all_data_long_ref, "Code and data/Derived data/2_All_data_long_READY.xlsx")


