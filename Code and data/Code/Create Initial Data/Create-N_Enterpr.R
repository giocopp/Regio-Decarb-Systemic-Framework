# ── libraries ──────────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

###

folder_path <- "Code and data/Initial data/Non sector data/"

# ── 1) Download ────────────────────────────────────────────────────
n_enterpr <- restatapi::get_eurostat_data(
  id          = "sbs_r_nuts2021",
  filters     = c("LOC_NR", "C", "C10", "C11", "C12", "C13", "C14", "C15", 
                  "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", 
                  "C25", "C26", "C27", "C28", "C29", "C30", "C31", "C32", "C33"),
  date_filter = c(2022, 2021),
  exact_match = TRUE,
  label       = FALSE,
  cflags      = TRUE,
  keep_flags  = TRUE
)

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT",
          "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

base_d <- readxl::read_xlsx("/Users/giocopp/Desktop/Paper code and data copy/Primary data/Eurostat data/Base Data/base_data_plus.xlsx") |>
  dplyr::select(CNTR_CODE, NUTS_ID) |>
  dplyr::rename(geo = NUTS_ID, country = CNTR_CODE)

# ── 2) Keep EU-27 national (2) & NUTS-2 (4); align keys ───────────
n_enterpr <- n_enterpr %>% 
  mutate(
    geo       = as.character(geo),
    values    = as.numeric(values),
    country   = substr(geo, 1, 2),
    geo_level = if_else(nchar(geo) == 2, "national", "regional")
  ) %>% 
  filter(substr(geo, 1, 2) %in% eu27, !grepl("ZZ$", geo), nchar(geo) %in% c(2, 4)) %>% 
  semi_join(base_d, by = c("country", "geo"))

# ── 3) Confidential cells → NA ─────────────────────────────────────
n_enterpr <- n_enterpr %>%
  mutate(values = if_else(flags == "c", NA_real_, values))

# ---- 2021 → 2022 carry-forward (do first) ----
n_enterpr <- n_enterpr %>%
  mutate(Year = as.integer(as.character(time))) %>%
  group_by(geo, country, nace_r2, geo_level) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    # if both years exist but 2022 is NA, fill it from 2021
    values    = if_else(Year == 2022 & is.na(values),
                        values[Year == 2021][1], values),
    has2022   = any(Year == 2022),
    pick_year = if_else(has2022, 2022L, 2021L)
  ) %>%
  ungroup() %>%
  filter(Year == pick_year) %>%
  select(-Year, -has2022, -pick_year)

# ── 4) Median donor by country × sector (REGIONAL only) ────────────
country_sector_reg_median <- n_enterpr %>%
  filter(geo_level == "regional") %>%
  group_by(country, nace_r2) %>%
  summarise(donor = median(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(donor = if_else(is.nan(donor), NA_real_, donor))

# ── 5) Impute regional NAs from median donor ───────────────────────
n_enterpr <- n_enterpr %>%
  left_join(country_sector_reg_median, by = c("country", "nace_r2")) %>%
  mutate(
    values = if_else(is.na(values) & geo_level == "regional" & !is.na(donor),
                     donor, values)
  ) %>%
  select(-donor)

# ── 6) Re-anchor regionals to national sector totals (exact) ───────
nat_tot <- n_enterpr %>%
  filter(geo_level == "national") %>%
  select(country, nace_r2, nat_total = values)

n_enterpr <- n_enterpr %>%
  left_join(nat_tot, by = c("country","nace_r2")) %>%
  group_by(country, nace_r2) %>%
  mutate(sum_reg = sum(values[geo_level == "regional"], na.rm = TRUE),
         values  = if_else(geo_level == "regional" & !is.na(nat_total) & sum_reg > 0,
                           values * nat_total / sum_reg, values)) %>%
  ungroup() %>%
  select(-nat_total, -sum_reg)

# ── 7) Enforce ∑(C10–C33) = C within each geo (additivity) ────────
C_geo <- n_enterpr %>% filter(nace_r2 == "C") %>% select(geo, C_geo = values)

n_enterpr <- n_enterpr %>%
  left_join(C_geo, by = "geo") %>%
  group_by(geo) %>%
  mutate(sub_sum = sum(values[nace_r2 != "C"], na.rm = TRUE),
         values  = if_else(nace_r2 != "C" & !is.na(C_geo) & sub_sum > 0,
                           values * C_geo / sub_sum, values)) %>%
  ungroup() %>%
  select(-C_geo, -sub_sum)

# ── 8) Optional rounding (do this last) ────────────────────────────
n_enterpr <- n_enterpr %>% mutate(values = round(values))

# ── 9) Sector aggregation to your custom groups ─────────────────────
n_enterpr <- n_enterpr %>%
  mutate(
    Sector_ID = case_when(
      nace_r2 == "C"                               ~ "C",
      nace_r2 %in% c("C10","C11","C12")            ~ "C10-C12",
      nace_r2 %in% c("C13","C14","C15")            ~ "C13-C15",
      nace_r2 %in% c("C16","C17","C18")            ~ "C16-C18",
      nace_r2 %in% c("C19","C20")                  ~ "C19-C20",
      nace_r2 %in% c("C21","C22")                  ~ "C21-C22",
      nace_r2 == "C23"                             ~ "C23",
      nace_r2 == "C24"                             ~ "C24",
      nace_r2 %in% c("C25","C28","C29","C30")      ~ "C25+C28-C30",
      nace_r2 %in% c("C26","C27")                  ~ "C26-C27",
      nace_r2 %in% c("C31","C32","C33")            ~ "C31-C33",
      TRUE                                         ~ NA_character_
    )
  ) %>%
  filter(!is.na(Sector_ID)) %>%
  group_by(geo, country, Sector_ID) %>%
  summarise(n_enterprises = sum(values, na.rm = TRUE), .groups = "drop")

# ── 10) Croatia HR04 regrouping (HR02+HR05+HR06 → HR04) ────────────
hr04 <- n_enterpr %>%
  filter(geo %in% c("HR02", "HR05", "HR06")) %>%
  group_by(Sector_ID) %>%
  summarise(
    geo           = "HR04",
    country       = "HR",
    n_enterprises = sum(n_enterprises, na.rm = TRUE),
    .groups       = "drop"
  )

n_enterpr <- n_enterpr %>%
  filter(!geo %in% c("HR02", "HR05", "HR06")) %>%
  bind_rows(hr04)

n_enterpr <- n_enterpr %>%
  rename(NUTS_ID = geo,
         Country_ID = country)

# ── 11) Export ─────────────────────────────────────────────────────
writexl::write_xlsx(n_enterpr, "Initial data/Sector data/N_Enterpr.xlsx")


