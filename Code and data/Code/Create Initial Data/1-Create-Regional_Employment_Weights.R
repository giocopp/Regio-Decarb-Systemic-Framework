# ── libraries ──────────────────────────────────────────────────────
library(dplyr)
library(stringr)
library(writexl)

# ── 1) Download ────────────────────────────────────────────────────
pers_empl_reg <- restatapi::get_eurostat_data(
  id          = "sbs_r_nuts2021",
  filters     = list(
    indic_sbs = "EMP_LOC_NR",
    sizeclas  = "TOTAL",
    nace_r2   = c("C","C10","C11","C12","C13","C14","C15",
                  "C16","C17","C18","C19","C20","C21","C22",
                  "C23","C24","C25","C26","C27","C28","C29",
                  "C30","C31","C32","C33")
  ),
  date_filter = c(2022, 2021),
  exact_match = TRUE,
  label       = FALSE,
  cflags      = TRUE,
  keep_flags  = TRUE
)

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT",
          "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# ── 2) Keep EU-27 national (2) & NUTS-2 (4); align keys ───────────
base_d <- readxl::read_xlsx("Code and data/Initial data/base_data_plus.xlsx") |>
  dplyr::select(CNTR_CODE, NUTS_ID) |>
  dplyr::rename(geo = NUTS_ID, country = CNTR_CODE)

pers_empl_reg <- pers_empl_reg %>% 
  mutate(
    geo       = as.character(geo),
    values    = as.numeric(values),
    country   = substr(geo, 1, 2),
    geo_level = if_else(nchar(geo) == 2, "national", "regional")
  ) %>% 
  filter(substr(geo, 1, 2) %in% eu27, !grepl("ZZ$", geo), nchar(geo) %in% c(2, 4)) %>% 
  semi_join(base_d, by = c("country", "geo"))

# ── 3) Confidential cells → NA ─────────────────────────────────────
pers_empl_reg <- pers_empl_reg %>%
  mutate(values = if_else(flags == "c", NA_real_, values))

# ---- prefer 2022 when both years exist; otherwise keep whichever exists ----
pers_empl_reg <- pers_empl_reg %>%
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

View(pers_empl_reg)

# ── 4) Median donor by country × sector (REGIONAL only) ────────────
country_sector_median <- pers_empl_reg %>%
  filter(geo_level == "regional") %>%
  group_by(country, nace_r2) %>%
  summarise(donor = median(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(donor = if_else(is.nan(donor), NA_real_, donor))

# ── 5) Impute regional NAs from median donor ───────────────────────
pers_empl_reg <- pers_empl_reg %>%
  left_join(country_sector_median, by = c("country", "nace_r2")) %>%
  mutate(
    values = if_else(is.na(values) & geo_level == "regional" & !is.na(donor),
                     donor, values)
  ) %>%
  select(-donor)

missing_nat <- pers_empl_reg %>%
  dplyr::filter(geo_level == "national", is.na(values)) %>%
  dplyr::select(country, nace_r2, geo, flags) %>%
  dplyr::arrange(country, nace_r2)

if (nrow(missing_nat) > 0) {
  message("Found ", nrow(missing_nat), " missing national cells BEFORE step 6.")
  print(missing_nat)
  # Optional per-country summary:
  print(missing_nat %>% dplyr::count(country, name = "n_missing") %>% dplyr::arrange(desc(n_missing)))
} else {
  message("No missing national cells before step 6.")
}
# ── 6) Fill missing national = sum(regional) ───
# pers_empl_reg <- pers_empl_reg %>%
#   group_by(country, nace_r2) %>%
#   mutate(values = if_else(geo_level == "national" & is.na(values),
#                           sum(values[geo_level == "regional"], na.rm = TRUE),
#                           values)) %>%
#   ungroup()

# ── 7) Re-anchor regionals to national sector totals (exact) ───────
mismatch <- pers_empl_reg %>%
  group_by(country, nace_r2) %>%
  summarise(
    national = values[geo_level == "national"][1],
    sum_reg  = sum(values[geo_level == "regional"], na.rm = TRUE),
    diff     = national - sum_reg,
    .groups = "drop"
  ) %>%
  filter(!is.na(national), sum_reg > 0, abs(diff) > 1e-6)

if (nrow(mismatch) == 0) {
  message("All regionals already sum to national: step 7 is a no-op and can be skipped.")
} else {
  message("Found ", nrow(mismatch), " country×sector mismatches: keep step 7 to re-anchor.")
  print(mismatch %>% arrange(country, nace_r2), n = 100)
}

nat_tot <- pers_empl_reg %>%
  filter(geo_level == "national") %>%
  select(country, nace_r2, nat_total = values)

pers_empl_reg <- pers_empl_reg %>%
  left_join(nat_tot, by = c("country","nace_r2")) %>%
  group_by(country, nace_r2) %>%
  mutate(sum_reg = sum(values[geo_level == "regional"], na.rm = TRUE),
         values  = if_else(geo_level == "regional" & !is.na(nat_total) & sum_reg > 0,
                           values * nat_total / sum_reg, values)) %>%
  ungroup() %>%
  select(-nat_total, -sum_reg)

# --- snapshot before step 8 ---
pre_step8 <- pers_empl_reg

# ── 8) Enforce ∑(C10–C33) = C within each geo (additivity) ────────
C_geo <- pers_empl_reg %>% filter(nace_r2 == "C") %>% select(geo, C_geo = values)

pers_empl_reg <- pers_empl_reg %>%
  left_join(C_geo, by = "geo") %>%
  group_by(geo) %>%
  mutate(sub_sum = sum(values[nace_r2 != "C"], na.rm = TRUE),
         values  = if_else(nace_r2 != "C" & !is.na(C_geo) & sub_sum > 0,
                           values * C_geo / sub_sum, values)) %>%
  ungroup() %>%
  select(-C_geo, -sub_sum)

# --- what changed in step 8 ---
diff_step8 <- pre_step8 %>%
  dplyr::select(geo, country, nace_r2, geo_level, value_before = values) %>%
  dplyr::inner_join(
    pers_empl_reg %>%
      dplyr::select(geo, country, nace_r2, geo_level, value_after = values),
    by = c("geo","country","nace_r2","geo_level")
  ) %>%
  dplyr::mutate(
    delta   = value_after - value_before,
    changed = (is.na(value_before) != is.na(value_after)) |
      (!is.na(value_before) & !is.na(value_after) & value_after != value_before)
  ) %>%
  dplyr::filter(changed)

# quick summaries
# --- summaries (safe when no changes) ---  # place right after diff_step8
if (nrow(diff_step8) == 0) {
  message("Step 8 made no changes.")
  summary_step8 <- tibble::tibble(
    n_changed        = 0L,
    abs_change_total = 0,
    max_abs_change   = 0
  )
} else {
  summary_step8 <- diff_step8 %>%
    dplyr::summarise(
      n_changed        = dplyr::n(),
      abs_change_total = sum(abs(delta), na.rm = TRUE),
      max_abs_change   = max(abs(delta), na.rm = TRUE)
    )
  print(summary_step8)
}

# ── Sector aggregation identical to n_enterpr ───────────────────────
pers_empl_reg <- pers_empl_reg %>%
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
  summarise(pers_employed = sum(values, na.rm = TRUE), .groups = "drop")

pers_empl_reg <- pers_empl_reg %>%
  rename(NUTS_ID = geo,
         Country_ID = country)

# Create the weights
# --- Build employment shares (regional weights) by country × sector ---
pers_empl_reg <- pers_empl_reg %>%
  dplyr::filter(nchar(NUTS_ID) == 4) %>%                     # <— keep only NUTS-2
  dplyr::group_by(Country_ID, Sector_ID) %>%
  dplyr::mutate(
    country_emp = sum(pers_employed, na.rm = TRUE),
    weight      = dplyr::if_else(country_emp > 0, pers_employed / country_emp, NA_real_)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-country_emp)

# --- Fallback: use 'C' weights when sector weights are NA -------------
w_C <- pers_empl_reg %>%
  filter(Sector_ID == "C") %>%
  select(Country_ID, NUTS_ID, weight_C = weight)

pers_empl_reg <- pers_empl_reg %>%
  left_join(w_C, by = c("Country_ID","NUTS_ID")) %>%
  mutate(weight = coalesce(weight, weight_C)) %>%
  select(-weight_C)

# --- Re-normalize to ensure weights sum to 1 within country × sector --
pers_empl_reg <- pers_empl_reg %>%
  group_by(Country_ID, Sector_ID) %>%
  mutate(
    w_sum  = sum(weight, na.rm = TRUE),
    weight = if_else(!is.na(w_sum) & w_sum > 0, weight / w_sum, weight)
  ) %>%
  ungroup() %>%
  select(-w_sum)

pers_empl_reg <- pers_empl_reg %>%
  dplyr::mutate(pers_employed = round(pers_employed)) %>%
  dplyr::select(Country_ID, NUTS_ID, Sector_ID, pers_employed, weight)

### Checks
pers_empl_reg %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(sum_w = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  arrange(abs(sum_w - 1))

# ── 10) (Optional) save ────────────────────────────────────────────
writexl::write_xlsx(
  pers_empl_reg,
  "Code and data/Initial data/Regional_Employment_Weights.xlsx"
)
