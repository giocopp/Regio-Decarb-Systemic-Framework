# ── Create-RE_Potential.R ─────────────────────────────────────────
# Creates a renewable energy potential indicator at NUTS-2 level
# using JRC ENSPRESO data (Energy System Potentials for Renewable
# Energy Sources).
#
# Data source: JRC ENSPRESO Integrated NUTS-2 Data
#   Ruiz et al. (2019), "ENSPRESO - an open, EU-28 wide, transparent
#   and coherent database of wind, solar and biomass energy potentials"
#   Energy Strategy Reviews, 26, 100379.
#
# The data represents TECHNICAL POTENTIAL (resource-based, not time-
# specific) -- how much RE could physically be produced in each region
# given wind speeds, solar irradiance, land availability, and biomass
# resources. We use the "medium" scenario.
#
# Total RE potential = wind_onshore + solar_total + biomass_total (TWh)
# ──────────────────────────────────────────────────────────────────

library(dplyr)
library(readxl)
library(writexl)

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT",
          "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# ── 1) Read ENSPRESO NUTS-2 data ─────────────────────────────────
ensp <- read.csv(
  "Code and data/Initial data/Non sector data/ENSPRESO_Integrated_Data/ENSPRESO_Integrated_NUTS2_Data.csv",
  sep = ";", stringsAsFactors = FALSE
)

cat("ENSPRESO data:", nrow(ensp), "NUTS-2 regions\n")

# ── 2) Compute total RE potential (medium scenario) ──────────────
ensp_clean <- ensp %>%
  mutate(
    Country_ID = substr(nuts2_code, 1, 2),
    # Total RE potential = wind + solar + biomass (TWh, medium scenario)
    RE_Total_TWh = wind_onshore_production_twh_medium +
                   solar_production_twh_medium_total +
                   biomass_production_twh_medium_total
  ) %>%
  filter(Country_ID %in% eu27)

cat("EU-27 regions:", nrow(ensp_clean), "\n")
cat("RE Total (TWh) range:", round(range(ensp_clean$RE_Total_TWh), 2), "\n")
cat("RE Total (TWh) mean:", round(mean(ensp_clean$RE_Total_TWh), 2), "\n")

# ── 3) Show top/bottom regions ───────────────────────────────────
cat("\nTop 10 RE potential regions (TWh):\n")
ensp_clean %>%
  arrange(desc(RE_Total_TWh)) %>%
  select(nuts2_code, RE_Total_TWh,
         wind = wind_onshore_production_twh_medium,
         solar = solar_production_twh_medium_total,
         biomass = biomass_production_twh_medium_total) %>%
  head(10) %>% as.data.frame() %>% print()

cat("\nBottom 10 RE potential regions (TWh):\n")
ensp_clean %>%
  arrange(RE_Total_TWh) %>%
  select(nuts2_code, RE_Total_TWh) %>%
  head(10) %>% as.data.frame() %>% print()

# ── 4) Format output ─────────────────────────────────────────────
re_out <- ensp_clean %>%
  transmute(
    Country_CD   = Country_ID,
    Country_Name = NA_character_,
    NUTS_ID      = nuts2_code,
    NUTS_Name    = NA_character_,
    Sector_CD    = NA_character_,
    Sector_ID    = NA_character_,
    Component    = "Vulnerability",
    Dimension    = "Diversification",
    Variable     = "RE_Potential",
    Unit         = "TWh (technical potential, medium scenario)",
    Value        = round(RE_Total_TWh, 4),
    Value_Norm   = NA_real_
  )

cat("\nOutput rows:", nrow(re_out), "\n")

# ── 5) Save ──────────────────────────────────────────────────────
writexl::write_xlsx(
  re_out,
  "Code and data/Initial data/Non sector data/DIVERS-RE_Potential.xlsx"
)
cat("Saved: Code and data/Initial data/Non sector data/DIVERS-RE_Potential.xlsx\n")
