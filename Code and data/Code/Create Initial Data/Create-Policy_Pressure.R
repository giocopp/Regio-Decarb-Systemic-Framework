# ── Create-Policy_Pressure.R ──────────────────────────────────────
# Creates a sector-level Policy Pressure indicator based on EU ETS
# and CBAM regulatory coverage.
#
# Regulatory sources:
#   EU ETS: Directive 2003/87/EC (as amended by Directive 2023/959)
#     - Annex I lists covered activities by industrial sector
#   CBAM: Regulation (EU) 2023/956
#     - Annex I lists covered products (cement, iron/steel, aluminium,
#       fertilisers, electricity, hydrogen)
#
# This is a SECTOR-LEVEL indicator (no regional variation).
# It is replicated across all NUTS-2 regions in the pipeline.
#
# Literature justification:
#   - Battiston et al. (2017): climate policy transmission channels
#   - Verde (2020): EU ETS competitiveness impacts
#   - Zachmann & McWilliams (2020): CBAM design and sectoral exposure
# ──────────────────────────────────────────────────────────────────

library(dplyr)
library(readxl)
library(writexl)

# ── 1) Define ETS and CBAM coverage by aggregated sector ─────────
# Coverage scores: 1 = fully covered, 0.5 = partially covered, 0 = not covered
#
# ETS coverage rationale (Directive 2003/87/EC, Annex I):
#   C10-C12 (Food/Beverage/Tobacco): Partially -- large combustion >20MW covered
#   C13-C15 (Textiles/Leather): Not covered
#   C16-C18 (Wood/Paper/Printing): Partially -- pulp & paper (>20t/day) covered
#   C19-C20 (Chemicals/Refineries): Fully -- refineries + bulk chemicals
#   C21-C22 (Pharma/Rubber/Plastics): Partially -- large installations only
#   C23 (Non-metallic minerals): Fully -- cement, lime, glass, ceramics
#   C24 (Basic metals): Fully -- iron, steel, aluminium, ferroalloys
#   C25+C28-C30 (Fabricated metals/Machinery/Transport): Partially -- large combustion
#   C26-C27 (Electronics/Electrical): Not covered (small installations)
#   C31-C33 (Other manufacturing): Not covered
#
# CBAM coverage rationale (Regulation 2023/956, Annex I):
#   Products covered: cement, iron, steel, aluminium, fertilisers, hydrogen, electricity
#   C23: cement, lime → covered
#   C24: iron, steel, aluminium → covered
#   C19-C20: hydrogen, some chemicals (fertilisers if included) → partially
#   All others: not covered

policy_data <- tibble::tibble(
  Sector_ID = c("C",           "C10-C12",  "C13-C15", "C16-C18",
                 "C19-C20",     "C21-C22",  "C23",     "C24",
                 "C25+C28-C30", "C26-C27",  "C31-C33"),

  # ETS coverage (0-1 scale)
  ETS_Coverage = c(
    0.50,  # C: aggregate manufacturing (weighted average ~0.5)
    0.25,  # C10-C12: only large combustion installations
    0.00,  # C13-C15: not covered
    0.50,  # C16-C18: pulp & paper covered, wood partially
    1.00,  # C19-C20: refineries + chemicals fully covered
    0.25,  # C21-C22: only largest installations
    1.00,  # C23: cement, lime, glass, ceramics all covered
    1.00,  # C24: iron, steel, aluminium all covered
    0.25,  # C25+C28-C30: only large combustion
    0.00,  # C26-C27: not covered
    0.00   # C31-C33: not covered
  ),

  # CBAM coverage (0-1 scale)
  CBAM_Coverage = c(
    0.25,  # C: aggregate
    0.00,  # C10-C12: not covered
    0.00,  # C13-C15: not covered
    0.00,  # C16-C18: not covered
    0.50,  # C19-C20: hydrogen, fertiliser chemicals
    0.00,  # C21-C22: not covered
    1.00,  # C23: cement covered
    1.00,  # C24: iron, steel, aluminium covered
    0.00,  # C25+C28-C30: not covered (downstream)
    0.00,  # C26-C27: not covered
    0.00   # C31-C33: not covered
  )
)

# ── 2) Compute composite Policy Pressure ─────────────────────────
# Equal-weighted combination of ETS and CBAM coverage
# Both range 0-1, so composite also ranges 0-1
policy_data <- policy_data %>%
  mutate(
    Policy_Pressure = (ETS_Coverage + CBAM_Coverage) / 2
  )

cat("Policy Pressure by sector:\n")
print(policy_data, n = 11)

# ── 3) Read base regions to replicate across ─────────────────────
base_data <- read_xlsx("Code and data/Initial data/base_data_plus.xlsx")
nuts2 <- base_data %>%
  filter(nchar(NUTS_ID) == 4) %>%
  select(NUTS_ID)

cat("\nRegions to replicate to:", nrow(nuts2), "\n")

# ── 4) Create full region x sector dataset ───────────────────────
# Policy Pressure is sector-level only, replicated across all regions
policy_full <- tidyr::crossing(nuts2, policy_data) %>%
  transmute(
    Country_CD   = substr(NUTS_ID, 1, 2),
    Country_Name = NA_character_,
    NUTS_ID      = NUTS_ID,
    NUTS_Name    = NA_character_,
    Sector_CD    = NA_character_,
    Sector_ID    = Sector_ID,
    Component    = "Exposure",
    Dimension    = "Exposure",
    Variable     = "Policy_Pressure",
    Unit         = "Index (0-1)",
    Value        = round(Policy_Pressure, 4),
    Value_Norm   = NA_real_
  )

cat("Output rows:", nrow(policy_full), "\n")
cat("Unique sectors:", length(unique(policy_full$Sector_ID)), "\n")
cat("Unique regions:", length(unique(policy_full$NUTS_ID)), "\n")

# ── 5) Save ──────────────────────────────────────────────────────
writexl::write_xlsx(
  policy_full,
  "Code and data/Initial data/Sector data/EXP-Policy_Pressure.xlsx"
)
cat("Saved: Code and data/Initial data/Sector data/EXP-Policy_Pressure.xlsx\n")

# Also save the mapping table for documentation
writexl::write_xlsx(
  policy_data,
  "Code and data/Initial data/Sector data/EXP-Policy_Pressure_Mapping.xlsx"
)
cat("Saved mapping table: EXP-Policy_Pressure_Mapping.xlsx\n")
