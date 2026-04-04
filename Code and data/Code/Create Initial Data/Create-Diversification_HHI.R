# ── Create-Diversification_HHI.R ──────────────────────────────────
# Creates Herfindahl-Hirschman Index of manufacturing employment
# concentration for each NUTS-2 region. Higher HHI = more concentrated
# = less diversified = more vulnerable.
#
# Source: EMPL_Region.xlsx (employment shares already computed)
# Reference: Boschma (2015) related variety; OECD/JRC Handbook (2008)
# ──────────────────────────────────────────────────────────────────

library(dplyr)
library(readxl)
library(writexl)

# ── 1) Read employment shares ────────────────────────────────────
empl <- read_xlsx("Code and data/Initial data/Sector data/EMPL_Region.xlsx")

cat("Input: EMPL_Region.xlsx\n")
cat("  Rows:", nrow(empl), "\n")
cat("  Sectors:", paste(sort(unique(empl$Sector_ID)), collapse = ", "), "\n")
cat("  Regions:", length(unique(empl$NUTS_ID)), "\n")

# ── 2) Compute HHI per region ────────────────────────────────────
# HHI = sum(share_s^2) across manufacturing subsectors
# Range: 1/N (perfect diversification) to 1 (single sector)
# Using 10 subsectors: minimum HHI = 1/10 = 0.10

hhi <- empl %>%
  filter(!is.na(Value), Value >= 0) %>%
  group_by(NUTS_ID) %>%
  summarise(
    n_sectors  = n(),
    sum_shares = sum(Value, na.rm = TRUE),
    HHI        = sum(Value^2, na.rm = TRUE),
    .groups    = "drop"
  )

# ── 3) Quality checks ────────────────────────────────────────────
cat("\nQuality checks:\n")
cat("  Regions with shares not summing to ~1:\n")
off_regions <- hhi %>% filter(abs(sum_shares - 1) > 0.05)
if (nrow(off_regions) > 0) {
  print(off_regions)
} else {
  cat("  All regions sum to ~1 (within 0.05 tolerance)\n")
}

cat("  HHI range:", round(min(hhi$HHI), 4), "to", round(max(hhi$HHI), 4), "\n")
cat("  HHI mean:", round(mean(hhi$HHI), 4), "\n")
cat("  HHI median:", round(median(hhi$HHI), 4), "\n")

# ── 4) Format output ──────���──────────────────────────────────────
hhi_out <- hhi %>%
  transmute(
    Country_CD   = substr(NUTS_ID, 1, 2),
    Country_Name = NA_character_,
    NUTS_ID      = NUTS_ID,
    NUTS_Name    = NA_character_,
    Sector_CD    = NA_character_,
    Sector_ID    = NA_character_,
    Component    = "Vulnerability",
    Dimension    = "Diversification",
    Variable     = "HHI_Employment",
    Unit         = "Index (0-1)",
    Value        = round(HHI, 6),
    Value_Norm   = NA_real_
  )

cat("\nOutput rows:", nrow(hhi_out), "\n")

# ── 5) Save ──────────────────────────────────────────────────────
writexl::write_xlsx(
  hhi_out,
  "Code and data/Initial data/Non sector data/DIVERS-HHI.xlsx"
)
cat("Saved: Code and data/Initial data/Non sector data/DIVERS-HHI.xlsx\n")
