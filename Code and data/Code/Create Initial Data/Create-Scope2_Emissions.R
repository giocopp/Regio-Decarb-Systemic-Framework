# ── Create-Scope2_Emissions.R ─────────────────────────────────────
# Approximates Scope 2 GHG emissions for manufacturing subsectors
# by multiplying sectoral electricity consumption by national grid
# emission factors.
#
# Scope2(country, sector) = Electricity_Consumption(country, sector) x Grid_EF(country)
# Then downscaled to NUTS-2 regions using employment weights.
#
# Data sources:
#   - Eurostat nrg_bal_c: Electricity consumption by industrial subsector (GWh, 2022)
#   - Grid emission factors: EEA/EMBER 2022 values (gCO2/kWh)
#     Source: European Environment Agency, "Greenhouse gas emission intensity
#     of electricity generation in Europe" (indicator CSI 049)
#     Cross-checked with EMBER Global Electricity Review 2023
#
# Limitation: Grid EF is country-level (no sub-national variation).
# This is acknowledged in the paper.
# ──────────────────────────────────────────────────────────────────

library(restatapi)
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# ── 1) Grid emission factors (gCO2/kWh) for EU-27, year 2022 ────
# Source: EEA indicator CSI 049 / EMBER Global Electricity Review 2023
# These are well-documented, publicly available values.
grid_ef <- tibble::tribble(
  ~Country_ID, ~Grid_EF_gCO2_kWh,
  "AT",   88,   # Austria: high hydro
  "BE",  149,   # Belgium: nuclear + gas
  "BG",  422,   # Bulgaria: coal-heavy
  "HR",  151,   # Croatia: hydro + imports
  "CY",  605,   # Cyprus: oil/gas isolated grid
  "CZ",  391,   # Czechia: coal + nuclear
  "DK",  108,   # Denmark: wind-dominant
  "EE",  470,   # Estonia: oil shale
  "FI",   73,   # Finland: nuclear + hydro + wind
  "FR",   56,   # France: nuclear-dominant
  "DE",  366,   # Germany: coal + renewables mix
  "EL",  342,   # Greece: lignite + gas + RE
  "HU",  208,   # Hungary: nuclear + gas
  "IE",  279,   # Ireland: gas + wind
  "IT",  247,   # Italy: gas + renewables
  "LV",   80,   # Latvia: hydro
  "LT",   64,   # Lithuania: imports + wind
  "LU",   71,   # Luxembourg: imports
  "MT",  389,   # Malta: gas (interconnector)
  "NL",  307,   # Netherlands: gas + wind
  "PL",  663,   # Poland: coal-dominant
  "PT",  148,   # Portugal: renewables + gas
  "RO",  248,   # Romania: hydro + nuclear + coal
  "SK",  107,   # Slovakia: nuclear + hydro
  "SI",  215,   # Slovenia: nuclear + coal + hydro
  "ES",  142,   # Spain: renewables + gas
  "SE",    8    # Sweden: hydro + nuclear + wind
)

cat("Grid emission factors loaded for", nrow(grid_ef), "countries\n")
cat("Range:", min(grid_ef$Grid_EF_gCO2_kWh), "-", max(grid_ef$Grid_EF_gCO2_kWh), "gCO2/kWh\n")

# ── 2) Download electricity consumption by industrial subsector ──
eu27 <- grid_ef$Country_ID

cat("\nDownloading electricity consumption from Eurostat (nrg_bal_c)...\n")
elec_raw <- restatapi::get_eurostat_data(
  id        = "nrg_bal_c",
  filters   = list(siec = "E7000", unit = "GWH", geo = eu27),
  date_filter = 2022,
  exact_match = FALSE,
  label       = FALSE,
  cflags      = TRUE,
  keep_flags  = TRUE
)

# Filter to industrial subsectors
ind_codes <- c("FC_IND_FBT_E", "FC_IND_TL_E", "FC_IND_WP_E", "FC_IND_PPP_E",
               "FC_IND_CPC_E", "FC_IND_IS_E", "FC_IND_NFM_E", "FC_IND_NMM_E",
               "FC_IND_MAC_E", "FC_IND_TE_E", "FC_IND_NSP_E", "FC_IND_E")

elec <- elec_raw %>%
  filter(nrg_bal %in% ind_codes) %>%
  mutate(
    Country_ID = as.character(geo),
    Elec_GWh   = as.numeric(values)
  ) %>%
  select(Country_ID, nrg_bal, Elec_GWh)

cat("Electricity data: ", nrow(elec), "rows for", length(unique(elec$Country_ID)), "countries\n")

# ── 3) Map energy balance codes to aggregated sectors ────────────
# Same mapping as Create-Energy_Totals_and_Shares.R
sector_map <- tibble::tribble(
  ~nrg_bal,        ~Sector_ID,
  "FC_IND_FBT_E",  "C10-C12",    # Food, beverage, tobacco
  "FC_IND_TL_E",   "C13-C15",    # Textiles, leather
  "FC_IND_WP_E",   "C16-C18",    # Wood, paper (partial)
  "FC_IND_PPP_E",  "C16-C18",    # Pulp, paper, printing
  "FC_IND_CPC_E",  "C19-C20",    # Chemical, petrochemical
  "FC_IND_IS_E",   "C24",        # Iron & steel
  "FC_IND_NFM_E",  "C24",        # Non-ferrous metals -> also C24
  "FC_IND_NMM_E",  "C23",        # Non-metallic minerals
  "FC_IND_MAC_E",  "C25+C28-C30",# Machinery
  "FC_IND_TE_E",   "C25+C28-C30",# Transport equipment
  "FC_IND_E",      "C"           # Total industry
)

# Note: FC_IND_NSP_E (not specified) needs to be distributed
# across sectors that don't have explicit codes

elec_mapped <- elec %>%
  inner_join(sector_map, by = "nrg_bal") %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(Elec_GWh = sum(Elec_GWh, na.rm = TRUE), .groups = "drop")

# Handle sectors without explicit energy codes:
# C21-C22 (Pharma/Rubber), C26-C27 (Electronics), C31-C33 (Other)
# Distribute FC_IND_NSP_E equally among unmapped sectors
nsp <- elec %>%
  filter(nrg_bal == "FC_IND_NSP_E") %>%
  select(Country_ID, NSP_GWh = Elec_GWh)

unmapped_sectors <- c("C21-C22", "C26-C27", "C31-C33")
n_unmapped <- length(unmapped_sectors)

nsp_split <- nsp %>%
  tidyr::crossing(Sector_ID = unmapped_sectors) %>%
  mutate(Elec_GWh = NSP_GWh / n_unmapped) %>%
  select(Country_ID, Sector_ID, Elec_GWh)

elec_all <- bind_rows(elec_mapped, nsp_split)

cat("Sectors with electricity data:", paste(sort(unique(elec_all$Sector_ID)), collapse = ", "), "\n")

# ── 4) Compute Scope 2 emissions ─────────────────────────────────
# Scope2 = Electricity_GWh * Grid_EF_gCO2_kWh * 1000 (GWh->MWh) / 1e6 (g->tonnes)
# Simplifies to: Scope2_tCO2 = Elec_GWh * Grid_EF_gCO2_kWh * 1000 / 1e6
#              = Elec_GWh * Grid_EF_gCO2_kWh / 1000

scope2 <- elec_all %>%
  inner_join(grid_ef, by = "Country_ID") %>%
  mutate(
    Scope2_tCO2 = Elec_GWh * Grid_EF_gCO2_kWh / 1000  # tonnes CO2
  )

cat("\nScope 2 emissions computed.\n")
cat("Total Scope 2 by country (top 5, tCO2, aggregate C):\n")
scope2 %>%
  filter(Sector_ID == "C") %>%
  arrange(desc(Scope2_tCO2)) %>%
  head(5) %>%
  select(Country_ID, Elec_GWh, Grid_EF_gCO2_kWh, Scope2_tCO2) %>%
  print()

# ── 5) Downscale to NUTS-2 using employment weights ─────────────
# Use the same approach as Create-Exposure_Emissions.R
# Need: N_Enterpr or EMPL_Region for weights

# Use EMPL_Region shares as proxy weights
empl <- read_xlsx("Code and data/Initial data/Sector data/EMPL_Region.xlsx") %>%
  select(NUTS_ID, Sector_ID, Share = Value) %>%
  mutate(Country_ID = substr(NUTS_ID, 1, 2))

# For sectors not in EMPL (e.g., "C"), use equal distribution
nuts2_list <- empl %>% distinct(NUTS_ID, Country_ID)

# Compute share of total employment (for sector C allocation)
empl_total <- empl %>%
  group_by(NUTS_ID) %>%
  summarise(Total_Share = sum(Share, na.rm = TRUE), .groups = "drop")

# For sector C: use total employment share across regions
empl_C <- empl_total %>%
  mutate(
    Country_ID = substr(NUTS_ID, 1, 2),
    Sector_ID  = "C"
  ) %>%
  group_by(Country_ID) %>%
  mutate(Share = Total_Share / sum(Total_Share, na.rm = TRUE)) %>%
  ungroup() %>%
  select(NUTS_ID, Sector_ID, Share, Country_ID)

empl_weights <- bind_rows(
  empl %>% group_by(Country_ID, Sector_ID) %>%
    mutate(Share = Share / sum(Share, na.rm = TRUE)) %>%
    ungroup(),
  empl_C
)

# Downscale national Scope 2 to regions
scope2_regional <- scope2 %>%
  select(Country_ID, Sector_ID, Scope2_tCO2) %>%
  left_join(empl_weights, by = c("Country_ID", "Sector_ID")) %>%
  mutate(
    Value = Scope2_tCO2 * Share
  ) %>%
  filter(!is.na(NUTS_ID), !is.na(Value))

cat("\nRegional Scope 2: ", nrow(scope2_regional), "rows\n")

# ── 6) Format output ─────────────────────────────────────────────
scope2_out <- scope2_regional %>%
  transmute(
    Country_ID = Country_ID,
    NUTS_ID    = NUTS_ID,
    Sector_ID  = Sector_ID,
    Indicator  = "Scope2_Emissions",
    Unit       = "tCO2eq",
    Value      = round(Value, 2)
  )

cat("Output rows:", nrow(scope2_out), "\n")
cat("Sectors:", paste(sort(unique(scope2_out$Sector_ID)), collapse = ", "), "\n")
cat("Regions:", length(unique(scope2_out$NUTS_ID)), "\n")

# Verification: regional sums should approximate national totals
check <- scope2_out %>%
  group_by(Country_ID, Sector_ID) %>%
  summarise(reg_sum = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  inner_join(scope2 %>% select(Country_ID, Sector_ID, Scope2_tCO2),
             by = c("Country_ID", "Sector_ID")) %>%
  mutate(diff_pct = abs(reg_sum - Scope2_tCO2) / Scope2_tCO2 * 100)

cat("\nVerification - max % difference national vs sum(regional):",
    round(max(check$diff_pct, na.rm = TRUE), 2), "%\n")

# ── 7) Save ──────────────────────────────────────────────────────
writexl::write_xlsx(
  scope2_out,
  "Code and data/Initial data/Sector data/EXP-Scope2_Emissions.xlsx"
)
cat("Saved: Code and data/Initial data/Sector data/EXP-Scope2_Emissions.xlsx\n")
