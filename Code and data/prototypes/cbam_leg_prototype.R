#!/usr/bin/env Rscript
# ─────────────────────────────────────────────────────────────────────────────
# PROTOTYPE (not yet wired into _targets.R) — CBAM-leg quantity for the
# carbon-cost-at-risk exposure. Computes the embodied (direct) carbon in
# extra-EU imports of CBAM-covered goods, by EU country x manufacturing
# using-sector, from the cached FIGARO 2023 tables (io + ghg).
#
# Interpretation: CBAM as an INPUT COST — sector j's exposure = embodied carbon
# in j's imported covered goods (excludes the producer-protection channel by
# design, matching the Export_ExtraEU direction decision).
#
# VERIFIED RESULT (FIGARO 2023): total = 258.6 Mt CO2; sector totals led by
# C19-C20 (60 Mt), C24 (50 Mt), C25+C28 (38 Mt), C21-C22 (30 Mt). Top cells
# IT-C24, DE-C19-C20, DE-C24, IT-C25+C28 — economically sensible.
#
# CAVEATS (to address before publication):
#  1. FIGARO 2-digit > exact CBAM goods: covered_goods = C20 (ALL chemicals,
#     not only fertilizers/hydrogen), C23 (ALL non-metallic minerals, not only
#     cement), C24 (ALL basic metals, not only steel/aluminium). => OVER-counts;
#     258 Mt is an upper bound. Precise scope needs CN-code trade (Comext) -> a
#     later refinement (analogous to raw-EUTL for the ETS leg).
#  2. DIRECT emission intensity only (Scope-1 embedded). CBAM also prices some
#     indirect/precursor emissions; a Scope-1&2 variant is possible (cf. Clora
#     et al. 2023, Energy Policy).
#  3. 12 origin x good intensities missing in env_ac_ghgfp -> those flows are
#     dropped (na.rm). Minor; could be imputed from a world-average intensity.
#  4. COUNTRY level here; NUTS-2 downscaling reuses downscale_national_to_nuts2()
#     (employment-share), exactly as create_scope3().
#  5. Vintage: FIGARO 2023 (latest cache). EUA price / free allocation target
#     2024/25 per project decision -> document the mismatch.
# ─────────────────────────────────────────────────────────────────────────────
suppressMessages(library(dplyr))

io  <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg <- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")

eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR",
          "HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")
stopifnot(length(eu27) == 27)
nonEU <- setdiff(unique(as.character(io$c_orig)), c(eu27, "DOM"))

covered_goods <- c("C20","C23","C24")   # see caveat 1

nace_map <- tibble::tribble(
  ~ind,     ~Sector_ID,
  "C10-12","C10-C12","C13-15","C13-C15",
  "C16","C16-C18","C17","C16-C18","C18","C16-C18",
  "C19","C19-C20","C20","C19-C20",
  "C21","C21-C22","C22","C21-C22",
  "C23","C23","C24","C24",
  "C25","C25+C28","C28","C25+C28",
  "C29","C29-C30","C30","C29-C30",
  "C26","C26-C27","C27","C26-C27",
  "C31_32","C31-C33","C33","C31-C33")

output <- io |> group_by(c_orig, ind_ava) |>
  summarise(output = sum(values, na.rm = TRUE), .groups = "drop")
emis <- ghg |> group_by(c_orig, nace_r2) |>
  summarise(emis_kt = sum(values, na.rm = TRUE), .groups = "drop")

f_tab <- output |>
  filter(c_orig %in% nonEU, ind_ava %in% covered_goods) |>
  left_join(emis, by = c("c_orig","ind_ava" = "nace_r2")) |>
  mutate(f = if_else(output > 0, (emis_kt * 1000) / output, 0)) |>
  select(c_orig, ind_ava, f)

imp <- io |>
  filter(c_orig %in% nonEU, ind_ava %in% covered_goods,
         c_dest %in% eu27, ind_use %in% nace_map$ind, values > 0) |>
  left_join(f_tab, by = c("c_orig","ind_ava")) |>
  mutate(emb_tCO2 = values * f)

cbam <- imp |> left_join(nace_map, by = c("ind_use" = "ind")) |>
  group_by(Country_ID = c_dest, Sector_ID) |>
  summarise(CBAM_emb_tCO2 = sum(emb_tCO2, na.rm = TRUE),
            import_MEUR   = sum(values,   na.rm = TRUE), .groups = "drop")
cbam <- bind_rows(cbam, cbam |> group_by(Country_ID) |>
  summarise(Sector_ID = "C", CBAM_emb_tCO2 = sum(CBAM_emb_tCO2),
            import_MEUR = sum(import_MEUR), .groups = "drop"))

# NEXT: downscale (Country_ID x Sector_ID) -> NUTS-2 via
# downscale_national_to_nuts2(cbam, empl_weights, "CBAM_emb_tCO2").
cat("CBAM leg: ", nrow(cbam), " country x sector cells; total ",
    round(sum(cbam$CBAM_emb_tCO2[cbam$Sector_ID!="C"])/1e6,1), " Mt CO2\n", sep="")
