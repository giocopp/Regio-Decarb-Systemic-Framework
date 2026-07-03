# diversification_hhi_prototype.R — regional manufacturing HHI as a SECOND
# indicator for the Diversification dimension of Vulnerability.
#
# Motivation: the Diversification pillar currently rests on a SINGLE indicator
# (Sector_Concentration = the focal sector's share of the region's manufacturing
# employment; sector-specific). A single-indicator pillar is the weakness the JRC
# Handbook flags and the same objection used to drop the Finance dimension
# (METHODOLOGY §11.1). This adds a region-level companion: the Herfindahl-
# Hirschman index of manufacturing employment across the 11 NACE sub-sectors —
# "how narrow is this region's manufacturing base overall."
#
#   HHI[r] = sum_s ( emp[r,s] / emp[r,C] )^2  =  sum_s Sector_Concentration[r,s]^2
#
# so HHI is the region-level aggregate of the existing indicator — consistent by
# construction, Eurostat-only (sbs_r_nuts2021 via empl_weights), no scope creep.
# Higher HHI = more concentrated = LESS diversified = MORE vulnerable (positive
# orientation, like Sector_Concentration; NOT reversed).
#
# STATUS: prototype — NOT wired into the index. Sizes the indicator, its spread,
# and how much a 2-indicator Diversification pillar would move vs the current
# single indicator. Run from "Code and data/":
#   Rscript prototypes/diversification_hhi_prototype.R

suppressMessages({library(targets); library(dplyr); library(tidyr)})
source("R/utils.R")   # range01, excluded_nuts, recombine_empl_nuts

# empl_weights already carries pers_employed by NUTS-2 x {C, 11 sub-sectors};
# recombine HR/NL/PT and drop ultraperipheral NUTS to match the index grid.
ew <- tar_read(empl_weights) |>
  recombine_empl_nuts() |>
  filter(!NUTS_ID %in% excluded_nuts)

empC <- ew |> filter(Sector_ID == "C") |> transmute(NUTS_ID, empC = pers_employed)

# per-cell focal-sector share (= the existing Sector_Concentration, by construction)
cell <- ew |>
  filter(Sector_ID != "C", !is.na(pers_employed)) |>
  inner_join(empC, by = "NUTS_ID") |>
  filter(empC > 0) |>
  transmute(Country_ID, NUTS_ID, Sector_ID, pers_employed, empC,
            SectorConc = pers_employed / empC)

# ---- regional HHI across the 11 sub-sectors ----
reg <- cell |>
  group_by(Country_ID, NUTS_ID) |>
  summarise(empC    = first(empC),
            covered = sum(pers_employed) / first(empC),   # share of C in observed subs
            n_sub   = sum(pers_employed > 0),
            HHI     = sum(SectorConc^2),
            .groups = "drop")

cat(sprintf("Regions: %d   |   HHI floor (even 11-way split) = %.3f\n",
            nrow(reg), 1/11))
cat(sprintf("HHI:  min %.3f | median %.3f | mean %.3f | max %.3f\n",
            min(reg$HHI), median(reg$HHI), mean(reg$HHI), max(reg$HHI)))
cat(sprintf("Coverage (sum of subs / C):  median %.2f  (low => sectors suppressed)\n",
            median(reg$covered)))

cat("\nMOST concentrated  (least diversified -> most vulnerable):\n")
print(reg |> arrange(desc(HHI)) |> head(8) |>
        transmute(NUTS_ID, Country_ID, n_sub, empC, HHI = round(HHI, 3)) |>
        as.data.frame(), row.names = FALSE)
cat("\nMOST diversified  (lowest HHI):\n")
print(reg |> arrange(HHI) |> head(8) |>
        transmute(NUTS_ID, Country_ID, n_sub, empC, HHI = round(HHI, 3)) |>
        as.data.frame(), row.names = FALSE)

# ---- effect on the Diversification pillar: 1-indicator vs 2-indicator ----
# pooled min-max, positive orientation (higher raw -> higher vulnerability)
piv <- cell |>
  inner_join(reg |> select(NUTS_ID, HHI), by = "NUTS_ID") |>
  mutate(SC_n    = range01(SectorConc, preserve_zeros = FALSE),
         HHI_n   = range01(HHI,        preserve_zeros = FALSE),
         Div_old = SC_n,                       # current single-indicator pillar
         Div_new = 0.5 * SC_n + 0.5 * HHI_n,   # 2-indicator pillar
         delta   = Div_new - Div_old)

rho_div  <- cor(piv$Div_old, piv$Div_new, method = "spearman")
rho_pair <- cor(piv$SectorConc, piv$HHI,  method = "spearman")
cat(sprintf("\nSector_Concentration vs HHI (per cell):           Spearman %.3f\n", rho_pair))
cat(sprintf("Diversification pillar  old(1-ind) vs new(2-ind): Spearman %.3f  (cells=%d)\n",
            rho_div, nrow(piv)))

cat("\nCells the HHI companion pushes UP most",
    "(small own-share, but the region is narrow overall):\n")
print(piv |> arrange(desc(delta)) |> head(8) |>
        transmute(NUTS_ID, Sector_ID,
                  SectorConc = round(SectorConc, 3), HHI = round(HHI, 3),
                  Div_old = round(Div_old, 2), Div_new = round(Div_new, 2),
                  delta = round(delta, 2)) |> as.data.frame(), row.names = FALSE)

out <- piv |>
  transmute(Country_ID, NUTS_ID, Sector_ID,
            SectorConc = round(SectorConc, 4), HHI = round(HHI, 4),
            SC_n = round(SC_n, 3), HHI_n = round(HHI_n, 3),
            Div_old = round(Div_old, 3), Div_new = round(Div_new, 3))
write.csv(out, "prototypes/diversification_hhi_output.csv", row.names = FALSE)
cat(sprintf("\nWrote prototypes/diversification_hhi_output.csv (%d rows)\n", nrow(out)))
