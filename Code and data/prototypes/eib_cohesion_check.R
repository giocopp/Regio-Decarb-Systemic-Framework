# eib_cohesion_check.R — evaluation of the EIB Group cohesion dataset as a
# candidate access-to-finance indicator (checked 2026-07-07, suggested by
# supervisor). VERDICT: NOT USABLE for the index; see below.
#
# Source: https://www.eib.org/en/projects/topics/regional-development/impact
# ("Cohesion Report Data 2021-2024"). The page charts are Datawrapper embeds;
# their underlying data is public at
#   https://datawrapper.dwcdn.net/{chart_id}/dataset.csv
# Regional table: chart rW8LO/1 -> eib_cohesion_data/eib_regional_signed_2021_2024.tsv
#   (236 NUTS-2 rows: EIB financing signed 2021-2024 EURM, cohesion category,
#    2024 population, 2023 GDP, intensity ratios)
# Group totals:   chart l2MX5/12 -> eib_cohesion_data/eib_group_totals.tsv
#   (EIB cohesion 108.4bn / non-cohesion 133.6bn; EIF 32/33bn)
# Snapshots retrieved 2026-07-07; TSV as served by Datawrapper.
#
# FINDINGS (reproduced by this script):
# 1. Amounts exist ONLY for the 141 cohesion regions (76 less developed + 65
#    transition, EUR 92.5bn total — below the 108.4bn EIB cohesion total, so
#    part of the lending is not regionally attributed; no method note
#    published). The ~95 more-developed rows are empty -> blank by
#    construction for ~40% of the index grid, incl. most top-risk regions.
# 2. Construct: measures policy finance RECEIVED, not access to finance —
#    the direction problem for which Cohesion_Fund is excluded
#    (METHODOLOGY §11); EIB cohesion targeting follows GDP/capita ->
#    mechanical entanglement with the Vulnerability side.
# 3. Empirics on the covered subset: signed-per-capita vs our index:
#    spearman ~ +0.20 (Exposure), -0.12 (Vulnerability), +0.11 (Risk) —
#    and less-developed regions receive LESS per head (~364 EUR) than
#    transition regions (~426 EUR).
# Also rejected as a discussion figure: top-risk regions are mostly outside
# the covered subset, and the cohesion mandate targets development status by
# design, so "lending does not track transition risk" would be true by
# construction.
#
# Run from "Code and data/":  Rscript prototypes/eib_cohesion_check.R

suppressMessages({library(dplyr); library(readr)})

eib <- read_tsv("prototypes/eib_cohesion_data/eib_regional_signed_2021_2024.tsv",
                show_col_types = FALSE)
names(eib) <- c("NUTS_ID","signed_meur","name","type","pop","gdp",
                "signed_pc","signed_gdp")

cat("rows:", nrow(eib),
    "| with amounts:", sum(!is.na(eib$signed_meur)),
    "| sum signed (bn):", round(sum(eib$signed_meur, na.rm = TRUE)/1e3, 1), "\n")
print(eib |> group_by(type) |>
        summarise(n = n(), with_amount = sum(!is.na(signed_meur)),
                  eur_pc = round(mean(1e6*signed_meur/pop, na.rm = TRUE))) |>
        as.data.frame())

rd <- read.csv("Final data/Risk_data.csv") |>
  filter(Sector_ID == "C") |>
  select(NUTS_ID, Exposure, Vulnerability, Risk_norm)
j <- eib |> inner_join(rd, by = "NUTS_ID") |>
  mutate(int_pc = 1e6*signed_meur/pop)
cat("matched to index grid:", nrow(j), "\n")
for (v in c("Exposure","Vulnerability","Risk_norm"))
  cat(sprintf("spearman(EIB signed per capita, %s) = %+.2f\n",
              v, cor(j$int_pc, j[[v]], method = "spearman", use = "pair")))
