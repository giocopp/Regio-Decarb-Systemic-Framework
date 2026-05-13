# ── compare_scope3.R ── Side-by-side comparison of Scope 3 variants ─────────
#
# Compares the two Scope 3 indicators currently sitting in Initial data/:
#   - EXP-Scope3_Emissions.xlsx (consumption-side, c_orig != X foreign-embedded)
#   - EXP-Scope3_Upstream.xlsx  (producer-side, MRIO Leontief excl. D35)
#
# Reports: magnitudes, sector rankings, country totals, rank correlations.

suppressMessages({
  library(dplyr); library(readxl); library(tibble); library(tidyr)
})

cons <- read_xlsx("Initial data/Sector data/EXP-Scope3_Emissions.xlsx") |>
  rename(Cons_tCO2 = Value)
prod <- read_xlsx("Initial data/Sector data/EXP-Scope3_Upstream.xlsx") |>
  rename(Prod_tCO2 = Value)

# Join on NUTS_ID + Sector_ID
both <- cons |>
  select(Country_ID, NUTS_ID, Sector_ID, Cons_tCO2) |>
  inner_join(prod |> select(NUTS_ID, Sector_ID, Prod_tCO2),
             by = c("NUTS_ID", "Sector_ID"))

cat("=== Coverage ===\n")
cat(sprintf("Consumption rows: %d, Producer rows: %d, joined: %d\n",
            nrow(cons), nrow(prod), nrow(both)))

cat("\n=== Magnitudes (Mt CO2eq, summed across NUTS-2) ===\n")
mag <- both |>
  group_by(Sector_ID) |>
  summarise(Cons_Mt = sum(Cons_tCO2, na.rm = TRUE) / 1e6,
            Prod_Mt = sum(Prod_tCO2, na.rm = TRUE) / 1e6,
            Ratio = round(Prod_Mt / Cons_Mt, 2),
            .groups = "drop")
print(mag)

cat("\n=== Rank correlation (within each sector) ===\n")
ranks <- both |>
  group_by(Sector_ID) |>
  summarise(rho = cor(Cons_tCO2, Prod_tCO2, method = "spearman",
                       use = "complete.obs"),
            n = sum(!is.na(Cons_tCO2) & !is.na(Prod_tCO2)),
            .groups = "drop")
print(ranks)

cat("\n=== Top-10 NUTS-2 by each indicator (sector C aggregate) ===\n")
top10_cons <- both |> filter(Sector_ID == "C") |>
  arrange(desc(Cons_tCO2)) |> head(10) |>
  mutate(Cons_Mt = round(Cons_tCO2/1e6,2)) |>
  select(NUTS_ID, Cons_Mt)
top10_prod <- both |> filter(Sector_ID == "C") |>
  arrange(desc(Prod_tCO2)) |> head(10) |>
  mutate(Prod_Mt = round(Prod_tCO2/1e6,2)) |>
  select(NUTS_ID, Prod_Mt)
cat("By consumption-side:\n"); print(top10_cons)
cat("By producer-side:\n"); print(top10_prod)

cat("\n=== Country totals (Mt) ===\n")
cnt <- both |> filter(Sector_ID == "C") |>
  group_by(Country_ID) |>
  summarise(Cons_Mt = round(sum(Cons_tCO2)/1e6, 2),
            Prod_Mt = round(sum(Prod_tCO2)/1e6, 2),
            Ratio = round(Prod_Mt / Cons_Mt, 2),
            .groups = "drop") |>
  arrange(desc(Prod_Mt))
print(cnt, n = 30)
