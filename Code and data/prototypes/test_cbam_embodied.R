# test_cbam_embodied.R — validate the embodied CBAM-intensity variant (REVISION §23).
# Compares the national (country x sector) CBAM leg under the headline DIRECT
# (Scope-1) intensity vs the full EMBODIED footprint f^T(I-A)^-1. Downscaling to
# NUTS-2 preserves national totals and uses identical weights for both, so the
# country x sector comparison isolates the intensity effect.
# Run from "Code and data/":  Rscript prototypes/test_cbam_embodied.R
suppressMessages({library(dplyr); library(Matrix)})
source("R/utils.R"); source("R/exposure.R")

paths <- figaro_cache_files()
io  <- readRDS(paths[["io"]]); ghg <- readRDS(paths[["ghg"]])
cat("FIGARO IO:", basename(paths[["io"]]), "| GHG:", basename(paths[["ghg"]]), "\n")

eu27 <- .eu27_codes(); goods <- c("C20","C23","C24"); nm <- .figaro_nace_map()
io  <- io  |> mutate(across(c(c_orig, c_dest, ind_ava, ind_use), as.character))
ghg <- ghg |> mutate(across(c(c_orig, nace_r2), as.character))
nonEU <- setdiff(unique(io$c_orig), c(eu27, "DOM"))

# Direct (Scope-1) intensity, tCO2/MEUR, per (origin, covered good)
out <- io  |> group_by(c_orig, ind_ava) |> summarise(output = sum(values, na.rm = TRUE), .groups = "drop")
em  <- ghg |> group_by(c_orig, nace_r2) |> summarise(emis_kt = sum(values, na.rm = TRUE), .groups = "drop")
f_direct <- out |> filter(c_orig %in% nonEU, ind_ava %in% goods) |>
  left_join(em, by = c("c_orig", "ind_ava" = "nace_r2")) |>
  mutate(f = if_else(output > 0, (emis_kt * 1000) / output, 0)) |>
  select(c_orig, ind_ava, f)

# Embodied (full footprint) intensity from the new engine helper
cat("Building Leontief inverse for embodied intensity ...\n")
f_emb <- .figaro_embodied_intensity(io, ghg) |> filter(c_orig %in% nonEU, ind_ava %in% goods)

# National CBAM leg (tCO2) under each intensity, charged to the using sector
nat <- function(f_tab) {
  io |> filter(c_orig %in% nonEU, ind_ava %in% goods,
               c_dest %in% eu27, ind_use %in% nm$ind, values > 0) |>
    left_join(f_tab, by = c("c_orig", "ind_ava")) |>
    left_join(nm, by = c("ind_use" = "ind")) |>
    group_by(Country_ID = c_dest, Sector_ID) |>
    summarise(t = sum(values * f, na.rm = TRUE), .groups = "drop")
}
cmp <- full_join(nat(f_direct) |> rename(direct = t),
                 nat(f_emb)    |> rename(embodied = t),
                 by = c("Country_ID", "Sector_ID")) |>
  mutate(across(c(direct, embodied), ~ coalesce(., 0)))

cat(sprintf("\nDIRECT   total: %6.1f Mt  (sanity: design log = 259 Mt)\n", sum(cmp$direct)/1e6))
cat(sprintf("EMBODIED total: %6.1f Mt  (x%.2f vs direct)\n",
            sum(cmp$embodied)/1e6, sum(cmp$embodied)/sum(cmp$direct)))
cat(sprintf("Spearman(direct, embodied) over %d country x sector cells: %.3f\n",
            nrow(cmp), cor(cmp$direct, cmp$embodied, method = "spearman")))
cat(sprintf("Pearson  (direct, embodied): %.3f\n",
            cor(cmp$direct, cmp$embodied, method = "pearson")))

cat("\nPer-sector totals (Mt) and embodied/direct ratio:\n")
print(cmp |> group_by(Sector_ID) |>
        summarise(direct_Mt = round(sum(direct)/1e6, 1),
                  embodied_Mt = round(sum(embodied)/1e6, 1),
                  ratio = round(sum(embodied)/pmax(sum(direct), 1e-9), 2)) |>
        arrange(desc(embodied_Mt)), n = 20)
