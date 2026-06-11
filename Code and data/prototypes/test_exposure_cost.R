suppressMessages({library(dplyr); library(targets)})
source("R/utils.R"); source("R/exposure_cost.R")

cat("######## (2) CBAM leg -> NUTS-2 (real FIGARO 2023) ########\n")
io  <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg <- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
ew  <- tar_read(empl_weights)
cbam <- compute_cbam_leg(io, ghg, ew)
cat("rows:", nrow(cbam), "| distinct NUTS-2:", dplyr::n_distinct(cbam$NUTS_ID),
    "| sectors:", dplyr::n_distinct(cbam$Sector_ID), "\n")
cat("NAs:", sum(is.na(cbam$CBAM_emb_tCO2)),
    "| total Mt (excl C):", round(sum(cbam$CBAM_emb_tCO2[cbam$Sector_ID!='C'])/1e6,1), "\n")
cat("national total preserved by downscale? (should ~match 258.6 Mt country-level)\n")
cat("top NUTS-2 x sector cells:\n")
print(cbam |> filter(Sector_ID!="C") |> arrange(desc(CBAM_emb_tCO2)) |> head(6) |>
      mutate(kt=round(CBAM_emb_tCO2/1000,1)) |> select(NUTS_ID, Sector_ID, kt))

cat("\n######## (1) Engine: pooled vs within-sector (synthetic) ########\n")
# 3 sectors x 4 regions. Scope1 varies by region; free_alloc_share varies by
# SECTOR (A=0.9 -> low ETS price; C=0.1 -> high). CBAM set to 0 to isolate ETS leg.
syn <- expand.grid(Sector_ID = c("A","B","C"), reg = 1:4,
                   stringsAsFactors = FALSE) |>
  mutate(NUTS_ID = paste0(Sector_ID, reg), Country_ID = "XX",
         ets_emis_t = 100*reg + 10, CBAM_emb_tCO2 = 0, cbam_cov = 0,
         free_alloc_share = c(A=0.9, B=0.5, C=0.1)[Sector_ID])
syn_uniform <- syn |> mutate(free_alloc_share = 0.5)   # price made sector-flat

rs <- function(a, b) cor(round(a, 9), round(b, 9), method = "spearman")  # round: kill FP tie noise
eA <- assemble_exposure_cost(syn)$Exposure          # POOLED, real (sector-varying) price
eU <- assemble_exposure_cost(syn_uniform)$Exposure  # POOLED, uniform price
cat("POOLED  Spearman(real-price vs uniform-price):", round(rs(eA, eU), 4),
    " -> <1 means PRICE MATTERS\n")

wA <- assemble_exposure_cost(syn,         within_sector=TRUE)$Exposure
wU <- assemble_exposure_cost(syn_uniform, within_sector=TRUE)$Exposure
cat("WITHIN  Spearman(real-price vs uniform-price):", round(rs(wA, wU), 4),
    " | max|diff|:", signif(max(abs(wA-wU)),3),
    " -> =1 / 0 means price WASHES OUT\n")

cat("\nIllustration (pooled exposure, real price) — high-free-alloc sector A should rank below low-free-alloc C at equal emissions:\n")
print(assemble_exposure_cost(syn) |> arrange(desc(Exposure)) |>
      select(Sector_ID, reg, ets_emis_t, free_alloc_share, P_ETS, Exposure) |> head(6))
