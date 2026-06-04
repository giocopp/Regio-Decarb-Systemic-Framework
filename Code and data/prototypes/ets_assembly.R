suppressMessages({library(dplyr); library(tidyr); library(readxl); library(targets)})
source("R/utils.R"); source("R/exposure_cost.R")

# ── Crosswalk: EEA process activity code -> 12 NACE manufacturing sector ──
# Standard EU ETS Annex I activity types, CORROBORATED by 2024 magnitudes/
# free-alloc patterns. Codes 20 (combustion/power), 10/50 (aviation), 45 (CCS),
# 99 (other) deliberately excluded (not manufacturing process).
act_map <- tibble::tribble(
  ~code, ~Sector_ID,
  "35","C16-C18","36","C16-C18",                                   # pulp, paper
  "21","C19-C20","22","C19-C20","37","C19-C20","38","C19-C20",     # refining, coke, carbon black, nitric
  "39","C19-C20","40","C19-C20","41","C19-C20","42","C19-C20",     # adipic, glyoxal, ammonia, bulk chem
  "43","C19-C20","44","C19-C20",                                   # hydrogen, soda ash
  "29","C23","30","C23","31","C23","32","C23","33","C23","34","C23", # cement,lime,glass,ceramics,wool,gypsum
  "23","C24","24","C24","25","C24","26","C24","27","C24","28","C24") # metal ore,steel,ferrous,alu x2,non-ferrous

# ── Country x Sector free_alloc_share from EEA 2024 (GR->EL) ──
D <- "Initial data/eea_t_eu-emission-trading-scheme_p_2005-2025_v01_r00"
fa <- read_excel(file.path(D,"ETS_Database_April_2026.xlsx"), sheet="Sheet1") |>
  mutate(country_code = if_else(country_code=="GR","EL",country_code)) |>
  filter(year=="2024", country_code %in% .eu27_codes(),
         citl_information %in% c("1.1 Freely allocated allowances","2. Verified emissions")) |>
  inner_join(act_map, by=c("main_activity_code"="code")) |>
  mutate(var = if_else(citl_information=="2. Verified emissions","verified","freealloc")) |>
  group_by(Country_ID=country_code, Sector_ID, var) |>
  summarise(v=sum(value,na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from=var, values_from=v, values_fill=0) |>
  mutate(free_alloc_share = if_else(verified>0, pmin(freealloc/verified, 1), NA_real_))
cat("country x sector free_alloc_share cells:", nrow(fa), "\n")
cat("EU-level mean free_alloc_share by sector:\n")
print(fa |> group_by(Sector_ID) |>
      summarise(mean_share=round(mean(free_alloc_share,na.rm=TRUE),3),
                n_countries=n(), verified_Mt=round(sum(verified)/1e6,1)))

# ── Assemble ──
s1 <- tar_read(scope1_data) |> select(Country_ID, NUTS_ID, Sector_ID, Scope1_kt = Value)
io  <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg <- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
cbam <- compute_cbam_leg(io, ghg, tar_read(empl_weights)) |>
  select(NUTS_ID, Sector_ID, CBAM_emb_tCO2)
cbam_cov <- tibble::tibble(Sector_ID=c("C19-C20","C23","C24"), cbam_cov=1)

df <- s1 |>
  left_join(fa |> select(Country_ID, Sector_ID, free_alloc_share),
            by=c("Country_ID","Sector_ID")) |>
  left_join(cbam, by=c("NUTS_ID","Sector_ID")) |>
  left_join(cbam_cov, by="Sector_ID") |>
  mutate(free_alloc_share = coalesce(free_alloc_share, 1),   # no process coverage -> no ETS cost
         CBAM_emb_tCO2 = coalesce(CBAM_emb_tCO2, 0),
         cbam_cov = coalesce(cbam_cov, 0))

expo <- assemble_exposure_cost(df, eua_price = 1)   # EUA scalar irrelevant to ranking

cat("\n=== new pooled Exposure: rows", nrow(expo), "NAs", sum(is.na(expo$Exposure)), "===\n")
cat("share of ETS cost vs CBAM cost (EU totals):\n")
cat("  ETS_cost total:", round(sum(expo$ETS_cost)/1e9,2), "bn (t*share-units) | CBAM_cost total:",
    round(sum(expo$CBAM_cost)/1e9,2), "bn\n")

# Compare to baseline exposure
base <- read.csv("Final data/Risk_data.csv") |> select(NUTS_ID, Sector_ID, Exposure_base=Exposure)
cmp <- expo |> inner_join(base, by=c("NUTS_ID","Sector_ID")) |>
  filter(!is.na(Exposure_base) & !is.na(Exposure))
cat("\nSpearman(new pooled Exposure, baseline within-sector Exposure):",
    round(cor(cmp$Exposure, cmp$Exposure_base, method="spearman"),3), "\n")

cat("\nTop 12 cells by NEW exposure:\n")
print(expo |> arrange(desc(Exposure)) |> head(12) |>
      mutate(across(c(Scope1_kt,CBAM_emb_tCO2,free_alloc_share,Exposure), ~round(.,3))) |>
      select(NUTS_ID, Sector_ID, Scope1_kt, free_alloc_share, CBAM_emb_tCO2, Exposure))
