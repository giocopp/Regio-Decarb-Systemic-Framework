suppressMessages({library(dplyr); library(tidyr); library(readxl); library(targets)})
source("R/utils.R"); source("R/04_normalize.R"); source("R/05_aggregate.R")
source("R/exposure_cost.R")

ETS_XLSX <- "Initial data/eea_t_eu-emission-trading-scheme_p_2005-2025_v01_r00/ETS_Database_April_2026.xlsx"
act_map <- tibble::tribble(~code,~Sector_ID,
  "35","C16-C18","36","C16-C18",
  "21","C19-C20","22","C19-C20","37","C19-C20","38","C19-C20","39","C19-C20",
  "40","C19-C20","41","C19-C20","42","C19-C20","43","C19-C20","44","C19-C20",
  "29","C23","30","C23","31","C23","32","C23","33","C23","34","C23",
  "23","C24","24","C24","25","C24","26","C24","27","C24","28","C24")

compute_fa <- function(mult = 1) {
  read_excel(ETS_XLSX, sheet="Sheet1") |>
    mutate(country_code = if_else(country_code=="GR","EL",country_code)) |>
    filter(year=="2024", country_code %in% .eu27_codes(),
           citl_information %in% c("1.1 Freely allocated allowances","2. Verified emissions")) |>
    inner_join(act_map, by=c("main_activity_code"="code")) |>
    mutate(var=if_else(citl_information=="2. Verified emissions","verified","freealloc")) |>
    group_by(Country_ID=country_code, Sector_ID, var) |>
    summarise(v=sum(value,na.rm=TRUE), .groups="drop") |>
    pivot_wider(names_from=var, values_from=v, values_fill=0) |>
    mutate(free_alloc_share = if_else(verified>0, pmin(freealloc/verified,1)*mult, NA_real_)) |>
    select(Country_ID, Sector_ID, free_alloc_share)
}

dims <- list(
  Energy=c("Energy_Consumption","Fossil_Share","Renewables_Share","RE_Potential"),
  Labour=c("Unemployment_Rate","Labour_Market_Slack","Highly_Skilled_Workers"),
  Supply_Chain=c("Import_ExtraEU"), Technology=c("BERD","Regional_Innovation"),
  Institutions=c("QoG_Index","Climate_Mitigation_Laws"),
  Diversification=c("Sector_Concentration"))

dr  <- tar_read(data_reshaped); ew <- tar_read(empl_weights)
s1  <- tar_read(scope1_data) |> select(Country_ID, NUTS_ID, Sector_ID, Scope1_kt=Value)
io  <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg <- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
cbam <- compute_cbam_leg(io, ghg, ew) |> select(NUTS_ID, Sector_ID, CBAM_emb_tCO2)
cbam_cov_tab <- tibble::tibble(Sector_ID=c("C19-C20","C23","C24"), cbam_cov=1)

# pooled Vulnerability (built once; independent of free-alloc)
vw <- normalize_indicators(filter(dr, Sector_ID!="C"), ew, pool=TRUE)$wide
for (nm in names(dims)) { vars<-intersect(dims[[nm]],names(vw))
  for (vv in vars) vw <- impute_with_median(vw, vv)
  vw[[paste0("Vuln_",nm)]] <- rowMeans(select(vw, all_of(vars)), na.rm=TRUE) }
vw <- vw |> mutate(Vulnerability = range01(rowMeans(across(starts_with("Vuln_")), na.rm=TRUE)))
vsel <- vw |> select(NUTS_ID, Country_ID, Sector_ID, Vulnerability)

build_tri <- function(mult=1) {
  fa <- compute_fa(mult)
  df <- s1 |> filter(Sector_ID!="C") |>
    left_join(fa, by=c("Country_ID","Sector_ID")) |>
    left_join(cbam, by=c("NUTS_ID","Sector_ID")) |>
    left_join(cbam_cov_tab, by="Sector_ID") |>
    mutate(free_alloc_share=coalesce(free_alloc_share,1),
           CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0), cbam_cov=coalesce(cbam_cov,0))
  e <- assemble_exposure_cost(df)
  e |> inner_join(vsel, by=c("NUTS_ID","Country_ID","Sector_ID")) |>
    mutate(E=if_else(Exposure==0,NA_real_,Exposure),
           Risk_raw=sqrt(E)*sqrt(Vulnerability), Risk_norm=range01(Risk_raw))
}

tri <- build_tri(1)
cat("=== full pooled TRI (mult=1): rows", nrow(tri), "NAs Risk", sum(is.na(tri$Risk_norm)), "===\n")
base <- read.csv("Final data/Risk_data.csv") |> select(NUTS_ID, Sector_ID, Risk_base=Risk_norm)
cmp <- tri |> inner_join(base, by=c("NUTS_ID","Sector_ID")) |> filter(!is.na(Risk_norm)&!is.na(Risk_base))
cat("Spearman(new pooled TRI, baseline within-sector TRI):", round(cor(cmp$Risk_norm,cmp$Risk_base,method="spearman"),3),"\n")
cat("\nTop 10 TRI cells (pooled):\n")
print(tri |> arrange(desc(Risk_norm)) |> head(10) |>
      transmute(NUTS_ID,Sector_ID,Exposure=round(Exposure,3),Vulnerability=round(Vulnerability,3),Risk_norm=round(Risk_norm,3)))

# (a) C roll-up check
Ce <- build_tri(1)  # reuse e via tri? recompute exposure raw:
fa1 <- compute_fa(1)
eC <- s1 |> filter(Sector_ID!="C") |> left_join(fa1,by=c("Country_ID","Sector_ID")) |>
  left_join(cbam,by=c("NUTS_ID","Sector_ID")) |> left_join(cbam_cov_tab,by="Sector_ID") |>
  mutate(free_alloc_share=coalesce(free_alloc_share,1),CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0),cbam_cov=coalesce(cbam_cov,0)) |>
  assemble_exposure_cost() |> group_by(NUTS_ID) |>
  summarise(raw=sum(ETS_cost+CBAM_cost), .groups="drop") |> mutate(Exposure_C=range01(raw))
cat("\n(a) C roll-up: non-zero C exposure cells:", sum(eC$Exposure_C>0), "/ ", nrow(eC),
    " | top C region:", eC$NUTS_ID[which.max(eC$Exposure_C)], "\n")

# (c) free-allocation phase-out sensitivity
cat("\n=== (c) Free-allocation phase-out sensitivity ===\n")
t1 <- build_tri(1.0); t05 <- build_tri(0.5); t0 <- build_tri(0.0)
j <- t1 |> select(NUTS_ID,Sector_ID,r1=Risk_norm) |>
  inner_join(t05|>select(NUTS_ID,Sector_ID,r05=Risk_norm),by=c("NUTS_ID","Sector_ID")) |>
  inner_join(t0|>select(NUTS_ID,Sector_ID,r0=Risk_norm),by=c("NUTS_ID","Sector_ID")) |>
  filter(!is.na(r1)&!is.na(r0))
cat("Spearman(current vs 50% phase-out):", round(cor(j$r1,j$r05,method="spearman"),3),
    " | (current vs full phase-out):", round(cor(j$r1,j$r0,method="spearman"),3),"\n")
shareETS <- function(m){fa<-compute_fa(m); d<-s1|>filter(Sector_ID!="C")|>left_join(fa,by=c("Country_ID","Sector_ID"))|>
  left_join(cbam,by=c("NUTS_ID","Sector_ID"))|>left_join(cbam_cov_tab,by="Sector_ID")|>
  mutate(free_alloc_share=coalesce(free_alloc_share,1),CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0),cbam_cov=coalesce(cbam_cov,0))|>
  assemble_exposure_cost(); sum(d$ETS_cost)/sum(d$ETS_cost+d$CBAM_cost)}
cat("ETS share of total carbon cost:  mult=1.0:",round(shareETS(1),3)," 0.5:",round(shareETS(.5),3)," 0.0:",round(shareETS(0),3),"\n")

# write prototype output
out <- tri |> mutate(Risk_Band = ifelse(is.na(Risk_norm),"Zero Risk",
  as.character(cut(Risk_norm, seq(0,1,.2), include.lowest=TRUE,
    labels=c("Very Low","Low","Medium","High","Very High"))))) |>
  select(NUTS_ID, Country_ID, Sector_ID, Scope1_kt, free_alloc_share, CBAM_emb_tCO2,
         ETS_cost, CBAM_cost, Exposure, Vulnerability, Risk_norm, Risk_Band)
write.csv(out, "Final data/Risk_data_carbon_cost_PROTOTYPE.csv", row.names=FALSE)
cat("\nwrote PROTOTYPE csv:", nrow(out), "rows;",
    sum(out$Risk_Band!="Zero Risk"), "with positive carbon-cost risk\n")
