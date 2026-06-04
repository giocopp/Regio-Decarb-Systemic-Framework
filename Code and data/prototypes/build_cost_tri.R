suppressMessages({library(dplyr); library(tidyr); library(readxl); library(targets)})
source("R/utils.R"); source("R/04_normalize.R"); source("R/05_aggregate.R"); source("R/exposure_cost.R")
# OFFICIAL carbon-cost-at-risk TRI (prototype; not yet wired into _targets):
#   Exposure = carbon cost ONLY (emissions facet dropped -> no double-count),
#              log-transformed, pooled across the 11 sub-sectors.
#   Carbon cost = Scope1*EUA*(1-free_alloc_share) + CBAM_imports*EUA.
#   CBAM applies to ALL importing sectors (cbam_cov=1) -> every sector covered.
#   Risk = sqrt(Exposure) * sqrt(Vulnerability_pooled), pooled. EUA scalar = 1.

ETS_XLSX <- "Initial data/eea_t_eu-emission-trading-scheme_p_2005-2025_v01_r00/ETS_Database_April_2026.xlsx"
act_map <- tibble::tribble(~code,~Sector_ID,
  "35","C16-C18","36","C16-C18","21","C19-C20","22","C19-C20","37","C19-C20","38","C19-C20",
  "39","C19-C20","40","C19-C20","41","C19-C20","42","C19-C20","43","C19-C20","44","C19-C20",
  "29","C23","30","C23","31","C23","32","C23","33","C23","34","C23",
  "23","C24","24","C24","25","C24","26","C24","27","C24","28","C24")
compute_fa <- function(mult=1) read_excel(ETS_XLSX,sheet="Sheet1") |>
  mutate(country_code=if_else(country_code=="GR","EL",country_code)) |>
  filter(year=="2024", country_code %in% .eu27_codes(),
    citl_information %in% c("1.1 Freely allocated allowances","2. Verified emissions")) |>
  inner_join(act_map,by=c("main_activity_code"="code")) |>
  mutate(var=if_else(citl_information=="2. Verified emissions","verified","freealloc")) |>
  group_by(Country_ID=country_code,Sector_ID,var) |> summarise(v=sum(value,na.rm=TRUE),.groups="drop") |>
  pivot_wider(names_from=var,values_from=v,values_fill=0) |>
  mutate(free_alloc_share=if_else(verified>0,pmin(freealloc/verified,1)*mult,NA_real_)) |>
  select(Country_ID,Sector_ID,free_alloc_share)

dims <- list(Energy=c("Energy_Consumption","Fossil_Share","Renewables_Share","RE_Potential"),
  Labour=c("Unemployment_Rate","Labour_Market_Slack","Highly_Skilled_Workers"),
  Supply_Chain=c("Import_ExtraEU"), Technology=c("BERD","Regional_Innovation"),
  Institutions=c("QoG_Index","Climate_Mitigation_Laws"), Diversification=c("Sector_Concentration"))

dr <- tar_read(data_reshaped); ew <- tar_read(empl_weights)
s1 <- tar_read(scope1_data) |> select(Country_ID,NUTS_ID,Sector_ID,Scope1_kt=Value) |> filter(Sector_ID!="C")
io <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg<- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
cbam <- compute_cbam_leg(io,ghg,ew) |> select(NUTS_ID,Sector_ID,CBAM_emb_tCO2)

# Vulnerability (pooled over sub-sectors)
vw <- normalize_indicators(filter(dr,Sector_ID!="C"),ew,pool=TRUE)$wide
for (nm in names(dims)){vars<-intersect(dims[[nm]],names(vw)); for(vv in vars) vw<-impute_with_median(vw,vv)
  vw[[paste0("Vuln_",nm)]]<-rowMeans(select(vw,all_of(vars)),na.rm=TRUE)}
vw <- vw|>mutate(Vulnerability=range01(rowMeans(across(starts_with("Vuln_")),na.rm=TRUE)))
vsel <- vw|>select(NUTS_ID,Country_ID,Sector_ID,Vulnerability)

assemble <- function(mult=1, log_scale=TRUE)
  s1 |> left_join(compute_fa(mult),by=c("Country_ID","Sector_ID")) |>
    left_join(cbam,by=c("NUTS_ID","Sector_ID")) |>
    mutate(free_alloc_share=coalesce(free_alloc_share,1),
           CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0), cbam_cov=1) |>   # CBAM on ALL importers
    assemble_exposure_cost(log_scale=log_scale)

build_tri <- function(mult=1) assemble(mult) |>
  inner_join(vsel,by=c("NUTS_ID","Country_ID","Sector_ID")) |>
  mutate(E=if_else(Exposure==0,NA_real_,Exposure), Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))

tri <- build_tri(1)
cat("=== carbon-cost-only TRI: cells", nrow(tri),
    "| positive", sum(!is.na(tri$Risk_norm)), "| sectors", n_distinct(tri$Sector_ID[!is.na(tri$Risk_norm)]),"/ 11 ===\n")
cat("Exposure sd:", round(sd(tri$Exposure,na.rm=TRUE),3), "\n")
base <- read.csv("Final data/Risk_data.csv")|>select(NUTS_ID,Sector_ID,Risk_base=Risk_norm)
cmp <- tri|>inner_join(base,by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(Risk_norm)&!is.na(Risk_base))
cat("Spearman(carbon-cost TRI vs baseline):", round(cor(cmp$Risk_norm,cmp$Risk_base,method="spearman"),3),"\n")
cat("\nPer-sector mean exposure:\n")
print(tri|>group_by(Sector_ID)|>summarise(mean=round(mean(Exposure,na.rm=TRUE),3),n_pos=sum(!is.na(Risk_norm)))|>arrange(desc(mean)),n=11)
cat("\nTop 10 TRI cells:\n")
print(tri|>arrange(desc(Risk_norm))|>head(10)|>transmute(NUTS_ID,Sector_ID,Exposure=round(Exposure,3),Vulnerability=round(Vulnerability,3),Risk_norm=round(Risk_norm,3)))

# (a) C total-manufacturing roll-up: per-region sum of sub carbon costs
Craw <- assemble(1, log_scale=FALSE) |> group_by(NUTS_ID,Country_ID) |> summarise(raw=sum(Exposure_raw),.groups="drop")
Cv <- vsel|>group_by(NUTS_ID,Country_ID)|>summarise(Vulnerability=mean(Vulnerability,na.rm=TRUE),.groups="drop")|>mutate(Vulnerability=range01(Vulnerability))
C_tri <- Craw|>mutate(Sector_ID="C", Exposure=range01(log1p(pmax(raw,0))))|>inner_join(Cv,by=c("NUTS_ID","Country_ID"))|>
  mutate(E=if_else(Exposure==0,NA_real_,Exposure), Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))
cat("\n(a) C roll-up: non-zero", sum(C_tri$Exposure>0),"/",nrow(C_tri)," top:",C_tri$NUTS_ID[which.max(C_tri$Risk_norm)],"\n")

# (c) free-allocation phase-out sensitivity
t0 <- build_tri(0)
jj <- tri|>select(NUTS_ID,Sector_ID,r1=Risk_norm)|>inner_join(t0|>select(NUTS_ID,Sector_ID,r0=Risk_norm),by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(r1)&!is.na(r0))
shareETS <- function(m){d<-assemble(m,log_scale=FALSE); sum(d$ETS_cost)/sum(d$ETS_cost+d$CBAM_cost)}
cat("(c) phase-out Spearman current-vs-full:", round(cor(jj$r1,jj$r0,method="spearman"),3),
    "| ETS share of cost @100/50/0% free-alloc:", round(shareETS(1),2),round(shareETS(.5),2),round(shareETS(0),2),"\n")

out <- bind_rows(tri, C_tri) |>
  mutate(Risk_Band=ifelse(is.na(Risk_norm),"Zero Risk",as.character(cut(Risk_norm,seq(0,1,.2),include.lowest=TRUE,labels=c("Very Low","Low","Medium","High","Very High")))))|>
  select(NUTS_ID,Country_ID,Sector_ID,Exposure,Vulnerability,Risk_norm,Risk_Band)
write.csv(out,"Final data/Risk_data_carbon_cost_PROTOTYPE.csv",row.names=FALSE)
cat("\nwrote PROTOTYPE csv:",nrow(out),"rows (incl C)\n")
