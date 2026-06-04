suppressMessages({library(dplyr); library(tidyr); library(readxl); library(targets)})
source("R/utils.R"); source("R/04_normalize.R"); source("R/05_aggregate.R"); source("R/exposure_cost.R")

ETS_XLSX <- "Initial data/eea_t_eu-emission-trading-scheme_p_2005-2025_v01_r00/ETS_Database_April_2026.xlsx"
act_map <- tibble::tribble(~code,~Sector_ID,
  "35","C16-C18","36","C16-C18",
  "21","C19-C20","22","C19-C20","37","C19-C20","38","C19-C20","39","C19-C20",
  "40","C19-C20","41","C19-C20","42","C19-C20","43","C19-C20","44","C19-C20",
  "29","C23","30","C23","31","C23","32","C23","33","C23","34","C23",
  "23","C24","24","C24","25","C24","26","C24","27","C24","28","C24")
compute_fa <- function(mult=1) read_excel(ETS_XLSX, sheet="Sheet1") |>
  mutate(country_code=if_else(country_code=="GR","EL",country_code)) |>
  filter(year=="2024", country_code %in% .eu27_codes(),
         citl_information %in% c("1.1 Freely allocated allowances","2. Verified emissions")) |>
  inner_join(act_map, by=c("main_activity_code"="code")) |>
  mutate(var=if_else(citl_information=="2. Verified emissions","verified","freealloc")) |>
  group_by(Country_ID=country_code, Sector_ID, var) |>
  summarise(v=sum(value,na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from=var, values_from=v, values_fill=0) |>
  mutate(free_alloc_share=if_else(verified>0, pmin(freealloc/verified,1)*mult, NA_real_)) |>
  select(Country_ID, Sector_ID, free_alloc_share)

dims <- list(Energy=c("Energy_Consumption","Fossil_Share","Renewables_Share","RE_Potential"),
  Labour=c("Unemployment_Rate","Labour_Market_Slack","Highly_Skilled_Workers"),
  Supply_Chain=c("Import_ExtraEU"), Technology=c("BERD","Regional_Innovation"),
  Institutions=c("QoG_Index","Climate_Mitigation_Laws"), Diversification=c("Sector_Concentration"))

dr <- tar_read(data_reshaped); ew <- tar_read(empl_weights)
s1 <- tar_read(scope1_data) |> select(Country_ID, NUTS_ID, Sector_ID, Scope1_kt=Value)
io <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg<- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
cbam <- compute_cbam_leg(io, ghg, ew) |> select(NUTS_ID, Sector_ID, CBAM_emb_tCO2)
cbam_cov_tab <- tibble::tibble(Sector_ID=c("C19-C20","C23","C24"), cbam_cov=1)

# Vulnerability (pooled)
vw <- normalize_indicators(filter(dr, Sector_ID!="C"), ew, pool=TRUE)$wide
for (nm in names(dims)){vars<-intersect(dims[[nm]],names(vw)); for(vv in vars) vw<-impute_with_median(vw,vv)
  vw[[paste0("Vuln_",nm)]]<-rowMeans(select(vw,all_of(vars)),na.rm=TRUE)}
vw <- vw|>mutate(Vulnerability=range01(rowMeans(across(starts_with("Vuln_")),na.rm=TRUE)))
vsel <- vw|>select(NUTS_ID,Country_ID,Sector_ID,Vulnerability)

# --- emissions facet: LOG vs LINEAR ---
logn <- function(x) range01(log1p(pmax(x,0)))     # log1p handles 0; pooled range01
scopes_raw <- dr |>
  filter(Sector_ID!="C", !NUTS_ID %in% excluded_nuts,
         Indicator %in% c("Scope1_Emissions","Scope2_Emissions","Scope3_Emissions")) |>
  transmute(NUTS_ID, Country_ID=substr(NUTS_ID,1,2), Sector_ID, Indicator, Value) |>
  group_by(NUTS_ID,Country_ID,Sector_ID,Indicator) |> summarise(Value=sum(Value,na.rm=TRUE),.groups="drop") |>
  pivot_wider(names_from=Indicator, values_from=Value)
em_log <- scopes_raw |> mutate(emis_facet = rowMeans(cbind(logn(Scope1_Emissions),logn(Scope2_Emissions),logn(Scope3_Emissions)),na.rm=TRUE)) |>
  select(NUTS_ID,Country_ID,Sector_ID,emis_facet)
em_lin <- vw |> transmute(NUTS_ID,Country_ID,Sector_ID,
  emis_facet=rowMeans(across(any_of(c("Scope1_Emissions","Scope2_Emissions","Scope3_Emissions"))),na.rm=TRUE))

build_tri <- function(mult=1, w_emis=0.5, use_log=TRUE){
  fa<-compute_fa(mult)
  e <- s1|>filter(Sector_ID!="C")|>left_join(fa,by=c("Country_ID","Sector_ID"))|>
    left_join(cbam,by=c("NUTS_ID","Sector_ID"))|>left_join(cbam_cov_tab,by="Sector_ID")|>
    mutate(free_alloc_share=coalesce(free_alloc_share,1),CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0),cbam_cov=coalesce(cbam_cov,0))|>
    assemble_exposure_cost()
  e <- if(use_log) mutate(e, cost_facet=range01(log1p(pmax(Exposure_raw,0)))) else mutate(e, cost_facet=Exposure)
  emf <- if(use_log) em_log else em_lin
  e|>left_join(emf,by=c("NUTS_ID","Country_ID","Sector_ID"))|>
    mutate(Exposure=range01(w_emis*emis_facet+(1-w_emis)*cost_facet))|>
    inner_join(vsel,by=c("NUTS_ID","Country_ID","Sector_ID"))|>
    mutate(E=if_else(Exposure==0,NA_real_,Exposure),Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))
}

lin <- build_tri(use_log=FALSE); lg <- build_tri(use_log=TRUE)
cat("=== VARIABILITY: linear vs log exposure ===\n")
cat(sprintf("Exposure sd:   linear %.3f   log %.3f\n", sd(lin$Exposure,na.rm=TRUE), sd(lg$Exposure,na.rm=TRUE)))
cat(sprintf("Exposure IQR:  linear %.3f   log %.3f\n",
    IQR(lin$Exposure,na.rm=TRUE), IQR(lg$Exposure,na.rm=TRUE)))
cat("\nPer-sector mean exposure (LOG):\n")
print(lg|>group_by(Sector_ID)|>summarise(mean=round(mean(Exposure,na.rm=TRUE),3))|>arrange(desc(mean)),n=12)
cat("\nTop 8 TRI cells (LOG):\n")
print(lg|>arrange(desc(Risk_norm))|>head(8)|>transmute(NUTS_ID,Sector_ID,Exposure=round(Exposure,3),Vulnerability=round(Vulnerability,3),Risk_norm=round(Risk_norm,3)))
base<-read.csv("Final data/Risk_data.csv")|>select(NUTS_ID,Sector_ID,Risk_base=Risk_norm)
cmp<-lg|>inner_join(base,by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(Risk_norm)&!is.na(Risk_base))
cat("\nSpearman(log TRI vs baseline):",round(cor(cmp$Risk_norm,cmp$Risk_base,method="spearman"),3),"\n")
t0<-build_tri(0,use_log=TRUE); j<-lg|>select(NUTS_ID,Sector_ID,r1=Risk_norm)|>inner_join(t0|>select(NUTS_ID,Sector_ID,r0=Risk_norm),by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(r1)&!is.na(r0))
cat("Phase-out Spearman (current vs full):",round(cor(j$r1,j$r0,method="spearman"),3),"\n")

out<-lg|>mutate(Risk_Band=ifelse(is.na(Risk_norm),"Zero Risk",as.character(cut(Risk_norm,seq(0,1,.2),include.lowest=TRUE,labels=c("Very Low","Low","Medium","High","Very High")))))|>
  select(NUTS_ID,Country_ID,Sector_ID,emis_facet,cost_facet,Exposure,Vulnerability,Risk_norm,Risk_Band)
write.csv(out,"Final data/Risk_data_carbon_cost_PROTOTYPE.csv",row.names=FALSE)
cat("\nwrote PROTOTYPE csv (log):",nrow(out),"rows;",sum(out$Risk_Band!="Zero Risk"),"positive\n")
