suppressMessages({library(dplyr); library(tidyr); library(readxl); library(targets)})
source("R/utils.R"); source("R/04_normalize.R"); source("R/05_aggregate.R"); source("R/exposure_cost.R")
# OFFICIAL carbon-cost-at-risk TRI (prototype; not yet wired into _targets):
#   Exposure = carbon cost ONLY, log-transformed, pooled over the 11 sub-sectors.
#   ETS leg uses EEA ETS-covered VERIFIED emissions (NOT Eurostat Scope 1):
#       ETS_cost = verified_NUTS * EUA * (1 - free_alloc_share).
#   CBAM leg = embodied carbon in extra-EU covered imports (FIGARO), ALL importers.
#   Scope 1/2/3 are kept in the OUTPUT for analysis only (not used in the index).
#   Croatia recombined HR02/05/06 -> HR04 so the grid matches Vulnerability (230 regions).

ETS_XLSX <- "Initial data/eea_t_eu-emission-trading-scheme_p_2005-2025_v01_r00/ETS_Database_April_2026.xlsx"
act_map <- tibble::tribble(~code,~Sector_ID,
  "35","C16-C18","36","C16-C18","21","C19-C20","22","C19-C20","37","C19-C20","38","C19-C20",
  "39","C19-C20","40","C19-C20","41","C19-C20","42","C19-C20","43","C19-C20","44","C19-C20",
  "29","C23","30","C23","31","C23","32","C23","33","C23","34","C23",
  "23","C24","24","C24","25","C24","26","C24","27","C24","28","C24")
raw_ets <- read_excel(ETS_XLSX,sheet="Sheet1") |> mutate(country_code=if_else(country_code=="GR","EL",country_code))
# country x sector: free-allocation share + ETS-covered verified emissions
ets_cs <- function(mult=1) raw_ets |>
  filter(year=="2024", country_code %in% .eu27_codes(),
    citl_information %in% c("1.1 Freely allocated allowances","2. Verified emissions")) |>
  inner_join(act_map,by=c("main_activity_code"="code")) |>
  mutate(var=if_else(citl_information=="2. Verified emissions","verified","freealloc")) |>
  group_by(Country_ID=country_code,Sector_ID,var) |> summarise(v=sum(value,na.rm=TRUE),.groups="drop") |>
  pivot_wider(names_from=var,values_from=v,values_fill=0) |>
  mutate(free_alloc_share=if_else(verified>0,pmin(freealloc/verified,1)*mult,NA_real_))

dims <- list(Energy=c("Energy_Consumption","Fossil_Share","Renewables_Share","RE_Potential"),
  Labour=c("Unemployment_Rate","Labour_Market_Slack","Highly_Skilled_Workers"),
  Supply_Chain=c("Import_ExtraEU"), Technology=c("BERD","Regional_Innovation"),
  Institutions=c("QoG_Index","Climate_Mitigation_Laws"), Diversification=c("Sector_Concentration"))

ew <- tar_read(empl_weights)
# Croatia recombination HR02/05/06 -> HR04 (NL/PT already on NUTS-2021 codes)
ew2 <- ew |> mutate(NUTS_ID=if_else(NUTS_ID %in% c("HR02","HR05","HR06"),"HR04",NUTS_ID)) |>
  group_by(Country_ID,NUTS_ID,Sector_ID) |>
  summarise(pers_employed=sum(pers_employed,na.rm=TRUE), weight=sum(weight,na.rm=TRUE), .groups="drop")
dr <- tar_read(data_reshaped)
io <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg<- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
cbam <- compute_cbam_leg(io,ghg,ew2) |> select(NUTS_ID,Sector_ID,CBAM_emb_tCO2)   # 11 subs, HR04 grid

# Vulnerability (pooled) -> defines the 230-region x 11-sector grid
vw <- normalize_indicators(filter(dr,Sector_ID!="C"),ew,pool=TRUE)$wide
for (nm in names(dims)){vars<-intersect(dims[[nm]],names(vw)); for(vv in vars) vw<-impute_with_median(vw,vv)
  vw[[paste0("Vuln_",nm)]]<-rowMeans(select(vw,all_of(vars)),na.rm=TRUE)}
vw <- vw|>mutate(Vulnerability=range01(rowMeans(across(starts_with("Vuln_")),na.rm=TRUE)))
vsel <- vw|>select(NUTS_ID,Country_ID,Sector_ID,Vulnerability)

# raw scopes for ANALYSIS columns (not in the index)
scopes_raw <- dr |> filter(Sector_ID!="C", Indicator %in% c("Scope1_Emissions","Scope2_Emissions","Scope3_Emissions")) |>
  group_by(NUTS_ID,Sector_ID,Indicator) |> summarise(Value=sum(Value,na.rm=TRUE),.groups="drop") |>
  pivot_wider(names_from=Indicator,values_from=Value)

verified_nuts <- function(mult=1) downscale_national_to_nuts2(
  ets_cs(mult)|>select(Country_ID,Sector_ID,verified), ew2, "verified") |> rename(ets_emis_t=verified)

assemble_df <- function(mult=1) vsel |>
  left_join(verified_nuts(mult),by=c("NUTS_ID","Country_ID","Sector_ID")) |>
  left_join(cbam,by=c("NUTS_ID","Sector_ID")) |>
  left_join(ets_cs(mult)|>select(Country_ID,Sector_ID,free_alloc_share),by=c("Country_ID","Sector_ID")) |>
  mutate(ets_emis_t=coalesce(ets_emis_t,0), CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0),
         free_alloc_share=coalesce(free_alloc_share,1), cbam_cov=1)

build_tri <- function(mult=1) assemble_exposure_cost(assemble_df(mult)) |>
  mutate(E=if_else(Exposure==0,NA_real_,Exposure), Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))

tri <- build_tri(1)
cat("=== carbon-cost TRI (EEA verified): cells", nrow(tri), "| regions", n_distinct(tri$NUTS_ID),
    "| positive", sum(!is.na(tri$Risk_norm)), "| sectors", n_distinct(tri$Sector_ID[!is.na(tri$Risk_norm)]),"/ 11 ===\n")
cat("Croatia HR04 present?", "HR04" %in% tri$NUTS_ID, " | Exposure sd:", round(sd(tri$Exposure,na.rm=TRUE),3),"\n")
base <- read.csv("Final data/Risk_data.csv")|>select(NUTS_ID,Sector_ID,Risk_base=Risk_norm)
cmp <- tri|>inner_join(base,by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(Risk_norm)&!is.na(Risk_base))
cat("Spearman(vs baseline):", round(cor(cmp$Risk_norm,cmp$Risk_base,method="spearman"),3),"\n")
cat("\nPer-sector mean exposure:\n")
print(tri|>group_by(Sector_ID)|>summarise(mean=round(mean(Exposure,na.rm=TRUE),3),n=sum(!is.na(Risk_norm)))|>arrange(desc(mean)),n=11)
cat("\nTop 10 TRI cells:\n")
print(tri|>arrange(desc(Risk_norm))|>head(10)|>transmute(NUTS_ID,Sector_ID,Exposure=round(Exposure,3),Vulnerability=round(Vulnerability,3),Risk_norm=round(Risk_norm,3)))

# (a) C roll-up
Craw <- assemble_exposure_cost(assemble_df(1),log_scale=FALSE)|>group_by(NUTS_ID,Country_ID)|>summarise(raw=sum(Exposure_raw),.groups="drop")
Cv <- vsel|>group_by(NUTS_ID,Country_ID)|>summarise(Vulnerability=mean(Vulnerability,na.rm=TRUE),.groups="drop")|>mutate(Vulnerability=range01(Vulnerability))
C_tri <- Craw|>mutate(Sector_ID="C",Exposure=range01(log1p(pmax(raw,0))))|>inner_join(Cv,by=c("NUTS_ID","Country_ID"))|>
  mutate(E=if_else(Exposure==0,NA_real_,Exposure),Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))
cat("\n(a) C roll-up: regions", nrow(C_tri)," top:",C_tri$NUTS_ID[which.max(C_tri$Risk_norm)],"\n")

# (c) phase-out sensitivity
t0 <- build_tri(0); jj <- tri|>select(NUTS_ID,Sector_ID,r1=Risk_norm)|>inner_join(t0|>select(NUTS_ID,Sector_ID,r0=Risk_norm),by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(r1)&!is.na(r0))
shareETS <- function(m){d<-assemble_exposure_cost(assemble_df(m),log_scale=FALSE); sum(d$ETS_cost)/sum(d$ETS_cost+d$CBAM_cost)}
cat("(c) phase-out Spearman current-vs-full:",round(cor(jj$r1,jj$r0,method="spearman"),3),
    "| ETS share of cost @100/50/0% free-alloc:",round(shareETS(1),2),round(shareETS(.5),2),round(shareETS(0),2),"\n")

out <- bind_rows(tri, C_tri) |> left_join(scopes_raw,by=c("NUTS_ID","Sector_ID")) |>
  mutate(Risk_Band=ifelse(is.na(Risk_norm),"Zero Risk",as.character(cut(Risk_norm,seq(0,1,.2),include.lowest=TRUE,labels=c("Very Low","Low","Medium","High","Very High")))))|>
  select(NUTS_ID,Country_ID,Sector_ID,Scope1_Emissions,Scope2_Emissions,Scope3_Emissions,
         ets_emis_t,CBAM_emb_tCO2,Exposure,Vulnerability,Risk_norm,Risk_Band)
write.csv(out,"Final data/Risk_data_carbon_cost_PROTOTYPE.csv",row.names=FALSE)
cat("\nwrote PROTOTYPE csv:",nrow(out),"rows (incl C); analysis cols S1/S2/S3 retained\n")
