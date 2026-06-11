# SUPERSEDED by the Phase-5b targets in _targets.R + R/exposure_cost.R;
# kept as the historical exploration record.
# Carbon-cost-at-risk TRI (prototype). Exposure = ETS cost + CBAM cost, min-max (env TRI_NORM=rank optional),
# pooled; headline at full phase-in (free_alloc=0). ETS leg geocoded (prototypes/ets_geocode.R).
suppressMessages({library(dplyr); library(tidyr); library(readxl); library(targets)})
source("R/utils.R"); source("R/04_normalize.R"); source("R/05_aggregate.R"); source("R/exposure_cost.R")
# percentile rank in (0,1), NA-safe — optional top-level normalization
prank <- function(x){ ok <- !is.na(x); o <- rep(NA_real_, length(x)); o[ok] <- (rank(x[ok], ties.method="average")-0.5)/sum(ok); o }
NORM <- Sys.getenv("TRI_NORM", unset = "minmax")   # top-level norm for Exposure & Vulnerability: "minmax" (default) or "rank"
norm_fun <- function(x) if (identical(NORM, "rank")) prank(x) else range01(x)

EUA_2024 <- 64.8

ets_geo <- read.csv("Initial data/EUTL_euets_info/ets_nuts2_sector.csv", stringsAsFactors=FALSE) |>
  dplyr::select(NUTS_ID, Country_ID, Sector_ID, ets_emis_t)
ets_fa <- read.csv("Initial data/EUTL_euets_info/ets_country_sector_freealloc.csv", stringsAsFactors=FALSE) |>
  dplyr::transmute(Country_ID, Sector_ID, free_alloc_base = pmin(free_alloc_share, 1))
fa_of <- function(mult=1) dplyr::transmute(ets_fa, Country_ID, Sector_ID, free_alloc_share = free_alloc_base * mult)

dims <- list(Energy=c("Energy_Consumption","Fossil_Share","Renewables_Share","RE_Potential"),
  Labour=c("Unemployment_Rate","Labour_Market_Slack","Highly_Skilled_Workers"),
  Technology=c("BERD","Regional_Innovation"),
  Institutions=c("QoG_Index","Climate_Mitigation_Laws"), Diversification=c("Sector_Concentration"))

ew <- tar_read(empl_weights)
ew2 <- ew |> mutate(NUTS_ID=if_else(NUTS_ID %in% c("HR02","HR05","HR06"),"HR04",NUTS_ID)) |>
  group_by(Country_ID,NUTS_ID,Sector_ID) |>
  summarise(pers_employed=sum(pers_employed,na.rm=TRUE), weight=sum(weight,na.rm=TRUE), .groups="drop")
dr <- tar_read(data_reshaped)
io <- readRDS("Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2023.rds")
ghg<- readRDS("Initial data/Non sector data/FIGARO_env_ac_ghgfp_2023.rds")
# CBAM downscaling: geocoded plant shares for the 4 ETS sectors (real production geography),
# employment for the rest (no plant/output data at sub-sector x NUTS-2). Heavy-sector weights
# span the full country grid with explicit 0 for plant-less regions, else the downscaler's
# C-fallback (utils.R) refills them and inflates the total.
heavy_sec <- c("C16-C18","C19-C20","C23","C24")
grid <- ew2 |> filter(Sector_ID=="C") |> distinct(Country_ID,NUTS_ID)
geo_share <- ets_geo |> filter(Sector_ID %in% heavy_sec) |>
  group_by(Country_ID,Sector_ID) |> mutate(weight=ets_emis_t/sum(ets_emis_t,na.rm=TRUE)) |>
  ungroup() |> select(Country_ID,NUTS_ID,Sector_ID,weight)
geo_keys <- distinct(geo_share,Country_ID,Sector_ID)
geo_w <- geo_keys |> inner_join(grid,by="Country_ID",relationship="many-to-many") |>
  left_join(geo_share,by=c("Country_ID","NUTS_ID","Sector_ID")) |> mutate(weight=coalesce(weight,0)) |>
  group_by(Country_ID,Sector_ID) |> mutate(weight=weight/sum(weight)) |> ungroup()
emp_w <- ew2 |> select(Country_ID,NUTS_ID,Sector_ID,weight) |> anti_join(geo_keys,by=c("Country_ID","Sector_ID"))
cbam_weights <- bind_rows(geo_w,emp_w)
cbam <- compute_cbam_leg(io,ghg,cbam_weights) |> select(NUTS_ID,Sector_ID,CBAM_emb_tCO2)

vw <- normalize_indicators(filter(dr,Sector_ID!="C"),ew,pool=TRUE)$wide
for (nm in names(dims)){vars<-intersect(dims[[nm]],names(vw)); for(vv in vars) vw<-impute_with_median(vw,vv)
  vw[[paste0("Vuln_",nm)]]<-rowMeans(select(vw,all_of(vars)),na.rm=TRUE)}
vw <- vw|>mutate(Vulnerability=norm_fun(rowMeans(across(starts_with("Vuln_")),na.rm=TRUE)))
vsel <- vw|>select(NUTS_ID,Country_ID,Sector_ID,Vulnerability)

scopes_raw <- dr |> filter(Sector_ID!="C", Indicator %in% c("Scope1_Emissions","Scope2_Emissions","Scope3_Emissions")) |>
  group_by(NUTS_ID,Sector_ID,Indicator) |> summarise(Value=sum(Value,na.rm=TRUE),.groups="drop") |>
  pivot_wider(names_from=Indicator,values_from=Value)

verified_nuts <- function(mult=1) ets_geo

assemble_df <- function(mult=1, cbam_factor=1) vsel |>
  left_join(verified_nuts(mult),by=c("NUTS_ID","Country_ID","Sector_ID")) |>
  left_join(cbam,by=c("NUTS_ID","Sector_ID")) |>
  left_join(fa_of(mult),by=c("Country_ID","Sector_ID")) |>
  mutate(ets_emis_t=coalesce(ets_emis_t,0), CBAM_emb_tCO2=coalesce(CBAM_emb_tCO2,0),
         free_alloc_share=coalesce(free_alloc_share,1), cbam_cov=cbam_factor)

build_tri <- function(mult=1, cbam_factor=1, eua=1) assemble_exposure_cost(assemble_df(mult,cbam_factor), eua_price=eua, log_scale=FALSE) |>
  mutate(Exposure=norm_fun(Exposure), E=if_else(Exposure==0,NA_real_,Exposure), Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))

tri <- build_tri(0)
cat("=== carbon-cost TRI (FULL phase-in, EUTL geocoded): cells", nrow(tri), "| regions", n_distinct(tri$NUTS_ID),
    "| positive", sum(!is.na(tri$Risk_norm)), "| sectors", n_distinct(tri$Sector_ID[!is.na(tri$Risk_norm)]),"/ 11 ===\n")
cat("Croatia HR04 present?", "HR04" %in% tri$NUTS_ID, " | Exposure sd:", round(sd(tri$Exposure,na.rm=TRUE),3),"\n")
base <- read.csv("Final data/Risk_data.csv")|>select(NUTS_ID,Sector_ID,Risk_base=Risk_norm)
cmp <- tri|>inner_join(base,by=c("NUTS_ID","Sector_ID"))|>filter(!is.na(Risk_norm)&!is.na(Risk_base))
cat("Spearman(vs baseline):", round(cor(cmp$Risk_norm,cmp$Risk_base,method="spearman"),3),"\n")
cat("\nPer-sector mean exposure:\n")
print(tri|>group_by(Sector_ID)|>summarise(mean=round(mean(Exposure,na.rm=TRUE),3),n=sum(!is.na(Risk_norm)))|>arrange(desc(mean)),n=11)
cat("\nTop 10 TRI cells:\n")
print(tri|>arrange(desc(Risk_norm))|>head(10)|>transmute(NUTS_ID,Sector_ID,Exposure=round(Exposure,3),Vulnerability=round(Vulnerability,3),Risk_norm=round(Risk_norm,3)))

Craw <- assemble_exposure_cost(assemble_df(0),log_scale=FALSE)|>group_by(NUTS_ID,Country_ID)|>summarise(raw=sum(Exposure_raw),.groups="drop")
Cv <- vsel|>group_by(NUTS_ID,Country_ID)|>summarise(Vulnerability=mean(Vulnerability,na.rm=TRUE),.groups="drop")|>mutate(Vulnerability=norm_fun(Vulnerability))
C_tri <- Craw|>mutate(Sector_ID="C",Exposure=norm_fun(pmax(raw,0)))|>inner_join(Cv,by=c("NUTS_ID","Country_ID"))|>
  mutate(E=if_else(Exposure==0,NA_real_,Exposure),Risk_norm=range01(sqrt(E)*sqrt(Vulnerability)))
cat("\n(a) C roll-up: regions", nrow(C_tri)," top:",C_tri$NUTS_ID[which.max(C_tri$Risk_norm)],"\n")

# CBAM factor F(t), Reg. (EU) 2023/956: free allocation = base*(1-F), CBAM coverage = F
cbam_F <- c(`2026`=0.025,`2027`=0.05,`2028`=0.10,`2029`=0.225,`2030`=0.485,
            `2031`=0.61,`2032`=0.735,`2033`=0.86,`2034`=1.0)
F_of <- function(y) if (y <= 2025) 0 else as.numeric(cbam_F[as.character(y)])
yrs  <- c(2024,2026,2028,2030,2032,2034)
traj <- do.call(rbind, lapply(yrs, function(y){
  Fy <- F_of(y)
  d  <- assemble_exposure_cost(assemble_df(1-Fy, Fy), eua_price=EUA_2024, log_scale=FALSE)
  rk <- build_tri(1-Fy, Fy)
  jj <- tri|>select(NUTS_ID,Sector_ID,rH=Risk_norm)|>
        inner_join(rk|>select(NUTS_ID,Sector_ID,r=Risk_norm),by=c("NUTS_ID","Sector_ID"))|>
        filter(!is.na(rH)&!is.na(r))
  data.frame(year=y, free_alloc=round(1-Fy,3), CBAM_factor=Fy,
             cost_EURbn=round(sum(d$Exposure_raw)/1e9,1),
             ETS_share=round(sum(d$ETS_cost)/sum(d$ETS_cost+d$CBAM_cost),2),
             n_priced=sum(rk$Exposure>0,na.rm=TRUE),
             rho_vs_2034=round(cor(jj$r,jj$rH,method="spearman"),2))
}))
cat("\n(c) phase-out trajectory (EUA =",EUA_2024,"EUR/t; headline = 2034):\n")
print(traj, row.names=FALSE)

out <- bind_rows(tri, C_tri) |> left_join(scopes_raw,by=c("NUTS_ID","Sector_ID")) |>
  mutate(Risk_Band=ifelse(is.na(Risk_norm),"Zero Risk",as.character(cut(Risk_norm,seq(0,1,.2),include.lowest=TRUE,labels=c("Very Low","Low","Medium","High","Very High")))))|>
  select(NUTS_ID,Country_ID,Sector_ID,Scope1_Emissions,Scope2_Emissions,Scope3_Emissions,
         ets_emis_t,CBAM_emb_tCO2,Exposure,Vulnerability,Risk_norm,Risk_Band)
write.csv(out,"Final data/Risk_data_carbon_cost_PROTOTYPE.csv",row.names=FALSE)
cat("\nwrote PROTOTYPE csv:",nrow(out),"rows (incl C)\n")
