# Transition Risk Index — Methodology

This document specifies the methodology actually implemented by the pipeline. It is the source of truth for the paper and supplementary appendix. Each section maps to a specific stage of the `targets` pipeline.

## 1. Conceptual framework

The Transition Risk Index (TRI) follows the **IPCC AR6 risk framework**:

$$\text{TRI}_{r,s} = \text{Exposure}_{r,s}^{0.5} \times \text{Vulnerability}_{r,s}^{0.5}$$

Indexed over $r$ ∈ EU-27 NUTS-2 regions, $s$ ∈ 12 manufacturing sectors (NACE Rev.2 Section C plus 11 sub-aggregates). Aggregation is geometric so a region cannot have non-zero risk on a single axis alone.

The pipeline produces **two TRI variants**:

1. **Headline — covered-carbon TRI** (`risk_data` target, §10.1): Exposure is the **covered carbon volume a region-sector carries** — its EUTL plant-level ETS verified emissions plus the embodied carbon in its extra-EU imports of CBAM-covered goods (both in tonnes CO2) — normalised **pooled across all region × sector cells** (so a cell's score reflects its carbon volume relative to all manufacturing, not rescaled within its own sector; §10.1). Vulnerability is the pooled 4-dimension adaptive-capacity composite (§9.1). No carbon price enters: a single EU-wide EUA price is a common scalar that cancels under normalisation, so it never moved the ranking — the priced formulation and its 2024→2034 phase-in trajectory were removed (§10.1).
2. **Raw-emissions baseline TRI** (`risk_data_raw_emissions` target, §10.2): the index as submitted to Climate Policy in Oct-2025 (Scope 1/2/3 emissions exposure, within-sector normalisation, 6 vulnerability dimensions). Retained for comparison only; the sensitivity workbook reports the Spearman between the two.

Both variants build Exposure and Vulnerability from harmonised regional indicators normalised to $[0.01, 0.99]$ at the indicator level.

The unit of analysis is the **(NUTS-2 region × manufacturing sector)** cell. After excluding ultraperipheral regions, 230 NUTS-2 regions × 12 sectors = 2,760 cells.

## 2. Geographic and sectoral scope

### NUTS-2 regions
- EU-27 NUTS-2 (NUTS 2021 classification).
- **Excluded by design**: ultraperipheral regions whose Eurostat coverage on climate, energy, and employment indicators is incomplete and which are not part of the continental industrial base the TRI targets. The full excluded list is in `R/utils.R::excluded_nuts`: `FRY1, FRY2, FRY3, FRY4, FRY5, ES63, ES64, PT20, PT30, FI20`.
- **CY00 (Cyprus) is kept** as the sole NUTS-2 of an EU-27 member state.
- Effective sample: **230 NUTS-2 regions**.

### NUTS-2013 → NUTS-2021 recombinations
Three countries reorganised their NUTS-2 between vintages. The pipeline applies the following recombinations in `R/03_reshape.R::reshape_to_grid()` and `R/04_normalize.R::normalize_indicators()` (the latter for `empl_weights`):

| Country | NUTS-2013 codes | NUTS-2021 codes |
|---|---|---|
| Croatia | HR02 + HR05 + HR06 | HR04 (Continentalna Hrvatska) |
| Netherlands | NL35 → NL31 (Utrecht), NL36 → NL33 (Zuid-Holland) | |
| Portugal | PT19 + PT1D → PT16 (Centro), PT1A + PT1B → PT17 (Lisboa), PT1C → PT18 (Alentejo) | |

For each recombination, indicator values are aggregated using the rule in `agg_rules` (utils.R): `sum` for extensive quantities (€, kt CO2eq, GWh, counts), `mean` for intensive quantities (rates, indices, shares).

### Manufacturing sectors
NACE Rev.2 Section C plus 11 sub-aggregates (lookup in `R/utils.R::sector_aggregation`):

| Sector_ID | Description |
|---|---|
| C | Total Manufacturing |
| C10-C12 | Food, Beverage, Tobacco |
| C13-C15 | Textiles, Leather, Wearing |
| C16-C18 | Wood, Paper, Printing |
| C19-C20 | Chemicals & Petrochemical |
| C21-C22 | Pharmaceutical & Plastic |
| C23 | Non-metallic Mineral Products |
| C24 | Basic Metal Products |
| C25+C28 | Fabricated Metal & Machinery |
| C26-C27 | Electronic & Electrical |
| C29-C30 | Motor Vehicles & Transport Equipment |
| C31-C33 | Other Manufacturing & Repair |

## 3. Data sources

All indicators are downloaded automatically by `R/01_create_data.R` from Eurostat (via `restatapi`) plus three small external sources. Every download selects its **own latest year with ≥95% EU-27 coverage** (Section 4). The audit log of which year each indicator picked is in `Final data/Coverage_Report.xlsx`.

| Indicator | Source dataset(s) | Geo level pulled | Function |
|---|---|---|---|
| Employment_Weights | `sbs_r_nuts2021` (EMP_LOC_NR, TOTAL size, NACE C10–C33) | NUTS-2 | `create_employment_weights` |
| GFCF | `nama_10r_2gfcf` (S1 sector, NACE C, MIO_EUR) | NUTS-2 | `create_gfcf` |
| Unemployment_Rate | `lfst_r_lfu3rt` (TOTAL ISCED, sex=T, age=Y20-64) | NUTS-2 | `create_unemployment` |
| Labour_Market_Slack | `lfst_r_sla_ga` (PC_ELF, sex=T, wstatus=SLACK, age=Y15-74) | NUTS-2 | `create_labour_slack` |
| Highly_Skilled_Workers | `hrst_st_rcat` (category=HRST, unit=PC_ACT, sex=T) | NUTS-2 | `create_highly_skilled` |
| Scope1_Emissions (Scope 1) | `env_ac_ainah_r2` (airpol=GHG, THS_T, all C NACE codes) | National → NUTS-2 by empl share | `create_scope1` |
| Scope2_Emissions | `nrg_bal_c` (industrial electricity, GWH) × hard-coded EEA/EMBER 2022 grid EFs | National → NUTS-2 by empl share | `create_scope2` |
| Scope3_Emissions | FIGARO `naio_10_fcp_ii4` + `env_ac_ghgfp` (Leontief MRIO with D35 excluded) | National → NUTS-2 by empl share | `create_scope3` |
| Energy_Consumption | `nrg_bal_c` (FC_IND_* by sub-sector, siec=TOTAL, GWH) | National → NUTS-2 by empl share | `create_energy_consumption` |
| Fossil_Share, Renewables_Share | `nrg_bal_c` ratios: siec=FE / siec=TOTAL and siec=RA000 / siec=TOTAL | National replicated to NUTS-2 | `create_energy_shares` |
| Capital_Stock_Based_Prod | `nama_10_cp_a21` (NCS_HW, N11N, I20 — index 2020=100) | National replicated to NUTS-2 | `create_capital_stock_prod` |
| Import_ExtraEU, Export_ExtraEU | `ext_tec01` (partner=EXT_EU, sizeclas=TOTAL, THS_EUR) | National → NUTS-2 by empl share | `create_trade_extra_eu` |
| BERD (regional R&D intensity) | `rd_e_gerdreg` (business-sector R&D, % of regional GDP) | NUTS-2 (region-level, replicated across sectors); gap fallback chain (2026-07-03): latest non-NA year per region (PT16-18, PL43, PL62) → NUTS-1 replicated (all BE) → national replicated (all NL) — legitimate for an intensity, mirrors the QoG NUTS-1 rule | `create_regional_berd` |
| QoG_Index | EQI **2024 wave**, standalone regional release `qog_eqi_long_24.csv` (Charron et al., University of Gothenburg) | NUTS-2 direct where surveyed at NUTS-2 (NUTS-2021 codes); BE and DE are surveyed at NUTS-1 → replicated to their NUTS-2 regions; single-NUTS-2 states (CY, EE, LU, LV, MT) carry the national score | `create_qog` |
| Climate_Mitigation_Laws | QoG Environmental Indicators (`ccl_nmitlp`, latest year per country) | National replicated to NUTS-2 | `create_climate_laws` |
| Sector_Concentration | Derived from `sbs_r_nuts2021` (share of the focal NACE sub-sector in the region's manufacturing employment) | NUTS-2 × Sector | `create_sector_concentration` |
| RE_Potential | JRC ENSPRESO (wind onshore + solar + biomass, medium scenario, TWh); ENSPRESO's pre-2016 NUTS codes are mapped to NUTS-2021 (2026-07-03): 26 geometrically verified 1:1 renames (all FR, five PL) + polygon-area-share splits for PL12→PL91/92, HU10→HU11/12, LT00→LT01/02, IE01+IE02→IE04/05/06 — before the fix these regions were silently country-median-imputed | NUTS-2 | `create_re_potential` |
| Cohesion_Fund | EU Cohesion Open Data API (ERDF+CF+ESF 2014-2020 MFF, regionalised) ÷ NUTS-2 population from `demo_r_d2jan` (2020) | NUTS-2 | `create_cohesion_fund` |
| Regional_Innovation | EC Regional Innovation Scoreboard — RIS 2025 edition database, Regional Innovation Index (RII) for year 2024, "relative to EU 2018"; `TECH-RIS.xlsx` is regenerated from the official URL by `data_builders/build_tech_ris.R` (2026-07-03 — the former manual RIS-2023 extract left the five single-region countries empty) | NUTS-2 (countries published at NUTS-1 are replicated to their NUTS-2; native NL/PT/HR codes recombined in reshape) | `data_builders/build_tech_ris.R` |
| Policy_Pressure | Hard-coded ETS + CBAM coverage scores per NACE sector. **Computed for reference only — in NO composite** (a per-sector constant cancels exactly under within-sector normalisation, §10.2; ETS/CBAM coverage enters the headline TRI as covered carbon volume, §10.1) | Constant per sector | `create_policy_pressure` |

Covered-carbon Exposure inputs (headline TRI, §10.1):

| Input | Source | Geo level | Function / target |
|---|---|---|---|
| ETS verified emissions (2023, tonnes) | EUTL via EUETS.info (Abrell), installation-level, Google-geocoded; built once by `data_builders/ets_geocode.R` → `Initial data/EUTL_euets_info/ets_nuts2_sector.csv`. Manufacturing = Annex-I process activities 21–44 (mapped by activity code) **plus activity-20 fuel-combustion installations attributed via the installation NACE code (divisions 10–33)**; NACE-35 power/heat and combustion installations without a NACE code are excluded | Installation → NUTS-2 (point-in-polygon, NUTS-3 carried) | `read_ets_nuts2` → `ets_geo` |
| CBAM embodied-import carbon | FIGARO `naio_10_fcp_ii4` + `env_ac_ghgfp` (cache written by `create_scope3`); covered goods C20/C23/C24, charged to all importing sectors | National → NUTS-2 by the importing sector's **employment shares** for all sectors — the pipeline's canonical downscaling rule (§5), adopted 2026-07-03 after the external trade validation (§14). The former plant-emission hybrid is retained only as the `cbam_leg_hybrid` sensitivity variant | `compute_cbam_leg` → `cbam_leg` |

External (non-Eurostat) sources requiring manual download:
- QoG Environmental Indicators (`qog_ei_eureg.csv`) — University of Gothenburg
- EQI standalone regional release (`qog_eqi_long_24.csv`) — same; small, committed to the repo
- JRC ENSPRESO regional renewable potentials
- EC Regional Innovation Scoreboard xlsx
- EUTL installation database (EUETS.info bulk zip; re-download URL in `Initial data/EUTL_euets_info/README.md` — only the derived CSVs are committed)

## 4. Year selection

Each creator function calls `pick_latest_complete_year()` (`R/utils.R`) on a five-year window of Eurostat data and selects the **latest year with EU-27 coverage ≥ 95%** on the relevant geo (× sector) dimension. If no year meets the threshold within the five-year window, the year with the highest observed coverage is returned and the missing cells are logged.

Per-cell fallback: where a region has data in an earlier year but is missing in the selected year (e.g. Latvia in `sbs_r_nuts2021` 2023), the most recent non-NA value is used — since 2026-07-09 this rule is implemented inside `.fetch_nuts2_latest()` itself, so it covers every NUTS-2-level indicator (before, only `create_employment_weights` had it). Where a region is missing from the indicator dataset entirely but the country has a value at the next-broader geo level (e.g. Ireland's EQI 2017 stored under NUTS-2013 codes), the country-level value replaces the missing NUTS-2 value (Section 7 — QoG fallback chain).

**NUTS-2024 recoding (2026-07-09).** Eurostat vintages from 2024 onwards arrive in NUTS-2024 codes: Utrecht NL31→NL35, Zuid-Holland NL33→NL36, and Portugal's PT16/PT17/PT18 split into PT19/PT1A–PT1D. `.fetch_nuts2_latest()` recodes these back onto the pipeline's NUTS-2021 grid *before* its grid filter (merged PT codes aggregated with the indicator's rule — mean for rates, sum for GFCF). Before the fix the filter silently dropped the recoded regions, so Utrecht, Zuid-Holland and the three Portuguese regions carried country-median-imputed values for every indicator fetched through this helper (the three Labour indicators and GFCF).

Special cases (year not driven by the helper):
- **Scope 3** uses `min(dataEnd(naio_10_fcp_ii4), dataEnd(env_ac_ghgfp))` — both FIGARO tables must align at the same vintage for the MRIO computation to be coherent.
- **QoG_Index** uses the **2024 EQI survey wave** (Charron et al.) — the latest of the five published rounds (2010, 2013, 2017, 2021, 2024) in the standalone regional release.
- **Cohesion_Fund** uses the fixed 2014–2020 Multiannual Financial Framework totals (not annual data).
- **Policy_Pressure** is hard-coded per NACE sector.

The chosen year per indicator is recorded in `Final data/Coverage_Report.xlsx` after every pipeline run.

## 5. Downscaling: national → NUTS-2

A single canonical rule governs every indicator whose Eurostat source is national:

- **Extensive quantities** (€, kt CO2eq, GWh, persons, counts) → **employment-share downscaling**. For each (Country, Sector) the national value is split across the country's NUTS-2 regions in proportion to the region's share of national manufacturing employment in that sector. Implemented by `downscale_national_to_nuts2()` in `R/utils.R`, using the `weight` column of the `empl_weights` target (region's pers_employed in that Country × Sector divided by national total, re-normalised to sum to 1 across regions). For Sector "C" (Total Manufacturing) the weight is the region's share of total manufacturing employment.

  Where a sector-specific weight is missing (e.g. Luxembourg has zero C24 employment in `sbs_r_nuts2021` but reports C24 emissions in `env_ac_ainah_r2`), the helper falls back to the country's Sector-C weight for that NUTS-2. The fallback weights are **not renormalised**, so a country×sector containing filled cells allocates slightly more than its national total (on the CBAM component: panel sum 259.3 Mt vs 258.6 Mt of pre-downscale FIGARO national totals, +0.3%) — accepted so that no region with real activity is dropped; national totals are preserved exactly wherever every region carries its own weight.

- **Intensive quantities** (ratios, indices, percentages) → **uniform replication**. Each NUTS-2 region of a country receives the same national value. Implemented by `replicate_national_to_nuts2()` in `R/utils.R`. Applies to Fossil_Share, Renewables_Share, and Capital_Stock_Based_Prod.

Indicators already published at NUTS-2 (GFCF, Unemployment, Labour Slack, HRST, RE Potential, HHI, Regional Innovation, EQI when at NUTS-2 level) are used directly without downscaling.

## 6. Per-employee normalisation

**Gross_Fixed_Capital_Formation** is converted to per-employee intensity before min-max normalisation, in `R/04_normalize.R` (`to_per_empl`), using the NUTS-2 × Sector manufacturing employment count from `empl_weights`. **BERD is no longer per-employee:** it is now a regional R&D *intensity* (business-sector R&D as % of regional GDP, `rd_e_gerdreg` via `create_regional_berd`), already intensive. The previous national-BERD-by-employment series reduced algebraically to a country × sector constant (the downscale weight and per-employee divisor cancel), carrying no within-country variation; the regional source fixes this (see `Review/LITERATURE_GATHERED.md` §H).

Other extensive indicators (Scope1_Emissions, Scope2_Emissions, Scope3_Emissions, Energy_Consumption, Import_ExtraEU, Export_ExtraEU) are NOT divided by employment in this step because they were already downscaled using employment shares (Section 5); dividing again would double-count and inflate small regions.

**Exception — pooled path (headline), 2026-07-09:** in the pooled normalisation (`pool = TRUE`, used only by the headline's Vulnerability) **Energy_Consumption** *is* divided by cell employment. The audit showed the employment-downscaled volume is >0.99-correlated with employment within every country × sector — its within-country "regional variation" is pure size, a vulnerability artifact of the same class as the dropped Sector_Concentration. Divided by employment it reduces algebraically to the sector's national energy intensity (a country × sector constant, the same information class as Fossil_Share); cells without employment become NA and take the group median. The legacy within-sector baseline keeps the volume, as submitted.

## 7. Winsorisation

GFCF per employee is winsorised at the **99th percentile within each (Indicator × Sector_ID)** group (`winsorize_upper(Value, p=0.99)` in `R/04_normalize.R`). This caps the Ireland multinational profit-shifting distortion that the JRC Handbook on Constructing Composite Indicators (2008) explicitly flags. GFCF is not part of the TRI composite (see §11), but it is published in the normalised reference tables and the cap prevents the headline numbers from being dominated by Ireland's MNC accounting. BERD is **not** winsorised: it is now a bounded regional R&D intensity (business R&D as % of regional GDP), where the high values are bona fide R&D-intensive regions (e.g. Stuttgart, Oberbayern, Île-de-France) with no profit-shifting inflation comparable to Ireland's GFCF. The cap is computed from values of the same indicator only; no other indicators are winsorised.

## 8. Min-max normalisation to [0.01, 0.99]

Each indicator's raw value is mapped to the interval [0.01, 0.99] using
$$V_N = 0.01 + 0.98 \cdot \frac{V - \min(V)}{\max(V) - \min(V)}$$

Special cases (in `R/04_normalize.R::.compute_value_n()`):
- $V = \min$ → $V_N = 0.01$
- $V = \max$ → $V_N = 0.99$
- $\max - \min = 0$ (constant indicator) → $V_N = 0.5$ for all
- $V = 0$ on Scope1_Emissions → $V_N = 0$ (preserves true zeros)
- $V$ is NA → $V_N$ is NA

### Grouping for normalisation
- **Policy_Pressure** is a sector-level constant (every NUTS-2 region in sector $s$ has the same raw value). Normalising within (Indicator × Sector) would collapse min = max → 0.5. It is therefore normalised across all (Sector × NUTS-2) values together (group by `Indicator` only).
- **All other indicators** are normalised within (Indicator × Sector_ID) so each sector's regional ranking is comparable.

### Orientation reversal
Indicators where higher raw value = lower vulnerability are reversed after the min-max step: $V_N \leftarrow 1 - V_N$. The "positive" list (NOT reversed, higher raw = higher vulnerability contribution) is:

`Scope1_Emissions, Scope2_Emissions, Scope3_Emissions, Policy_Pressure, Energy_Consumption, Fossil_Share, Unemployment_Rate, Labour_Market_Slack, Export_ExtraEU, Import_ExtraEU, Sector_Concentration`

All other indicators (Renewables_Share, RE_Potential, GFCF, BERD, Regional_Innovation, QoG_Index, Climate_Mitigation_Laws, Cohesion_Fund, Capital_Stock_Based_Prod, Highly_Skilled_Workers) are reversed.

## 9. Vulnerability dimensions

The **legacy baseline** builds Vulnerability from six dimensions, each a row-mean of its constituent normalised indicators (the headline TRI uses four of them — §9.1). Defined in `R/05_aggregate.R::dimensions`:

| Dimension | Indicators | Notes |
|---|---|---|
| **Energy** | Energy_Consumption, Fossil_Share, Renewables_Share, RE_Potential | |
| **Labour** | Unemployment_Rate, Labour_Market_Slack, Highly_Skilled_Workers | |
| **Supply_Chain** | Import_ExtraEU | Export_ExtraEU intentionally excluded — see §11 |
| **Technology** | BERD (regional R&D intensity), Regional_Innovation | BERD = business R&D as % of regional GDP (`rd_e_gerdreg`, NUTS-2 via `create_regional_berd`), replacing the national-BERD-by-employment series that carried no within-country variation; the regional-resilience literature measures the innovation channel with NUTS-2 R&D/patents (Bristow & Healy 2018; Filippetti 2020 — see `Review/LITERATURE_GATHERED.md` §H). Also operationalises the "financing-for-innovation" channel a standalone Finance dimension would otherwise carry — see §11 |
| **Institutions** | QoG_Index, Climate_Mitigation_Laws | EQI partially captures financial-governance quality |
| **Diversification** | Sector_Concentration (focal sector's share of regional manufacturing employment; sector-specific by construction) | Replaces an earlier region-only Herfindahl-Hirschman index that did not vary across sectors |

A standalone **Finance** dimension is **not** included. The rationale is in §11.

Each dimension score is re-normalised to [0.01, 0.99] within Sector_ID. Vulnerability is the row-mean of the six dimension scores, re-normalised to [0.01, 0.99] within Sector_ID.

NAs are imputed cell-by-cell at the indicator stage from the country × sector median (`R/utils.R::impute_with_median`) before the dimension means are computed. Remaining NAs (where the country has no other regions to draw a median from) propagate via `rowMeans(na.rm = TRUE)`.

### 9.1 Vulnerability for the headline TRI (`vulnerability_pooled`)

The headline TRI uses a **pooled, 4-dimension** variant (`build_vulnerability_pooled()` in `R/exposure.R`): **Energy, Labour, Technology, Institutions**.

- Indicators are normalised **pooled across sectors** (`pool = TRUE` in `normalize_indicators()`), so Vulnerability lives on the same cross-sector scale as the pooled Exposure (the two enter $\sqrt{E}\cdot\sqrt{V}$ symmetrically).
- **Supply_Chain (= Import_ExtraEU) is dropped.** Extra-EU imports are already priced in the CBAM component of Exposure; keeping them in Vulnerability would count imports on both sides of the geometric mean.
- **Diversification (= Sector_Concentration) is dropped (2026-07-03, decision with the supervisor).** Under pooled normalisation the indicator encodes which sectors are large everywhere (sector means: food 0.27, fabricated metals & machinery 0.29, vs chemicals 0.07) — sector composition, not regional specialisation — and it was a single-indicator pillar. Sector_Concentration remains computed and stays a dimension of the legacy baseline (§9), where the within-sector normalisation removes the sector-size component.
- Same NA imputation; dimension scores are row-means of the pooled indicator values, and **each dimension is re-normalised to [0,1] before the final average** — the pooled analogue of §9's per-dimension re-normalisation. Without this step the dimensions entered the mean with their raw variances and effective influence was very unequal (2026-07-03 audit: correlation with the composite 0.81 for Technology vs 0.11 for Energy). Vulnerability is then the dimension row-mean normalised at the top level with the scheme matching `tri_norm_mode` ("rank" → percentile rank, otherwise min-max — the log step applies only to the skewed raw exposure volume, not to a bounded mean).

## 10. Exposure

### 10.1 Headline: covered-carbon Exposure (`R/exposure.R`)

Exposure is the **covered carbon per regional manufacturing job** (adopted 2026-07-09; `tri_exposure_denom = "per_employee"`): the cell's ETS verified emissions plus the embodied carbon in its extra-EU imports of CBAM-covered goods, divided by the **region's total manufacturing employment**:

$$\text{Exposure\_raw}_{r,s} = \underbrace{Q^{ETS}_{r,s}}_{\text{ETS component}} + \underbrace{Q^{CBAM}_{r,s}}_{\text{CBAM component}} \qquad (\text{tonnes CO}_2)$$

$$\text{Exposure}_{r,s} = \text{range01}\!\left(\frac{\text{Exposure\_raw}_{r,s}}{L_{r}}\right) \quad \text{POOLED across all } (r,s) \text{ cells}$$

with $L_r$ the region's manufacturing employment (`sbs_r_nuts2021`, sum over the 11 sub-sectors; minimum in the grid 3,005 jobs, so no fallback is needed). The intensity form removes the size component — a region is not exposed merely for being big; it is exposed where its industrial employment base carries a lot of regulated carbon per job.

**Why the regional denominator, not the cell's own sector employment.** The natural first choice, cell-level sector employment, was implemented and rejected on verified evidence: NUTS-2 *sector* employment fails in four distinct ways, all biasing the same direction (denominators shrink exactly at plant regions, inflating intensity there):
1. **Headquarters attribution** — EL65 reports 43 refining employees against the ~2 Mt Corinth refinery, while 80% of Greek C19 employment (4,161 of 5,180) sits in Attiki;
2. **Confidentiality suppression as missing rows** — NL32 C24 (IJmuiden) and ES62 C19 (Repsol Cartagena) are simply absent from `sbs_r_nuts2021`, so division sums silently treat them as zero (Zeeland's C19-C20 employment = its C20 only);
3. **Definitional contractor exclusion** — SBS "persons employed" *excludes manpower supplied by other enterprises* (Eurostat SBS glossary); supplied plant contractors are booked under NACE 78.2, so capital-intensive outsourced plants report skeleton payrolls (Tata IJmuiden: >9,200 site staff per the operator vs no publishable SBS cell);
4. **Reporting volatility at constant plants** — one-year swings such as PT11 C19 462 → 55, EL52 184 → 875, ITG2 496 → 1,698, which would translate 1:1 into exposure swings with unchanged emissions.
Regional manufacturing *totals* have none of these pathologies. Under the cell-level variant the Corinth artifact alone pinned the pooled min-max scale; the regional denominator reproduces its ranking broadly (ρ = 0.94) while removing the artifacts. Consequences of the shared within-region denominator: the cross-sector pattern *inside* a region follows the raw tonnes, and the CBAM component varies regionally with the sector's weight in the region's industrial base (no longer a country × sector constant as under the cell denominator).

The raw tonnes remain published (`exposure_ETS`, `exposure_CBAM`, `exposure_total`) next to the intensity (`exposure_per_empl`, tCO2 per regional manufacturing job; `empl_int` = $L_r$). **The named alternative — no division at all (raw volume)** — is retained as a sensitivity row (ρ = 0.88 vs the headline, §14): volume answers "where is the most regulated carbon", the intensity answers "where is the regional industrial employment base most carbon-burdened".

**Disclosed limits.** (i) The intensity distribution remains right-skewed (real single-plant burdens: Zeeland 397, Sardegna 196 tCO2 per job at the "C" level), so under the linear pooled min-max most cells sit low on the scale (96% of positive cells < 0.05) — the `log`/`rank` normalisation variants are reported in §14 (ρ ≈ 0.78) and revisiting the normalisation remains on the agenda. (ii) Contractors remain excluded from SBS employment at every aggregation level (supplied manpower sits in NACE 78 and services), so intensity *levels* are somewhat overstated throughout. The regional denominator removes the plant-driven differential (a single outsourced plant can no longer distort its own cell) and any within-region cross-sector distortion; a residual **cross-region** differential — regions whose manufacturing relies systematically more on agency/contractor labour carry slightly understated denominators — remains and is disclosed rather than corrected (a worker-side LFS cross-check is the candidate SI test; the regional LFS series requires careful dimension handling before it can serve).

- $Q^{ETS}$: EUTL installation-level **verified emissions** (2023, tonnes), placed at NUTS-2 by point-in-polygon on the geocoded plant coordinates — real production geography, not employment downscaling. Covered installations are the Annex-I process activities 21–44 (which map to the four heavy sectors C16-C18, C19-C20, C23, C24) **plus activity-20 fuel-combustion installations attributed to their manufacturing sector via the installation NACE code** — so every sub-sector can carry plant-level ETS emissions (e.g. food-industry boilers in C10-C12, vehicle-plant power stations in C29-C30), while NACE-35 power/heat stays excluded.
- $Q^{CBAM}$: embodied direct carbon in **extra-EU imports of covered goods** (FIGARO MRIO; origin industries C20/C23/C24 as the covered-goods proxy), charged to **every importing manufacturing sector** — CBAM falls on whoever imports covered inputs, which is why all 11 sub-sectors carry exposure. Caveat: FIGARO 2-digit industries are broader than the exact CBAM goods list, so the CBAM component is an upper bound on covered import carbon. This sector-MRIO estimation is standard practice (EEBT/MRIO — Kanemoto et al. 2012; Cadarso et al. 2018); narrowing to the exact CBAM product list would need product/CN-level trade data outside this composite's Eurostat-MRIO scope. See `Review/EXPOSURE_CARBON_COST_REVISION.md` §23 and `EXPOSURE_CARBON_COST_LITERATURE.md` §5b for the literature justification and an implemented full-embodied-intensity robustness variant (`compute_cbam_leg(intensity = "embodied")`), ranking-robust at NUTS-2×sector Spearman ≈ 0.99 vs the direct headline.
- **No carbon price.** The two components enter as raw tonnes CO2. A price would be a single EU-wide EUA scalar (there is no sub-national carbon price), which cancels exactly under min-max — and monotonically under log/rank — so it never changed the ranking or the E×V interaction inside Risk. The earlier priced formulation (EUA × free-allocation and CBAM-coverage factors) and its legislated 2024–2034 phase-in trajectory were therefore removed. All covered carbon is counted in full (both components, no free-allocation discount, no phase-in ramp). The published exposure columns are `exposure_ETS`, `exposure_CBAM`, and `exposure_total` (tonnes CO2).

**Normalisation (`tri_norm_mode = "minmax"`).** The headline applies plain pooled min-max (linear `range01`), consistent with the min-max used at the indicator level ("min-max everywhere"). Because the raw carbon volume spans orders of magnitude, the linear scale concentrates most cells near 0 (Exposure sd ≈ 0.05) — two alternatives are therefore wired and reported in the sensitivity workbook: `"log"` (`range01(log1p(.))`, sd ≈ 0.25) and `"rank"` (percentile rank — uniform spread). All three preserve the identical cell-level *ordering* of Exposure; the choice affects spacing, hence map readability and the E×V interaction inside Risk. Switch by editing the `tri_norm_mode` target in `_targets.R`. True-zero cells keep Exposure = 0 under every scheme (→ "Zero Risk" band).

**Why pooled (cross-sector) normalisation.** Exposure is normalised across all region × sector cells together, not within each sector, so carbon volumes stay comparable across sectors: a cell's score reflects its covered carbon relative to *all* manufacturing, and genuinely high-volume activities (steel C24, cement C23, refining/chemicals C19-C20) score high while the light sectors score low. Within-sector min-max would instead rescale each sector internally — giving every sector its own top-Exposure cells and erasing the cross-sector volume gradient that is the point of the measure. `assemble_exposure(within_sector = TRUE)` computes the within-sector variant for comparison.

### 10.2 Raw-emissions baseline Exposure (`risk_data_raw_emissions`)

Exposure is the row-mean of three indicators, re-normalised to [0.01, 0.99] within Sector_ID:

`Exposure = mean(Scope1_Emissions, Scope2_Emissions, Scope3_Emissions)`

The same NA imputation as Vulnerability applies. **Policy_Pressure was removed from this composite** (2026-06): it is a per-sector constant, and any per-sector constant cancels exactly under the within-sector range01 re-scaling — its presence or absence provably never changed the index. The indicator is still computed and stored for reference.

## 11. Indicators excluded from the composite (intentional design choices)

Four indicators are computed and stored in the `Initial data/` xlsx files for reference and reproducibility but are NOT used in the TRI composite, and the Finance dimension that the first three would feed is itself dropped from Vulnerability:

- **Gross_Fixed_Capital_Formation (GFCF)**: measures realised investment *volume* (NUTS-2, NACE C-aggregate only, per `nama_10r_2gfcf`), not access to transition finance. The raw series is dominated by Ireland's multinational profit-shifting accounting (which is the reason a p99 winsorisation is applied in §7 — itself a flag, per the JRC Handbook (Nardo et al. 2008), that the indicator is structurally distorted) and the source has no NACE 2-digit disaggregation, so the same regional GFCF number is replicated across all eleven manufacturing sub-sectors. The indicator is therefore *not* a meaningful sectoral discriminator of transition-finance access. We retain the data file for reference and reproducibility but exclude it from the composite.
- **Cohesion_Fund**: orientation is ambiguous because cohesion transfers simultaneously signal regional need AND provide support. Treating high transfers as "more resilient" makes Cohesion-recipient regions look less vulnerable than non-recipients, contradicting the literature on regional economic vulnerability. Excluded.
- **Capital_Stock_Based_Prod**: the `NCS_HW / N11N / I20` index measures capital per hour worked indexed to 2020 = 100. Direction is ambiguous — growth could mean modernisation (good) or labour exodus / hours collapsing (bad); decline could mean stranded-asset write-offs (bad) or labour absorption (good). Reviewer 2 of 25CP7059-RA flagged this ambiguity. Excluded.
- **Export_ExtraEU**: Reviewer 2 noted that "being embedded in global supply chains may also be positive as it may drive innovation and allow a company to tap into new markets more easily". Export access is treated as a resilience factor with ambiguous direction for vulnerability. Excluded. Only Import_ExtraEU is used in the Supply_Chain dimension (legacy baseline only — the headline TRI drops the dimension entirely, §9.1).
- **Wage_per_h** (present in the first submission): removed from the pipeline entirely. Reviewer 2 noted that high wages can *impede* labour mobility when alternative industries pay less, making the direction ambiguous (high wages = skilled, adaptable workforce vs. high wages = costly reallocation). With no defensible orientation, the indicator is out; workforce adaptability is carried by Highly_Skilled_Workers, Unemployment_Rate, and Labour_Market_Slack.

### 11.1 Why there is no standalone Finance dimension

A previous version of the index carried a Finance dimension built solely from GFCF (per employee, p99-capped). On methodological review we cannot defend it for four reasons:

1. **No clean regional × sectoral indicator of *access to* transition finance exists in Eurostat or in the listed external sources.** GFCF captures realised investment volume, not access; the three other candidates we evaluated (cohesion transfers, capital-stock index, tangible/intangible investment) each fail on direction unambiguity or on granularity. ECB MFI loan series are euro-area-only; the EIB Investment Survey (EIBIS) is representative only at country × broad sector group. The Just Transition Fund's national envelopes are themselves a function of regional carbon-intensity and employment in carbon-intensive sectors — i.e. the same variables that feed the Exposure side of the TRI — so using JTF allocations would induce mechanical correlation with Exposure.
2. **Comparable regional EU transition-risk composites do not carry a standalone Finance dimension.** Vrontisi et al. (2024), Sokolowski et al. (2022), and the JRC Coal Regions in Transition framework all structure vulnerability around socioeconomic, energy, and institutional pillars without a separate finance axis.
3. **The JRC Handbook on Constructing Composite Indicators (Nardo et al., 2008; JRC47008) counsels against single-indicator pillars** when the sole indicator's direction is documented as distorted (here: the Ireland MNC issue, which the previous version masked via winsorisation — itself an acknowledgement of the distortion).
4. **The conceptual content "finance for transition" is partially captured by the remaining dimensions.** BERD in Technology is private R&D investment — i.e. the financing flow that underwrites the technology channel of transition. QoG_Index in Institutions incorporates regional financial-governance quality. The composite is therefore not silent on the financing channel; it simply does not give it its own pillar.

The dimension would only be reintroduced if a clean Eurostat-level NUTS-2 × NACE-division indicator of transition-finance access became available (e.g. an EBS-era sector-specific investment series at `sbs_r_nuts2021` with direction validated against the literature).

## 12. TRI and risk bands

Final risk (both variants):
$$\text{Risk}_{r,s} = \text{Exposure}_{r,s}^{\,0.5} \cdot \text{Vulnerability}_{r,s}^{\,0.5}$$

(Geometric aggregation with equal weights, $\alpha = 0.5$.)

The re-scaling scope follows the variant's normalisation scope:
- **Headline TRI**: Risk_norm = range01(Risk) **pooled** over the 11-sub-sector panel; the "C" roll-up (per-region sum of raw covered carbon; Vulnerability = regional mean of sub-sector scores) is normalised separately across regions.
- **Raw-emissions baseline**: Risk_norm = range01(Risk) within Sector_ID.

When Exposure = 0 (headline: zero covered carbon; baseline: raw Scope1_Emissions = 0), Exposure is set to NA so the risk is undefined (not artificially zero from the geometric formula). Risk_norm is then NA → Risk_Band = "Zero Risk".

A **cell** is one (NUTS-2 region × manufacturing sub-sector) pair — the unit of analysis (§1): 230 regions × 11 sub-sectors = 2,530 cells, plus the 230 "C" roll-up rows. In the headline TRI a **Zero-Risk cell** is a region-sector with no covered carbon at all: no ETS installation of that sector in that region AND no CBAM-covered imports attributed to it (23 of 2,530 cells in the current build; the workbook's Zero-Risk *band* shows 24 rows because one further cell — LU00 × C13-C15 — carries covered carbon but an all-NA Vulnerability, so its Risk is undefined and it lands in the same band). Under the employment-share CBAM downscaling (§14) nearly every region-sector carries some import exposure, so the true zeros reduce to regions with neither an ETS plant nor reported sector employment: the island regions Voreio Aigaio (EL41) and Ionia Nisia (EL62) across all 11 sub-sectors, plus Malta × C24. (Under the former plant-emission hybrid the count was ~263, concentrated in the four heavy sectors whose CBAM share then followed the plant geography.)

**Reporting convention:** descriptive statistics by sector (e.g. mean Exposure per sector) are computed on **positive cells only**, with Zero-Risk cells reported as their own category (count per sector). Mixing the zeros into a sector mean conflates "the sector carries little covered carbon here" with "the sector's covered activity does not exist here", and mechanically deflates the heavy sectors, which have many plant-less regions.

Risk bands are equal-width quintiles on $[0, 1]$:

| Risk_norm range | Band |
|---|---|
| [0.0, 0.2] | Very Low |
| (0.2, 0.4] | Low |
| (0.4, 0.6] | Medium |
| (0.6, 0.8] | High |
| (0.8, 1.0] | Very High |

## 13. Pipeline

The full pipeline is orchestrated by `_targets.R` using the `{targets}` R package. Each Phase below corresponds to a contiguous block of targets:

- **Phase 1 — Create initial data**: every creator function (`create_*`) pulls Eurostat data, picks its year, downscales if needed, and writes one xlsx per indicator under `Initial data/`.
- **Phase 2 — Harmonise** (`R/02_harmonize.R`): reads the per-indicator xlsx files and joins them into one long tibble (region × sector × indicator).
- **Phase 3 — Reshape** (`R/03_reshape.R`): expands to a complete grid of (region × sector × indicator), handles NUTS recombinations, applies `agg_rules` (sum vs mean) for aggregated regions.
- **Phase 4 — Normalise** (`R/04_normalize.R`): per-employee division, winsorisation, min-max normalisation, orientation reversal.
- **Phase 5 — Risk index (headline)** (`R/exposure.R`): EUTL input (`ets_geo`), FIGARO cache (`figaro_cache`), CBAM components (`cbam_leg` employment-weighted headline, `cbam_leg_embodied`, `cbam_leg_hybrid` sensitivity), pooled Vulnerability (`vulnerability_pooled`), headline index (`risk_data`) → `Final data/Risk_data.xlsx` (`save_risk_xlsx`).
- **Phase 6 — Raw-emissions index (comparison)** (`R/05_aggregate.R`): NA imputation, Exposure / Vulnerability dimensions / TRI (`risk_data_raw_emissions`) → `Final data/Risk_data_raw_emissions.xlsx` (`save_risk_raw_emissions_xlsx`).
- **Phase 7 — Save raw data**: `Raw_data_not_normalized.xlsx` (`Coverage_Report.xlsx` is written in Phase 1).
- **Phase 8 — Figures**: maps, radars, headline three-panel map (`plot_risk_maps`) — all on the headline `risk_data` (PNG, 600 DPI).
- **Phase 9 — Sensitivity**: baseline battery (`run_sensitivity`, on the raw-emissions index) + headline comparisons (`run_risk_sensitivity`) → `Sensitivity_Analysis.xlsx`.
- **Phase 10 — Insights**: top/bottom table (`Top_Bottom_Regions_per_Sector.xlsx`), quadrants, within-country variance, cluster maps — all on the headline `risk_data`.

## 14. Sensitivity analysis

`Final data/Sensitivity_Analysis.xlsx` carries two sheets:

- **`baseline_battery`** — `R/07_sensitivity.R::run_sensitivity()`: the raw-emissions TRI against six families of alternative construction choices (list below).
- **`risk_tri`** — `run_risk_sensitivity()` (`R/exposure.R`): the headline TRI vs the raw-emissions baseline, vs its `log` and `rank` normalisation variants, vs the **full-embodied CBAM intensity** (target `cbam_leg_embodied`; ρ ≈ 0.99 both ways — ranking-robust), vs the **plant-emission hybrid CBAM allocation** (ρ ≈ 0.99), and — since 2026-07-09 — the **exposure denominator rows**: per regional-manufacturing-job (headline) vs raw volume, i.e. no division (ρ ≈ 0.88 pooled: the intensity re-ranking is real but bounded), and vs per-regional-GDP (ρ ≈ 0.97; regional *total* GDP, since sub-sector GVA does not exist at NUTS-2 — closely tracks the employment denominator because regional GDP and manufacturing employment are correlated). A per-GDP-per-capita denominator was evaluated and rejected: tonnes ÷ (€/person) is not an intensity of anything real, and dividing Exposure by a wealth variable double-counts the adaptive-capacity information that Vulnerability already carries.

**External validation of the CBAM regional allocation — and the resulting weight choice** (`validation/cbam_trade_validation.R`): the modeled region shares of national extra-EU covered-good imports (FIGARO 2023 flows × the downscaling weights) are compared with observed regional trade statistics. Against the available anchor — ISTAT extra-EU imports by region, CPA section C, 2023 (`validation/observed_imports_IT_sectionC_2023.csv`; the regional SDMX flows publish CPA sections only) — the **employment-share weights fit at Spearman ρ = 0.91** (21 Italian regions) and respect the physical ceiling that a region's covered-good imports cannot exceed its total observed manufacturing imports (only micro-region Valle d'Aosta stays marginally above, 1.26× on ~€0.1bn). The former **plant-emission hybrid** (plant shares in the four heavy sectors) fit at ρ = 0.69 and breached the ceiling in Sardegna (3.4×) and Puglia (1.4×), marginally in Sicilia (1.07×), while under-allocating import/logistics hubs (Lombardia −12.5 pp, Lazio −7.5 pp) — emission shares over-weight primary producers relative to where imports actually arrive. **The headline therefore uses employment shares for all sectors (adopted 2026-07-03)** — also the conceptually right proxy (CBAM is charged to the importing users of covered inputs, and employment locates the using industry) and the pipeline's canonical downscaling rule (§5); the hybrid is retained as the `cbam_leg_hybrid` sensitivity variant. Caveats cut both ways: observed trade is attributed to the declarant/destination region (imports declared at a headquarters shift away from the plant region), and FIGARO MRIO valuations differ from customs cif. Division-level observed data (CPA 20/23/24 by region; export recipes in the script header) sharpen the test.

Every comparison reports **two Spearman columns**: `rho_pooled` (stacked region × sector panel) and `rho_within_sector` (mean of per-sector correlations). The within-sector mean is the honest statistic for the within-sector-normalised baseline — stacking sector panels that were each normalised within sector inflates pooled agreement; the pooled column is the relevant one for the pooled (cross-sector) headline TRI.

The six baseline families:

1. **Alpha sensitivity**: re-compute Risk with $\alpha \in \{0.30, 0.40, 0.50, 0.60, 0.70\}$ in $\text{Risk} = E^{\alpha} \cdot V^{1-\alpha}$. Default $\alpha = 0.50$.
2. **PCA weights for Vulnerability**: use the absolute PC1 loadings of the six Vuln_* dimensions (re-normalised to sum to 1) as dimension weights, instead of the equal-weight row-mean.
3. **Leave-one-dimension-out**: drop each Vuln_* dimension one at a time and re-compute Vulnerability as the mean of the remaining five.
4. **Geometric vs arithmetic aggregation**: replace $E^{0.5} \cdot V^{0.5}$ with $0.5 \cdot E + 0.5 \cdot V$.
5. **Min-max vs z-score normalisation**: re-normalise Exposure and each Vuln_* dimension as z-scores within Sector_ID before composing the TRI.
6. **EDGAR Scope 1 alternative** (sectors C19-C20, C23, C24 only): replace the Eurostat-downscaled `Scope1_Emissions` with values derived from EDGAR 2024 0.1° gridded CO2 maps aggregated to NUTS-2 polygons. IPCC sector crosswalk: `REF_TRF + CHE → C19-C20`, `NMM → C23`, `IRO → C24`. Only the three heavy-emitter sectors are tested because EDGAR's facility-aware spatial proxies are most likely to differ from employment-share downscaling for point-source-dominated industries. Implemented in `R/08_sensitivity_alt.R::compute_edgar_scope1()`.

## 15. Data vintages and documented quirks

The Eurostat indicators each pick their own latest complete year (§4; audit log in `Final data/Coverage_Report.xlsx` — currently 2023–2025 depending on the indicator). The non-Eurostat inputs have fixed vintages:

| Input | Vintage | Note |
|---|---|---|
| EUTL verified emissions | 2023 | Latest EUETS.info release; matches the FIGARO vintage |
| FIGARO IO + GHG (Scope 3, CBAM component) | 2023 | `min(dataEnd)` of the two tables |
| Scope-2 grid emission factors | 2022 (EEA/Ember) | Hard-coded in `create_scope2` |
| EQI (QoG_Index) | 2024 survey wave | Latest published EQI round (standalone regional release) |
| Cohesion Fund | 2014–2020 MFF | Fixed programme totals (not in composite) |

Mixed vintages are inherent to a multi-source composite; the headline carbon components (EUTL 2023 + FIGARO 2023) are deliberately aligned.

Documented data quirks:

- **HR04 (Continentalna Hrvatska)** is rebuilt from HR02+HR05+HR06, except `Unemployment_Rate` and `Capital_Stock_Based_Prod`, which are **copied from HR03** (donor copy in `reshape_to_grid()`): both are intensive indicators with national-level sources in Croatia, so the HR03 value equals the national value that HR04 would receive.
- **DEB2 (Trier) and PL43 (Lubuskie)** report no `lfst_r_lfu3rt` unemployment in any year of the five-year window (small-region suppression) — they carry the country × sector median via the standard imputation. The energy balances' non-specified industry (`FC_IND_NSP_E`) is split equally across C21-C22 / C26-C27 / C31-C33 in `create_energy_consumption` and `create_energy_shares`.
- **Climate_Mitigation_Laws** (`ccl_nmitlp`, Climate Change Laws of the World via QoG Environmental Indicators) is a fixed-vintage cumulative count taken at each country's latest available year in the file — the underlying CCLW compilation ends around 2020–2022 depending on country; treated like the other fixed-vintage external inputs (§15 table).
- All-NA recombination groups stay NA (not 0) for both the Croatia and Portugal aggregations, and `harmonize_sector()` preserves NA when a collapse group is entirely missing (false zeros would otherwise trip the Scope-1 true-zero rule into spurious "Zero Risk" cells). Since 2026-07-03 an all-NA aggregate also no longer **overwrites** a value that already arrives on the recombined code — ENSPRESO delivers HR04 directly, and the upsert used to clobber it with NA.

## 16. Reproducibility

- All Eurostat downloads are scripted (no manual files for any Eurostat indicator). `TECH-RIS.xlsx` is also scripted since 2026-07-03 (`data_builders/build_tech_ris.R` downloads the official RIS 2025 database); no manual-only input remains.
- The pipeline picks the latest year with full coverage at every run, so `tar_make()` on a future date produces an updated index automatically. The exact year per indicator is logged in `Final data/Coverage_Report.xlsx`.
- Committed xlsx files in `Initial data/` and `Final data/` and PNGs in `Figures/` are the published snapshot. `git checkout <submission-tag>` reproduces the exact paper figures and numbers without re-running R.
- `tar_destroy(); tar_make()` refreshes the entire pipeline against the current Eurostat vintage.
