# Transition Risk Index — Methodology

This document specifies the methodology actually implemented by the pipeline. It is the source of truth for the paper and supplementary appendix. Each section maps to a specific stage of the `targets` pipeline.

## 1. Conceptual framework

The Transition Risk Index (TRI) follows the **IPCC AR6 risk framework**:

$$\text{TRI}_{r,s} = \text{Exposure}_{r,s}^{0.5} \times \text{Vulnerability}_{r,s}^{0.5}$$

Indexed over $r$ ∈ EU-27 NUTS-2 regions, $s$ ∈ 12 manufacturing sectors (NACE Rev.2 Section C plus 11 sub-aggregates). Aggregation is geometric so a region cannot have non-zero risk on a single axis alone.

The pipeline produces **two TRI variants**:

1. **Headline — carbon-cost-at-risk TRI** (`risk_data_cost` target, §10.1): Exposure is the euro carbon cost a region-sector faces once EU carbon pricing fully bites (EUTL plant-level ETS cost + CBAM cost on embodied imports, priced at full 2026–2034 phase-in), normalised **pooled across all region × sector cells**; Vulnerability is the pooled 5-dimension adaptive-capacity composite (§9.1). The pooled (cross-sector) normalisation is what allows a sector-level price signal to survive: under within-sector min-max any per-sector constant cancels exactly (§10.2). The legislated 2024→2034 phase-in path is the hazard layer (`cost_trajectory` target, §10.1.4).
2. **Legacy baseline — emissions-based TRI** (`risk_data` target, §10.2): the index as submitted to Climate Policy in Oct-2025 (Scope 1/2/3 emissions exposure, within-sector normalisation, 6 vulnerability dimensions). Retained for comparison; the sensitivity workbook reports the Spearman between the two.

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
| BERD (regional R&D intensity) | `rd_e_gerdreg` (business-sector R&D, % of regional GDP) | NUTS-2 (region-level, replicated across sectors) | `create_regional_berd` |
| QoG_Index | QoG EU Regional Dataset, EQI 2017 wave | NUTS-2 if available; NUTS-0 (national) replicated otherwise; NUTS-0 fallback for unmatched NUTS-2021 codes (Ireland IE04/IE05/IE06) | `create_qog` |
| Climate_Mitigation_Laws | QoG Environmental Indicators (`ccl_nmitlp`, latest year per country) | National replicated to NUTS-2 | `create_climate_laws` |
| Sector_Concentration | Derived from `sbs_r_nuts2021` (share of the focal NACE sub-sector in the region's manufacturing employment) | NUTS-2 × Sector | `create_sector_concentration` |
| RE_Potential | JRC ENSPRESO (wind onshore + solar + biomass, medium scenario, TWh) | NUTS-2 | `create_re_potential` |
| Cohesion_Fund | EU Cohesion Open Data API (ERDF+CF+ESF 2014-2020 MFF, regionalised) ÷ NUTS-2 population from `demo_r_d2jan` (2020) | NUTS-2 | `create_cohesion_fund` |
| Regional_Innovation | EC Regional Innovation Scoreboard (manual download — the only non-scripted input) | NUTS-2 | manual xlsx |
| Policy_Pressure | Hard-coded ETS + CBAM coverage scores per NACE sector. **Computed for reference only — in NO composite** (a per-sector constant cancels exactly under within-sector normalisation, §10.2; carbon pricing enters the headline TRI as a cost, §10.1) | Constant per sector | `create_policy_pressure` |

Carbon-cost exposure inputs (headline TRI, §10.1):

| Input | Source | Geo level | Function / target |
|---|---|---|---|
| ETS verified emissions (2023, tonnes) | EUTL via EUETS.info (Abrell), installation-level, Google-geocoded; built once by `prototypes/ets_geocode.R` → `Initial data/EUTL_euets_info/ets_nuts2_sector.csv` | Installation → NUTS-2 (point-in-polygon, NUTS-3 carried) | `read_ets_nuts2` → `ets_geo` |
| Free-allocation share (2023) | Same EUTL panel: allocatedFree / verified, capped at 1 | Country × sector | `read_ets_freealloc` → `ets_freealloc` |
| CBAM embodied-import carbon | FIGARO `naio_10_fcp_ii4` + `env_ac_ghgfp` (cache written by `create_scope3`); covered goods C20/C23/C24, charged to all importing sectors | National → NUTS-2 (hybrid weights: geocoded plant emission shares for C16-C18/C19-C20/C23/C24, employment shares otherwise) | `compute_cbam_leg` → `cbam_leg` |
| EUA price | €64.8/tCO2 — 2024 annual average (ESMA EU Carbon Markets Report 2024). Common scalar: sets the EUR interpretation, not the ranking | Scalar | `eua_price_eur` |
| CBAM phase-in factor F(t) | Reg. (EU) 2023/956 Art. 31 + revised ETS Directive Art. 10a (2026: 2.5% → 2034: 100%); free allocation = base × (1−F) | Scalar per year | `cbam_phase_factor` |

External (non-Eurostat) sources requiring manual download:
- QoG EU Regional Dataset (`qog_eureg.csv`) — University of Gothenburg
- QoG Environmental Indicators (`qog_ei_eureg.csv`) — same
- JRC ENSPRESO regional renewable potentials
- EC Regional Innovation Scoreboard xlsx
- EUTL installation database (EUETS.info bulk zip; re-download URL in `Initial data/EUTL_euets_info/README.md` — only the derived CSVs are committed)

## 4. Year selection

Each creator function calls `pick_latest_complete_year()` (`R/utils.R`) on a five-year window of Eurostat data and selects the **latest year with EU-27 coverage ≥ 95%** on the relevant geo (× sector) dimension. If no year meets the threshold within the five-year window, the year with the highest observed coverage is returned and the missing cells are logged.

Per-cell fallback: where a region has data in an earlier year but is missing in the selected year (e.g. Latvia in `sbs_r_nuts2021` 2023), the most recent non-NA value is used. Where a region is missing from the indicator dataset entirely but the country has a value at the next-broader geo level (e.g. Ireland's EQI 2017 stored under NUTS-2013 codes), the country-level value replaces the missing NUTS-2 value (Section 7 — QoG fallback chain).

Special cases (year not driven by the helper):
- **Scope 3** uses `min(dataEnd(naio_10_fcp_ii4), dataEnd(env_ac_ghgfp))` — both FIGARO tables must align at the same vintage for the MRIO computation to be coherent.
- **QoG_Index** uses the **2017 EQI survey wave** (Charron et al.). EQI is a triennial survey; 2017 is the latest fully published wave.
- **Cohesion_Fund** uses the fixed 2014–2020 Multiannual Financial Framework totals (not annual data).
- **Policy_Pressure** is hard-coded per NACE sector.

The chosen year per indicator is recorded in `Final data/Coverage_Report.xlsx` after every pipeline run.

## 5. Downscaling: national → NUTS-2

A single canonical rule governs every indicator whose Eurostat source is national:

- **Extensive quantities** (€, kt CO2eq, GWh, persons, counts) → **employment-share downscaling**. For each (Country, Sector) the national value is split across the country's NUTS-2 regions in proportion to the region's share of national manufacturing employment in that sector. Implemented by `downscale_national_to_nuts2()` in `R/utils.R`, using the `weight` column of the `empl_weights` target (region's pers_employed in that Country × Sector divided by national total, re-normalised to sum to 1 across regions). For Sector "C" (Total Manufacturing) the weight is the region's share of total manufacturing employment.

  Where a sector-specific weight is missing (e.g. Luxembourg has zero C24 employment in `sbs_r_nuts2021` but reports C24 emissions in `env_ac_ainah_r2`), the helper falls back to the country's Sector-C weight for that NUTS-2.

- **Intensive quantities** (ratios, indices, percentages) → **uniform replication**. Each NUTS-2 region of a country receives the same national value. Implemented by `replicate_national_to_nuts2()` in `R/utils.R`. Applies to Fossil_Share, Renewables_Share, and Capital_Stock_Based_Prod.

Indicators already published at NUTS-2 (GFCF, Unemployment, Labour Slack, HRST, RE Potential, HHI, Regional Innovation, EQI when at NUTS-2 level) are used directly without downscaling.

## 6. Per-employee normalisation

**Gross_Fixed_Capital_Formation** is converted to per-employee intensity before min-max normalisation, in `R/04_normalize.R` (`to_per_empl`), using the NUTS-2 × Sector manufacturing employment count from `empl_weights`. **BERD is no longer per-employee:** it is now a regional R&D *intensity* (business-sector R&D as % of regional GDP, `rd_e_gerdreg` via `create_regional_berd`), already intensive. The previous national-BERD-by-employment series reduced algebraically to a country × sector constant (the downscale weight and per-employee divisor cancel), carrying no within-country variation; the regional source fixes this (see `Review/LITERATURE_GATHERED.md` §H).

Other extensive indicators (Scope1_Emissions, Scope2_Emissions, Scope3_Emissions, Energy_Consumption, Import_ExtraEU, Export_ExtraEU) are NOT divided by employment in this step because they were already downscaled using employment shares (Section 5); dividing again would double-count and inflate small regions.

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

Vulnerability is built from six dimensions, each a row-mean of its constituent normalised indicators. Defined in `R/05_aggregate.R::dimensions`:

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

### 9.1 Vulnerability for the carbon-cost TRI (`vulnerability_pooled`)

The headline TRI uses a **pooled, 5-dimension** variant (`build_vulnerability_pooled()` in `R/exposure_cost.R`):

- Indicators are normalised **pooled across sectors** (`pool = TRUE` in `normalize_indicators()`), so Vulnerability lives on the same cross-sector scale as the pooled Exposure (the two enter $\sqrt{E}\cdot\sqrt{V}$ symmetrically).
- **Supply_Chain (= Import_ExtraEU) is dropped → 5 dimensions** (Energy, Labour, Technology, Institutions, Diversification). Extra-EU imports are already priced in the CBAM leg of Exposure; keeping them in Vulnerability would count imports on both sides of the geometric mean.
- Same NA imputation; dimension scores are row-means of the pooled indicator values; Vulnerability is the dimension row-mean normalised at the top level with the scheme matching `tri_norm_mode` ("rank" → percentile rank, otherwise min-max — the log step applies only to the skewed raw cost, not to a bounded mean).

## 10. Exposure

### 10.1 Headline: carbon-cost-at-risk (`R/exposure_cost.R`)

Exposure is the **euro carbon cost a region-sector faces once EU carbon pricing fully bites**:

$$\text{Exposure\_raw}_{r,s} = \underbrace{Q^{ETS}_{r,s} \cdot P \cdot (1 - fa_{c,s})}_{\text{ETS leg}} + \underbrace{Q^{CBAM}_{r,s} \cdot P \cdot F}_{\text{CBAM leg}}$$

$$\text{Exposure}_{r,s} = \text{range01}(\text{Exposure\_raw}_{r,s}) \quad \text{POOLED across all } (r,s) \text{ cells}$$

- $Q^{ETS}$: EUTL installation-level **verified emissions** (2023, tonnes), placed at NUTS-2 by point-in-polygon on the geocoded plant coordinates — real production geography, not employment downscaling, for the four ETS sectors (C16-C18, C19-C20, C23, C24).
- $fa_{c,s}$: country × sector **free-allocation share** (allocatedFree / verified, capped at 1 — over-allocated sectors face zero, not negative, cost).
- $Q^{CBAM}$: embodied direct carbon in **extra-EU imports of covered goods** (FIGARO MRIO; origin industries C20/C23/C24 as the covered-goods proxy), charged to **every importing manufacturing sector** — CBAM falls on whoever imports covered inputs, which is why all 11 sub-sectors carry exposure. Caveat: FIGARO 2-digit industries are broader than the exact CBAM goods list and the full EUA price is applied to all embodied import carbon (no netting of any carbon price paid abroad), so the CBAM leg is an upper bound on incidence. This sector-MRIO estimation is standard practice (EEBT/MRIO — Kanemoto et al. 2012; Cadarso et al. 2018); narrowing to the exact CBAM product list would need product/CN-level trade data outside this composite's Eurostat-MRIO scope, and Art. 9 netting is small for the major (low-carbon-price) origins. See `Review/EXPOSURE_CARBON_COST_REVISION.md` §23 and `EXPOSURE_CARBON_COST_LITERATURE.md` §5b for the literature justification and an implemented full-embodied-intensity robustness variant (`compute_cbam_leg(intensity = "embodied")`), which is ranking-robust — NUTS-2×sector Spearman 0.997 vs the direct headline, confirming the choice.
- $P$: EUA price (€64.8/t, 2024 average) — a common scalar, so it sets the EUR magnitudes, not the ranking.
- **Headline policy state: full phase-in** ($fa = 0$, $F = 1$; the 2034 end-state of Reg. 2023/956). At 2024 rules free allocation shields most heavy-industry process emissions and CBAM is not yet charging, so pricing the headline at 2024 would measure almost nothing (~€2bn over ~530 cells). Note the ordinal consequence: at full phase-in the price factors are uniform, so the headline *ranking* is driven by the quantity-and-location structure; the price structure bites in the interior years (`cost_trajectory`, §10.1.4) and in the EUR magnitudes.

**Normalisation (`tri_norm_mode = "minmax"`).** The headline applies plain pooled min-max (linear `range01`), consistent with the min-max used at the indicator level ("min-max everywhere"). Because the raw cost spans orders of magnitude, the linear scale concentrates most cells near 0 (Exposure sd ≈ 0.05) — two alternatives are therefore wired and reported in the sensitivity workbook: `"log"` (`range01(log1p(.))`, sd ≈ 0.25) and `"rank"` (percentile rank — uniform spread). All three preserve the identical cell-level *ordering* of Exposure; the choice affects spacing, hence map readability and the E×V interaction inside Risk. Switch by editing the `tri_norm_mode` target in `_targets.R`. True-zero cost cells keep Exposure = 0 under every scheme (→ "Zero Risk" band).

**Why pooled (cross-sector) normalisation.** A carbon price varies at most by country × sector and has zero within-sector variation. Under the within-sector min-max of §8 any per-sector constant cancels exactly — verified: the old Policy_Pressure indicator had **zero** effect on the baseline TRI (dropping it, or switching + to ×, leaves the index identical). Pooling is what lets policy variation survive into the index; `assemble_exposure_cost(within_sector = TRUE)` reproduces the wash-out as a diagnostic (`prototypes/test_exposure_cost.R`).

#### 10.1.4 Hazard layer: the 2024–2034 phase-in trajectory

`build_cost_trajectory()` re-prices the panel along the legislated path (free allocation = base × (1−F(t)), CBAM coverage = F(t), per Reg. 2023/956): total cost in € bn, ETS share of the cost, number of priced cells, and the Spearman of each year's ranking against the 2034 headline. Written to the `phase_in_trajectory` sheet of `Final data/Sensitivity_Analysis.xlsx`.

### 10.2 Legacy baseline: emissions-based Exposure (`risk_data`)

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
- **Headline (carbon-cost) TRI**: Risk_norm = range01(Risk) **pooled** over the 11-sub-sector panel; the "C" roll-up (per-region sum of raw costs; Vulnerability = regional mean of sub-sector scores) is normalised separately across regions.
- **Legacy baseline**: Risk_norm = range01(Risk) within Sector_ID.

When Exposure = 0 (headline: zero carbon cost; baseline: raw Scope1_Emissions = 0), Exposure is set to NA so the risk is undefined (not artificially zero from the geometric formula). Risk_norm is then NA → Risk_Band = "Zero Risk".

A **cell** is one (NUTS-2 region × manufacturing sub-sector) pair — the unit of analysis (§1): 230 regions × 11 sub-sectors = 2,530 cells, plus the 230 "C" roll-up rows. In the headline TRI a **Zero-Risk cell** is a region-sector with no carbon-cost base at all: no ETS installation of that sector in that region AND no CBAM-covered imports attributed to it (~307 of 2,530 cells). They concentrate in the four ETS sectors (C19-C20, C24, C16-C18, C23) because both cost legs of those sectors follow the geocoded plant geography — a region with no steelworks has no C24 carbon cost. The seven light sectors are downscaled by employment, so nearly every region carries some cost.

**Reporting convention:** descriptive statistics by sector (e.g. mean Exposure per sector) are computed on **positive cells only**, with Zero-Risk cells reported as their own category (count per sector). Mixing the zeros into a sector mean conflates "the sector faces little carbon cost here" with "the sector's priced activity does not exist here", and mechanically deflates the heavy sectors, which have many plant-less regions.

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
- **Phase 5 — Aggregate risk** (`R/05_aggregate.R`): NA imputation, Exposure / Vulnerability dimensions / TRI (legacy baseline).
- **Phase 5b — Carbon-cost TRI** (`R/exposure_cost.R`): EUTL inputs (`ets_geo`, `ets_freealloc`), FIGARO cache (`figaro_cache`), CBAM leg (`cbam_leg`), pooled Vulnerability (`vulnerability_pooled`), headline index (`risk_data_cost`), phase-in trajectory (`cost_trajectory`).
- **Phase 6 — Save** outputs to `Final data/`: `Risk_data_carbon_cost.xlsx` (headline), `Risk_data.xlsx` (legacy baseline), `Raw_data_not_normalized.xlsx`, `Coverage_Report.xlsx`, `Sensitivity_Analysis.xlsx`, `Top_Bottom_Regions_per_Sector.xlsx`.
- **Phase 7 — Figures**: maps, radars, quadrants, within-country variance, cluster maps, carbon-cost three-panel map (`plot_cost_tri_maps`) (PNG, 600 DPI).

## 14. Sensitivity analysis

`Final data/Sensitivity_Analysis.xlsx` carries three sheets:

- **`baseline_battery`** — `R/07_sensitivity.R::run_sensitivity()`: the legacy TRI against six families of alternative construction choices (list below).
- **`carbon_cost_tri`** — `run_sensitivity_cost()` (`R/exposure_cost.R`): the headline cost TRI vs the emissions baseline, vs its `minmax` and `rank` normalisation variants, vs the 2024 policy state (current free allocation, no CBAM), and vs the **full-embodied CBAM intensity** (target `cbam_leg_embodied`; ρ = 0.99 pooled / 0.99 within-sector — the CBAM intensity basis is ranking-robust, §10.1 / REVISION §23).
- **`phase_in_trajectory`** — `build_cost_trajectory()`: total cost (€ bn), ETS share, priced cells, and rank stability along the legislated 2024–2034 phase-in.

Every comparison reports **two Spearman columns**: `rho_pooled` (stacked region × sector panel) and `rho_within_sector` (mean of per-sector correlations). The within-sector mean is the honest statistic for the within-sector-normalised baseline — stacking sector panels that were each normalised within sector inflates pooled agreement; the pooled column is the relevant one for the pooled (cross-sector) cost TRI.

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
| EUTL verified emissions + free allocation | 2023 | Latest EUETS.info release; matches the FIGARO vintage |
| FIGARO IO + GHG (Scope 3, CBAM leg) | 2023 | `min(dataEnd)` of the two tables |
| EUA price | 2024 annual average (€64.8/t) | Scalar; EUR interpretation only |
| Scope-2 grid emission factors | 2022 (EEA/Ember) | Hard-coded in `create_scope2` |
| EQI (QoG_Index) | 2017 survey wave | Latest fully published wave at build time |
| Cohesion Fund | 2014–2020 MFF | Fixed programme totals (not in composite) |

Mixed vintages are inherent to a multi-source composite; the headline cost legs (EUTL 2023 × FIGARO 2023) are deliberately aligned.

Documented data quirks:

- **HR04 (Continentalna Hrvatska)** is rebuilt from HR02+HR05+HR06, except `Unemployment_Rate` and `Capital_Stock_Based_Prod`, which are **copied from HR03** (donor copy in `reshape_to_grid()`): both are intensive indicators with national-level sources in Croatia, so the HR03 value equals the national value that HR04 would receive.
- All-NA recombination groups stay NA (not 0) for both the Croatia and Portugal aggregations, and `harmonize_sector()` preserves NA when a collapse group is entirely missing (false zeros would otherwise trip the Scope-1 true-zero rule into spurious "Zero Risk" cells).

## 16. Reproducibility

- All Eurostat downloads are scripted (no manual files for any Eurostat indicator). The only manual input is `TECH-RIS.xlsx` (EC Regional Innovation Scoreboard — no public API).
- The pipeline picks the latest year with full coverage at every run, so `tar_make()` on a future date produces an updated index automatically. The exact year per indicator is logged in `Final data/Coverage_Report.xlsx`.
- Committed xlsx files in `Initial data/` and `Final data/` and PNGs in `Figures/` are the published snapshot. `git checkout <submission-tag>` reproduces the exact paper figures and numbers without re-running R.
- `tar_destroy(); tar_make()` refreshes the entire pipeline against the current Eurostat vintage.
