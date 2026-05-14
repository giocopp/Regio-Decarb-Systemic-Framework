# Transition Risk Index — Methodology

This document specifies the methodology actually implemented by the pipeline.
It is the source of truth for the paper and supplementary appendix. Each
section maps to a specific stage of the `targets` pipeline.

## 1. Conceptual framework

The Transition Risk Index (TRI) follows the **IPCC AR6 risk framework**:

$$\text{TRI}_{r,s} = \text{Exposure}_{r,s}^{0.5} \times \text{Vulnerability}_{r,s}^{0.5}$$

Indexed over $r$ ∈ EU-27 NUTS-2 regions, $s$ ∈ 12 manufacturing sectors (NACE
Rev.2 Section C plus 11 sub-aggregates). Both Exposure and Vulnerability are
composite scores built from harmonised regional indicators on the unit
interval $[0.01, 0.99]$. Aggregation is geometric so a region cannot have
non-zero risk on a single axis alone.

The unit of analysis is the **(NUTS-2 region × manufacturing sector)** cell.
After excluding ultraperipheral regions, 230 NUTS-2 regions × 12 sectors =
2,760 cells.

## 2. Geographic and sectoral scope

### NUTS-2 regions
- EU-27 NUTS-2 (NUTS 2021 classification).
- **Excluded by design**: ultraperipheral regions whose Eurostat coverage on
  climate, energy, and employment indicators is incomplete and which are not
  part of the continental industrial base the TRI targets. The full excluded
  list is in `R/utils.R::excluded_nuts`:
  `FRY1, FRY2, FRY3, FRY4, FRY5, ES63, ES64, PT20, PT30, FI20`.
- **CY00 (Cyprus) is kept** as the sole NUTS-2 of an EU-27 member state.
- Effective sample: **230 NUTS-2 regions**.

### NUTS-2013 → NUTS-2021 recombinations
Three countries reorganised their NUTS-2 between vintages. The pipeline
applies the following recombinations in `R/03_reshape.R::reshape_to_grid()`
and `R/04_normalize.R::normalize_indicators()` (the latter for
`empl_weights`):

| Country | NUTS-2013 codes | NUTS-2021 codes |
|---|---|---|
| Croatia | HR02 + HR05 + HR06 | HR04 (Continentalna Hrvatska) |
| Netherlands | NL35 → NL31 (Utrecht), NL36 → NL33 (Zuid-Holland) | |
| Portugal | PT19 + PT1D → PT16 (Centro), PT1A + PT1B → PT17 (Lisboa), PT1C → PT18 (Alentejo) | |

For each recombination, indicator values are aggregated using the rule in
`agg_rules` (utils.R): `sum` for extensive quantities (€, kt CO2eq, GWh,
counts), `mean` for intensive quantities (rates, indices, shares).

### Manufacturing sectors
NACE Rev.2 Section C plus 11 sub-aggregates (lookup in
`R/utils.R::sector_aggregation`):

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

All indicators are downloaded automatically by `R/01_create_data.R` from
Eurostat (via `restatapi`) plus three small external sources. Every download
selects its **own latest year with ≥95% EU-27 coverage** (Section 4). The
audit log of which year each indicator picked is in
`Final data/Coverage_Report.xlsx`.

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
| BERD | `rd_e_berdindr2` (national BERD by NACE, MIO_EUR) | National → NUTS-2 by empl share | `create_berd` |
| QoG_Index | QoG EU Regional Dataset, EQI 2017 wave | NUTS-2 if available; NUTS-0 (national) replicated otherwise; NUTS-0 fallback for unmatched NUTS-2021 codes (Ireland IE04/IE05/IE06) | `create_qog` |
| Climate_Mitigation_Laws | QoG Environmental Indicators (`ccl_nmitlp`, latest year per country) | National replicated to NUTS-2 | `create_climate_laws` |
| Sector_Concentration | Derived from `sbs_r_nuts2021` (share of the focal NACE sub-sector in the region's manufacturing employment) | NUTS-2 × Sector | `create_sector_concentration` |
| RE_Potential | JRC ENSPRESO (wind onshore + solar + biomass, medium scenario, TWh) | NUTS-2 | `create_re_potential` |
| Cohesion_Fund | EU Cohesion Open Data API (ERDF+CF+ESF 2014-2020 MFF, regionalised) ÷ NUTS-2 population from `demo_r_d2jan` (2020) | NUTS-2 | `create_cohesion_fund` |
| Regional_Innovation | EC Regional Innovation Scoreboard (manual download — the only non-scripted input) | NUTS-2 | manual xlsx |
| Policy_Pressure | Hard-coded ETS + CBAM coverage scores per NACE sector (sum of the two — the two schemes stack rather than substitute) | Constant per sector | `create_policy_pressure` |

External (non-Eurostat) sources requiring manual download:
- QoG EU Regional Dataset (`qog_eureg.csv`) — University of Gothenburg
- QoG Environmental Indicators (`qog_ei_eureg.csv`) — same
- JRC ENSPRESO regional renewable potentials
- EC Regional Innovation Scoreboard xlsx

## 4. Year selection

Each creator function calls `pick_latest_complete_year()` (`R/utils.R`) on a
five-year window of Eurostat data and selects the **latest year with EU-27
coverage ≥ 95%** on the relevant geo (× sector) dimension. If no year meets
the threshold within the five-year window, the year with the highest
observed coverage is returned and the missing cells are logged.

Per-cell fallback: where a region has data in an earlier year but is missing
in the selected year (e.g. Latvia in `sbs_r_nuts2021` 2023), the most recent
non-NA value is used. Where a region is missing from the indicator dataset
entirely but the country has a value at the next-broader geo level (e.g.
Ireland's EQI 2017 stored under NUTS-2013 codes), the country-level value
replaces the missing NUTS-2 value (Section 7 — QoG fallback chain).

Special cases (year not driven by the helper):
- **Scope 3** uses `min(dataEnd(naio_10_fcp_ii4), dataEnd(env_ac_ghgfp))`
  — both FIGARO tables must align at the same vintage for the MRIO
  computation to be coherent.
- **QoG_Index** uses the **2017 EQI survey wave** (Charron et al.). EQI is
  a triennial survey; 2017 is the latest fully published wave.
- **Cohesion_Fund** uses the fixed 2014–2020 Multiannual Financial Framework
  totals (not annual data).
- **Policy_Pressure** is hard-coded per NACE sector.

The chosen year per indicator is recorded in `Final data/Coverage_Report.xlsx`
after every pipeline run.

## 5. Downscaling: national → NUTS-2

A single canonical rule governs every indicator whose Eurostat source is
national:

- **Extensive quantities** (€, kt CO2eq, GWh, persons, counts) →
  **employment-share downscaling**. For each (Country, Sector) the national
  value is split across the country's NUTS-2 regions in proportion to the
  region's share of national manufacturing employment in that sector.
  Implemented by `downscale_national_to_nuts2()` in `R/utils.R`, using the
  `weight` column of the `empl_weights` target (region's pers_employed in
  that Country × Sector divided by national total, re-normalised to sum to
  1 across regions). For Sector "C" (Total Manufacturing) the weight is the
  region's share of total manufacturing employment.

  Where a sector-specific weight is missing (e.g. Luxembourg has zero C24
  employment in `sbs_r_nuts2021` but reports C24 emissions in
  `env_ac_ainah_r2`), the helper falls back to the country's Sector-C weight
  for that NUTS-2.

- **Intensive quantities** (ratios, indices, percentages) → **uniform
  replication**. Each NUTS-2 region of a country receives the same national
  value. Implemented by `replicate_national_to_nuts2()` in `R/utils.R`.
  Applies to Fossil_Share, Renewables_Share, and Capital_Stock_Based_Prod.

Indicators already published at NUTS-2 (GFCF, Unemployment, Labour Slack,
HRST, RE Potential, HHI, Regional Innovation, EQI when at NUTS-2 level) are
used directly without downscaling.

## 6. Per-employee normalisation

Two extensive indicators are converted to per-employee intensity before
min-max normalisation, in `R/04_normalize.R` (`to_per_empl`):
**Gross_Fixed_Capital_Formation** and **BERD**. The division uses the
NUTS-2 × Sector manufacturing employment count from `empl_weights`.

Other extensive indicators (Scope1_Emissions, Scope2_Emissions, Scope3_Emissions,
Energy_Consumption, Import_ExtraEU, Export_ExtraEU) are NOT divided by
employment in this step because they were already downscaled using employment
shares (Section 5); dividing again would double-count and inflate small
regions.

## 7. Winsorisation

GFCF per employee is winsorised at the **99th percentile within each
(Indicator × Sector_ID)** group (`winsorize_upper(Value, p=0.99)` in
`R/04_normalize.R`). This caps the Ireland multinational profit-shifting
distortion that the JRC Handbook on Constructing Composite Indicators
(2008) explicitly flags. GFCF is not part of the TRI composite (see §11),
but it is published in the normalised reference tables and the cap
prevents the headline numbers from being dominated by Ireland's MNC
accounting. BERD is **not** winsorised: the top BERD regions are bona
fide R&D agglomerations (Stuttgart, Oberbayern, Rhône-Alpes, Düsseldorf,
Île-de-France) and Ireland's per-employee BERD shows no profit-shifting
inflation comparable to its GFCF. The cap is computed from values of the
same indicator only; no other indicators are winsorised.

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
- **Policy_Pressure** is a sector-level constant (every NUTS-2 region in
  sector $s$ has the same raw value). Normalising within (Indicator × Sector)
  would collapse min = max → 0.5. It is therefore normalised across all
  (Sector × NUTS-2) values together (group by `Indicator` only).
- **All other indicators** are normalised within (Indicator × Sector_ID) so
  each sector's regional ranking is comparable.

### Orientation reversal
Indicators where higher raw value = lower vulnerability are reversed after
the min-max step: $V_N \leftarrow 1 - V_N$. The "positive" list (NOT reversed,
higher raw = higher vulnerability contribution) is:

`Scope1_Emissions, Scope2_Emissions, Scope3_Emissions, Policy_Pressure, Energy_Consumption, Fossil_Share, Unemployment_Rate, Labour_Market_Slack, Export_ExtraEU, Import_ExtraEU, Sector_Concentration`

All other indicators (Renewables_Share, RE_Potential, GFCF, BERD,
Regional_Innovation, QoG_Index, Climate_Mitigation_Laws, Cohesion_Fund,
Capital_Stock_Based_Prod, Highly_Skilled_Workers) are reversed.

## 9. Vulnerability dimensions

Vulnerability is built from six dimensions, each a row-mean of its
constituent normalised indicators. Defined in `R/05_aggregate.R::dimensions`:

| Dimension | Indicators | Notes |
|---|---|---|
| **Energy** | Energy_Consumption, Fossil_Share, Renewables_Share, RE_Potential | |
| **Labour** | Unemployment_Rate, Labour_Market_Slack, Highly_Skilled_Workers | |
| **Supply_Chain** | Import_ExtraEU | Export_ExtraEU intentionally excluded — see §11 |
| **Technology** | BERD (per employee), Regional_Innovation | BERD not winsorised — top R&D hubs reflect genuine industrial R&D, no Ireland inflation. BERD also operationalises the "financing-for-innovation" channel that a standalone Finance dimension would otherwise carry — see §11 |
| **Institutions** | QoG_Index, Climate_Mitigation_Laws | EQI partially captures financial-governance quality |
| **Diversification** | Sector_Concentration (focal sector's share of regional manufacturing employment; sector-specific by construction) | Replaces an earlier region-only Herfindahl-Hirschman index that did not vary across sectors |

A standalone **Finance** dimension is **not** included. The rationale is in §11.

Each dimension score is re-normalised to [0.01, 0.99] within Sector_ID.
Vulnerability is the row-mean of the six dimension scores, re-normalised
to [0.01, 0.99] within Sector_ID.

NAs are imputed cell-by-cell at the indicator stage from the country × sector
median (`R/utils.R::impute_with_median`) before the dimension means are
computed. Remaining NAs (where the country has no other regions to draw a
median from) propagate via `rowMeans(na.rm = TRUE)`.

## 10. Exposure composite

Exposure is the row-mean of four indicators, re-normalised to [0.01, 0.99]
within Sector_ID:

`Exposure = mean(Scope1_Emissions, Scope2_Emissions, Scope3_Emissions, Policy_Pressure)`

The same NA imputation as Vulnerability applies.

## 11. Indicators excluded from the composite (intentional design choices)

Four indicators are computed and stored in the `Initial data/` xlsx files
for reference and reproducibility but are NOT used in the TRI composite,
and the Finance dimension that the first three would feed is itself
dropped from Vulnerability:

- **Gross_Fixed_Capital_Formation (GFCF)**: measures realised investment
  *volume* (NUTS-2, NACE C-aggregate only, per `nama_10r_2gfcf`), not access
  to transition finance. The raw series is dominated by Ireland's
  multinational profit-shifting accounting (which is the reason a p99
  winsorisation is applied in §7 — itself a flag, per the JRC Handbook
  (Nardo et al. 2008), that the indicator is structurally distorted) and
  the source has no NACE 2-digit disaggregation, so the same regional GFCF
  number is replicated across all eleven manufacturing sub-sectors. The
  indicator is therefore *not* a meaningful sectoral discriminator of
  transition-finance access. We retain the data file for reference and
  reproducibility but exclude it from the composite.
- **Cohesion_Fund**: orientation is ambiguous because cohesion transfers
  simultaneously signal regional need AND provide support. Treating high
  transfers as "more resilient" makes Cohesion-recipient regions look less
  vulnerable than non-recipients, contradicting the literature on regional
  economic vulnerability. Excluded.
- **Capital_Stock_Based_Prod**: the `NCS_HW / N11N / I20` index measures
  capital per hour worked indexed to 2020 = 100. Direction is ambiguous —
  growth could mean modernisation (good) or labour exodus / hours collapsing
  (bad); decline could mean stranded-asset write-offs (bad) or labour
  absorption (good). Reviewer 2 of 25CP7059-RA flagged this ambiguity.
  Excluded.
- **Export_ExtraEU**: Reviewer 2 noted that "being embedded in global supply
  chains may also be positive as it may drive innovation and allow a company
  to tap into new markets more easily". Export access is treated as a
  resilience factor with ambiguous direction for vulnerability. Excluded.
  Only Import_ExtraEU is used in the Supply_Chain dimension.

### 11.1 Why there is no standalone Finance dimension

A previous version of the index carried a Finance dimension built solely
from GFCF (per employee, p99-capped). On methodological review we cannot
defend it for four reasons:

1. **No clean regional × sectoral indicator of *access to* transition
   finance exists in Eurostat or in the listed external sources.** GFCF
   captures realised investment volume, not access; the three other
   candidates we evaluated (cohesion transfers, capital-stock index,
   tangible/intangible investment) each fail on direction unambiguity or
   on granularity. ECB MFI loan series are euro-area-only; the EIB
   Investment Survey (EIBIS) is representative only at country × broad
   sector group. The Just Transition Fund's national envelopes are
   themselves a function of regional carbon-intensity and employment in
   carbon-intensive sectors — i.e. the same variables that feed the
   Exposure side of the TRI — so using JTF allocations would induce
   mechanical correlation with Exposure.
2. **Comparable regional EU transition-risk composites do not carry a
   standalone Finance dimension.** Vrontisi et al. (2024), Sokolowski et
   al. (2022), and the JRC Coal Regions in Transition framework all
   structure vulnerability around socioeconomic, energy, and institutional
   pillars without a separate finance axis.
3. **The JRC Handbook on Constructing Composite Indicators (Nardo et al.,
   2008; JRC47008) counsels against single-indicator pillars** when the
   sole indicator's direction is documented as distorted (here: the
   Ireland MNC issue, which the previous version masked via winsorisation
   — itself an acknowledgement of the distortion).
4. **The conceptual content "finance for transition" is partially
   captured by the remaining dimensions.** BERD in Technology is private
   R&D investment — i.e. the financing flow that underwrites the
   technology channel of transition. QoG_Index in Institutions
   incorporates regional financial-governance quality. The composite is
   therefore not silent on the financing channel; it simply does not give
   it its own pillar.

The dimension would only be reintroduced if a clean Eurostat-level
NUTS-2 × NACE-division indicator of transition-finance access became
available (e.g. an EBS-era sector-specific investment series at
`sbs_r_nuts2021` with direction validated against the literature).

## 12. TRI and risk bands

Final risk:
$$\text{Risk}_{r,s} = \text{Exposure}_{r,s}^{\,0.5} \cdot \text{Vulnerability}_{r,s}^{\,0.5}$$
$$\text{Risk\_norm}_{r,s} = \text{range01}(\text{Risk}_{r,s}) \quad \text{within Sector}_s$$

(Geometric aggregation with equal weights, $\alpha = 0.5$.)

When Exposure = 0 (i.e. raw Scope1_Emissions = 0 in a region), Exposure is set
to NA so the risk is undefined (not artificially zero from the geometric
formula). Risk_norm is then NA → Risk_Band = "Zero Risk".

Risk bands are equal-width quintiles on $[0, 1]$:

| Risk_norm range | Band |
|---|---|
| [0.0, 0.2] | Very Low |
| (0.2, 0.4] | Low |
| (0.4, 0.6] | Medium |
| (0.6, 0.8] | High |
| (0.8, 1.0] | Very High |

## 13. Pipeline

The full pipeline is orchestrated by `_targets.R` using the `{targets}` R
package. Each Phase below corresponds to a contiguous block of targets:

- **Phase 1 — Create initial data**: every creator function (`create_*`) pulls
  Eurostat data, picks its year, downscales if needed, and writes one xlsx
  per indicator under `Initial data/`.
- **Phase 2 — Harmonise** (`R/02_harmonize.R`): reads the per-indicator xlsx
  files and joins them into one long tibble (region × sector × indicator).
- **Phase 3 — Reshape** (`R/03_reshape.R`): expands to a complete grid of
  (region × sector × indicator), handles NUTS recombinations, applies
  `agg_rules` (sum vs mean) for aggregated regions.
- **Phase 4 — Normalise** (`R/04_normalize.R`): per-employee division,
  winsorisation, min-max normalisation, orientation reversal.
- **Phase 5 — Aggregate risk** (`R/05_aggregate.R`): NA imputation,
  Exposure / Vulnerability dimensions / TRI.
- **Phase 6 — Save** outputs to `Final data/`: `Risk_data.xlsx`,
  `Raw_data_not_normalized.xlsx`, `Coverage_Report.xlsx`,
  `Sensitivity_Analysis.xlsx`, `Top_Bottom_Regions_per_Sector.xlsx`.
- **Phase 7 — Figures**: maps, radars, quadrants, within-country
  variance, cluster maps (PNG, 600 DPI).

## 14. Sensitivity analysis

`R/07_sensitivity.R::run_sensitivity()` produces a Spearman correlation of
the baseline TRI ranking with the ranking from six families of alternative
construction choices. Output: `Final data/Sensitivity_Analysis.xlsx`.

1. **Alpha sensitivity**: re-compute Risk with $\alpha \in \{0.30, 0.40,
   0.50, 0.60, 0.70\}$ in $\text{Risk} = E^{\alpha} \cdot V^{1-\alpha}$.
   Default $\alpha = 0.50$.
2. **PCA weights for Vulnerability**: use the absolute PC1 loadings of the
   six Vuln_* dimensions (re-normalised to sum to 1) as dimension weights,
   instead of the equal-weight row-mean.
3. **Leave-one-dimension-out**: drop each Vuln_* dimension one at a time and
   re-compute Vulnerability as the mean of the remaining five.
4. **Geometric vs arithmetic aggregation**: replace
   $E^{0.5} \cdot V^{0.5}$ with $0.5 \cdot E + 0.5 \cdot V$.
5. **Min-max vs z-score normalisation**: re-normalise Exposure and each
   Vuln_* dimension as z-scores within Sector_ID before composing the TRI.
6. **EDGAR Scope 1 alternative** (sectors C19-C20, C23, C24 only): replace
   the Eurostat-downscaled `Scope1_Emissions` with values derived from EDGAR
   2024 0.1° gridded CO2 maps aggregated to NUTS-2 polygons. IPCC sector
   crosswalk: `REF_TRF + CHE → C19-C20`, `NMM → C23`, `IRO → C24`. Only the
   three heavy-emitter sectors are tested because EDGAR's facility-aware
   spatial proxies are most likely to differ from employment-share
   downscaling for point-source-dominated industries.
   Implemented in `R/08_sensitivity_alt.R::compute_edgar_scope1()`.

All comparisons are reported as Spearman $\rho$ between the perturbed
ranking and the baseline `Risk_norm`, per test.

## 15. Reproducibility

- All Eurostat downloads are scripted (no manual files for any Eurostat
  indicator). The only manual input is `TECH-RIS.xlsx` (EC Regional Innovation
  Scoreboard — no public API).
- The pipeline picks the latest year with full coverage at every run, so
  `tar_make()` on a future date produces an updated index automatically. The
  exact year per indicator is logged in `Final data/Coverage_Report.xlsx`.
- Committed xlsx files in `Initial data/` and `Final data/` and PNGs in
  `Figures/` are the published snapshot. `git checkout <submission-tag>`
  reproduces the exact paper figures and numbers without re-running R.
- `tar_destroy(); tar_make()` refreshes the entire pipeline against the
  current Eurostat vintage.
