# Methodology Notes: Transition Risk Index Revision

## Document purpose

This document records the methodological decisions made during the revision
of the Transition Risk Index (TRI) paper for resubmission to Climate Policy.
It explains what we changed, why, what problems we encountered, and how the
literature informed our choices. It serves as an internal reference for
writing the revised paper and the point-by-point response letter.

---

## 1. Overview of changes from the original submission

The original TRI combined a single-indicator Exposure (Scope 1 GHG
emissions) with a 5-dimension Vulnerability index (Energy, Labour, Finance,
Supply Chain, Technology), aggregated via a geometric mean. Three reviewers
identified fundamental problems with this framework. The revision addresses
every critique.

### 1.1 Exposure: from single indicator to multi-component

**Original:** GHG emissions (Scope 1 only), normalised per enterprise.

**Revised:** Three sub-indicators, equally weighted:

| Sub-indicator | Measures | Source |
|---|---|---|
| GHG Emissions (Scope 1) | Direct carbon intensity | Eurostat env_ac_ainah_r2 |
| Scope 2 Emissions | Indirect energy emissions | EEA grid emission factors x Eurostat nrg_bal_c electricity consumption |
| Policy Pressure | Regulatory exposure to ETS and CBAM | Hardcoded from EU ETS Directive 2003/87/EC and CBAM Regulation 2023/956 |

**Why:** Reviewer 3 wrote: "It merely measures the current GHG emissions
levels and then asserts that higher emission levels signify greater exposure
to decarbonisation risk. This seems a gross over simplification." Adding
Scope 2 and Policy Pressure captures the two channels through which
decarbonisation policy transmits risk: indirect energy costs (grid carbon
intensity) and direct regulatory pressure (which sectors are covered by ETS
and CBAM).

**Scope 2 approximation:** Country-level grid emission factors (gCO2/kWh)
from EEA multiplied by sectoral electricity consumption from Eurostat. This
is a coarse approximation (no sub-national grid variation) but is the best
available from open data. The paper acknowledges this limitation and notes
that Scope 3 remains excluded pending future work.

**Policy Pressure coding:** Each of the 11 aggregated sectors receives a
score (0-1) for ETS coverage and CBAM coverage based on the regulatory
texts. For example, C24 (basic metals) receives 1.0 for both ETS and CBAM;
C13-C15 (textiles) receives 0 for both. The composite is the
equal-weighted average. This is a sector-level indicator (no regional
variation) but captures meaningful cross-sector differentiation in
regulatory exposure.

### 1.2 Vulnerability: from 5 to 7 dimensions

**Original:** Energy, Labour, Finance, Supply Chain, Technology.

**Revised:** Energy, Labour, Finance, Supply Chain, Technology,
Institutions, Diversification.

Changes within existing dimensions and new dimensions are detailed in
Sections 2-6 below.

### 1.3 Normalisation: from per-enterprise to per-employee

**Original:** GHG Emissions, Energy Consumption, GFCF, and BERD divided by
number of enterprises per region-sector.

**Revised:** Only GFCF and BERD divided by number of employees. GHG
Emissions, Scope 2 Emissions, and Energy Consumption are NOT divided by
employees because they are already downscaled to regions via employment
shares in the data creation step (dividing again would double-count and
inflate small regions).

**Why:** Reviewer 2 wrote: "Why would a region with one very large polluting
firm be worse off than a region with many smaller polluting firms?" The
per-enterprise denominator conflates firm size with risk. Per-employee
normalization is standard in the composite indicator literature (OECD/JRC
Handbook on Constructing Composite Indicators, 2008).

**Zero-employment handling:** Region-sector combinations with zero employees
(13 cases, including LU00-C24, CY00-C26-C27, ES63/ES64 overseas territories)
have all indicator values set to NA before normalisation. These are sectors
that do not meaningfully exist in those regions.

### 1.4 NUTS code recombinations

Several NUTS-2 codes changed between vintages. The pipeline recombines:

- Croatia: HR02 + HR05 + HR06 -> HR04 (Continentalna Hrvatska)
- Netherlands: NL35 -> NL31, NL36 -> NL33
- Portugal: PT19 + PT1D -> PT16, PT1A + PT1B -> PT17, PT1C -> PT18

A critical bug in the original code was that employment data was not
recombined alongside the other indicators. HR04 had pers_employed = NA,
meaning per-employee normalisation was skipped and absolute values were
kept, making HR04 an extreme outlier (Exposure = 0.99 for almost all
sectors). This was fixed by summing employment across component regions
before the normalisation step.

### 1.5 Scenario dimension removed

**Original:** Figure 1b showed a three-element framework (Scenario,
Exposure, Vulnerability) but the equation and analysis only used Exposure
and Vulnerability.

**Revised:** The scenario/hazard element is removed from the framework
figure. The current EU policy environment (ETS + CBAM) is treated as a
fixed baseline scenario. This addresses Reviewer 3's critique that the
scenario element was "missing but would seem to be important."

### 1.6 Sensitivity analysis added

A new step (6-sensitivity-analysis.R) tests robustness across five
dimensions:

- Alpha sensitivity: alpha in {0.3, 0.4, 0.5, 0.6, 0.7}
- Weighting: equal weights vs PCA-derived weights
- Leave-one-dimension-out: influence of each vulnerability dimension
- Normalization: min-max vs z-score
- Aggregation: geometric vs arithmetic mean

This directly addresses Reviewer 3's request for robustness checks.

---

## 2. Energy dimension

**Original indicators:** Energy Consumption, Fossil Share, Renewables Share
(all from Eurostat nrg_bal_c).

**Revised indicators:** Energy Consumption, Fossil Share, Renewables Share,
RE Potential (from JRC ENSPRESO).

**Problem identified:** Fossil Share and Renewables Share are national-level
data (Eurostat energy balance nrg_bal_c provides only country-level
breakdowns for industrial subsectors). All regions within a country receive
identical values. This means the Energy dimension had zero within-country
discriminating power. PCA analysis confirmed this: Energy received near-zero
weight (0.004) because its variation was orthogonal to the other dimensions
that have genuine regional variation.

**Solution:** Added RE Potential from JRC ENSPRESO (Ruiz et al., 2019,
"ENSPRESO -- an open, EU-28 wide, transparent and coherent database of
wind, solar and biomass energy potentials", Energy Strategy Reviews, 26,
100379). This dataset provides technical renewable energy potential
(wind onshore + solar + biomass) at NUTS-2 level based on physical resource
availability (wind speeds, solar irradiance, land use constraints). It has
genuine regional variation: Castilla y Leon (ES41) has 933 TWh potential vs
Bremen (DE50) at 1.9 TWh.

After adding ENSPRESO, PCA weight for Energy rose from 0.004 to 0.117,
confirming the dimension now carries meaningful regional information.

**Limitation:** Fossil Share and Renewables Share remain national-level.
This is acknowledged in the paper. No open-source dataset provides regional
industrial energy mix at NUTS-2.

**Reviewer addressed:** Reviewer 2 comment 8d: "On Renewable Energy, I
think it would be necessary to not only look into current RE supply but also
into RE potentials in each region."

---

## 3. Labour dimension

**Original indicators:** Unemployment Rate, Labour Market Slack, Wage Per
Hour, Highly Skilled Workers.

**Revised indicators:** Unemployment Rate, Labour Market Slack, Highly
Skilled Workers.

**Change:** Removed Wage Per Hour.

**Why:** Reviewer 2 wrote: "High wages can impede labour mobility if
alternative industries pay significantly less." This is a valid point: the
theoretical direction of the wage indicator is ambiguous. High wages could
signal economic strength (lower vulnerability) OR could create adjustment
costs if workers must move to lower-paying green sectors (higher
vulnerability). Rather than making an arbitrary directional assumption, we
removed the indicator.

---

## 4. Institutions dimension (NEW)

**Indicators:** QoG Index (EQI), Climate Mitigation Laws (ccl_nmitlp).

**Data sources:**

- European Quality of Government Index (EQI): University of Gothenburg QoG
  EU Regional Dataset, 2017 survey wave. Available at NUTS-2 level. 154
  EU-27 regions with data. Reference: Charron, Dijkstra & Lapuente (2014,
  2021).

- Climate Mitigation Laws: QoG Environmental Indicators Dataset, variable
  ccl_nmitlp (number of climate change mitigation laws and policies in
  place). National-level, 2020 data. All 27 EU countries covered. Replicated
  to all NUTS-2 regions within each country.

**Why:** Reviewer 2 wrote: "The authors are not including an indicator for
institutional capacity due to lack of available data. I suggest the authors
look into the Quality of Government Index of Uni Gothenburg." We followed
this recommendation directly. The EQI provides the only regional-level
(NUTS-2) governance indicator covering all EU member states. It captures
corruption, impartiality, and quality of public services -- all relevant to
a region's capacity to implement and govern decarbonisation policies.

The Climate Mitigation Laws indicator adds a climate-policy-specific
dimension: countries with more mitigation laws have more developed
regulatory frameworks for managing the transition. This is national-level
(uniform within countries) but captures meaningful between-country variation
that the regional EQI does not (e.g., Spain has 40 mitigation laws vs
Cyprus with 2).

**Limitation:** EQI covers 154 of 237 target regions (65%). Missing values
are imputed using the country-sector median. The 2017 survey wave is the
most recent available in the downloaded dataset.

---

## 5. Diversification dimension (NEW)

**Indicator:** HHI Employment (Herfindahl-Hirschman Index of manufacturing
employment concentration).

**Data source:** Computed from existing EMPL_Region.xlsx (employment shares
by region x sector). HHI = sum(share_s^2) across the 10 manufacturing
subsectors within each region. Higher HHI = more concentrated = more
vulnerable.

**Why:** Reviewer 2 wrote: "This literature on regional industrial
development suggests that the existence of alternative industrial clusters
is a key requisite for developing new industrial development pathways and
co-determine regional adaptability. While the proposed indicator covers R&D
and innovation it does not consider regional diversification."

This is grounded in the regional economic geography literature:

- Boschma (2015): related variety -- regions with diverse but related
  industrial bases are better positioned to develop new growth paths.
- Neffke et al. (2011): skill-relatedness enables industrial branching into
  new sectors.
- Hassink (2010): regional lock-in -- specialised regions face higher
  transition costs.

The HHI is a standard measure of concentration. A region with employment
spread equally across 10 sectors (HHI = 0.10) is more diversified and
adaptive than one dominated by a single sector (HHI approaching 1.0).

**RE Potential was initially placed in this dimension** but was moved to the
Energy dimension to address the Energy dimension's lack of regional
variation (see Section 2). The Diversification dimension now contains only
HHI Employment.

---

## 6. Supply Chain dimension: problems and literature review

### 6.1 Current state

**Indicator:** Import ExtraEU (extra-EU imports by sector, from Eurostat
ext_tec09).

**Problem 1 -- No regional variation:** The data is national-level,
replicated identically across all regions within each country. Diagnostic:
27 distinct values across 229 observations for sector C; standard deviation
= 0 within every country. The dimension adds no within-country
discriminating power.

**Problem 2 -- Ambiguous theoretical direction:** Reviewer 2 wrote: "The
authors suggest that being part of global supply chains and being involved
in global trade is bad for regional economic resilience/adaptability. The
above cited literature suggests that this is not necessarily the case."
This is supported by the GVC literature: backward participation (importing
intermediates) can increase exposure to disruption but also reflects
productive integration and access to inputs that enhance adaptive capacity.

### 6.2 What the literature does

A systematic review of comparable regional composite indices reveals that
**no existing transition risk/vulnerability index at NUTS-2 includes a
supply chain or trade dependence dimension:**

- Rodriguez-Pose & Bartalucci (2023), "Regional vulnerability to the green
  transition" (DG GROW WP2023/16; Cambridge Journal of Regions, Economy and
  Society, 2024, 17(2), pp. 339-): Six dimensions (fossil fuel dependency,
  industry, agriculture, tourism, energy, transport). No supply chain
  dimension.

- McDowall, Reinauer, Fragkos, Miedzinski & Cronin (2023), "Mapping regional
  vulnerability in Europe's energy transition" (Climatic Change, 176(2)):
  IPCC-style framework at NUTS-2. No supply chain dimension.

- Vrontisi, Charalampidis, Fragkiadakis & Florou (2024), "Towards a just
  transition: Identifying EU regions at a socioeconomic risk of the
  low-carbon transition" (Energy and Climate Change): NUTS-2 level. No
  supply chain dimension.

- EU Just Transition Fund (Regulation EU 2021/1056): Allocation formula uses
  industrial GHG emissions, employment in industry, coal/peat employment,
  GNI per capita. No trade indicators.

The reason is a data gap: **official trade statistics (Eurostat COMEXT) are
available only at the national level.** There is no Eurostat dataset
providing import/export volumes at NUTS-2.

### 6.3 Potential alternatives

Two research-grade multi-regional input-output (MRIO) databases provide
regional trade data:

- **EUREGIO** (Guilhoto et al., 2023, "European multi regional input output
  data for 2008-2018", Scientific Data): 272 NUTS-2 regions, 10 sectors,
  annual 2008-2018. Open access.

- **FIGARO-REG** (JRC, 2025): 240 EU NUTS-2 regions, 56 industries (or 10
  aggregated), reference year 2017. Freely available via JRC Data Catalogue.

These could provide genuine regional trade dependence or GVC participation
measures. However:

- They cover limited reference years (EUREGIO: 2008-2018; FIGARO-REG: 2017
  only).
- Computing GVC participation requires substantial MRIO processing.
- No existing composite index uses them for this purpose.

### 6.4 Decision

**We propose dropping the Supply Chain dimension** from the vulnerability
index. Justification:

1. The current indicator has zero regional variation.
2. The theoretical direction is ambiguous (reviewer-confirmed).
3. No comparable index at NUTS-2 includes such a dimension.
4. Leave-one-out sensitivity analysis shows dropping Supply Chain barely
   affects rankings (Spearman rho = 0.98 with baseline).
5. The supply chain exposure concept is partially captured by our
   Policy Pressure sub-indicator in Exposure (CBAM coverage identifies
   sectors exposed to carbon border adjustments on their imports).

The limitation is discussed in the paper. FIGARO-REG is cited as a
potential future data source for regional trade vulnerability.

---

## 7. Finance dimension: problems and literature review

### 7.1 Current state

**Indicator:** Gross Fixed Capital Formation per employee (Eurostat
nama_10r_2gfcf, 2021, manufacturing sector C).

**Problem 1 -- Ireland outlier:** Ireland's GFCF is 8.7x the EU average
per employee due to multinational profit-shifting. Pharmaceutical and
technology companies book intellectual property and capital investment in
Ireland for tax purposes, inflating GFCF without corresponding real
productive capacity. This is the well-documented "leprechaun economics"
effect. The Irish CSO introduced Modified GNI (GNI*) to address this at
national level, but no modified GFCF exists at NUTS-2.

Consequence: After min-max normalisation, Ireland anchors the low end
(low vulnerability = high GFCF) and compresses all other regions toward
high vulnerability.

**Problem 2 -- Portugal zeros:** PT16, PT17, PT18 show GFCF = 0 as an
artifact of the NUTS code recombination (the source data uses older NUTS
codes that don't map cleanly).

### 7.2 What the literature recommends

**OECD/JRC Handbook on Constructing Composite Indicators (2008):**
Recommended outlier treatments include winsorisation, logarithmic
transformation, Box-Cox transformation, and ranking. The choice depends on
the distribution shape and the number of outliers.

**EU Regional Competitiveness Index 2.0 (Dijkstra, Poelman &
Rodriguez-Pose, 2022):** Of 68 indicators, 4 required logarithmic
transformation and 2 required winsorisation to handle outliers. The RCI
does not include a dedicated "Financial Development" pillar.

**Boudt, Todorov & Wang (2020), "Robust distribution-based winsorization in
composite indicators construction" (Social Indicators Research, 149(2)):**
Proposes replacing extreme values with quantiles of a fitted Weibull
distribution. Shows this produces rankings closer to "clean data" than
either no treatment or traditional quantile-based winsorisation.

**JRC COINr R package (Becker):** Implements standard winsorisation with
configurable parameters. Default: winsorise up to 5 data points per
indicator to bring skew and kurtosis below thresholds. Guidance: caution
when winsorising more than 10% of units.

### 7.3 Alternative indicators available at NUTS-2

| Indicator | Source | Notes |
|---|---|---|
| GFCF by NACE sector | Eurostat nama_10r_2gfcf | Current. Ireland problem. |
| GFCF for general government | Eurostat nama_10r_2gfcf | Excludes private IP distortion |
| GDP per capita | Eurostat tgs00003 | Standard proxy, widely used |
| EU structural fund payments | Cohesion Open Data Portal | Direct measure of public investment support |
| R&D expenditure (GERD) | Eurostat rd_e_gerdreg | Already partially captured by BERD in Technology |
| Venture capital | RIS/EIF | Limited regional coverage |
| Firm investment rates | EIB Investment Survey (EIBIS) | Survey-based, 114 NUTS-2 regions |

### 7.4 Decision

**Two changes to the Finance dimension:**

**a) Winsorise GFCF at the 95th percentile** following OECD/JRC Handbook
guidance and RCI 2.0 practice. This caps the Ireland outlier while
preserving the genuine regional variation across the rest of the EU.
Portugal zeros are handled by the zero-employment NA rule (Section 1.3).

**b) Add EU Cohesion Fund payments per capita** as a second indicator in
the Finance dimension, sourced from the Cohesion Open Data Portal
(cohesiondata.ec.europa.eu). This dataset provides regionalised (NUTS-2)
annual EU expenditure for ERDF, Cohesion Fund, and ESF, covering
1988-2022 in current EUR.

Rationale for including Cohesion Fund payments:

1. It is a genuinely regional indicator with substantial within-country
   variation (e.g., German regions range from near-zero in western Lander
   to significant allocations in eastern Lander).
2. It captures **public investment capacity for transition** -- regions
   receiving more structural funds have greater access to co-financed
   investment in infrastructure, skills, and innovation that facilitate
   industrial transformation.
3. It complements GFCF, which measures private investment capacity. The
   combination of private (GFCF) and public (Cohesion Funds) investment
   provides a more complete picture of a region's financial readiness for
   decarbonisation.
4. Higher Cohesion Fund allocation signals that the EU itself has identified
   the region as needing investment support, which is a direct policy
   signal of transition-relevant financial capacity.

**Direction:** Negative (higher payments = lower vulnerability). Regions
receiving more investment support have greater financial capacity to manage
the transition. This interpretation treats structural funds as an enabling
resource, not as a symptom of underdevelopment.

**Note on circularity:** One could argue that structural funds are allocated
precisely because regions are vulnerable, creating circularity. However,
the fund allocation is based on GDP per capita thresholds (Cohesion Fund)
and multi-year programming (ERDF/ESF), not on our vulnerability index.
The funds represent real financial resources available for transition
investment regardless of why they were allocated. Moreover, Fund payments
represent realised expenditure (not just eligibility), meaning they reflect
actual absorption capacity -- itself a dimension of financial readiness.
This interpretation aligns with the EIB Investment Survey finding that
firms in cohesion regions face different investment barriers than those in
more developed regions (EIB, Regional Cohesion in Europe 2021-2022).

---

## 8. Final revised framework summary

### Exposure (3 sub-indicators, equal weights)

| Indicator | Source | Level | Direction |
|---|---|---|---|
| GHG Emissions (Scope 1) | Eurostat env_ac_ainah_r2 | Country x Sector -> Region (employment-weighted) | + |
| Scope 2 Emissions | EEA grid EF x Eurostat nrg_bal_c | Country x Sector -> Region (employment-weighted) | + |
| Policy Pressure | EU ETS + CBAM regulations | Sector-level | + |

### Vulnerability (6 dimensions, equal weights)

| Dimension | Indicators | Source | Level | Direction |
|---|---|---|---|---|
| **Energy** | Energy Consumption | Eurostat nrg_bal_c | Country x Sector -> Region | + |
| | Fossil Share | Eurostat nrg_bal_c | National | + |
| | Renewables Share | Eurostat nrg_bal_c | National | - |
| | RE Potential | JRC ENSPRESO | NUTS-2 | - |
| **Labour** | Unemployment Rate | Eurostat lfst_r_sla_ga | NUTS-2 | + |
| | Labour Market Slack | Eurostat lfst_r_sla_ga | NUTS-2 | + |
| | Highly Skilled Workers | Eurostat lfst_reg_lfe2educ | NUTS-2 | - |
| **Finance** | GFCF per employee (winsorised) | Eurostat nama_10r_2gfcf | NUTS-2 | - |
| | EU Cohesion Fund payments per capita | Cohesion Open Data Portal | NUTS-2 | - |
| **Technology** | BERD per employee | Eurostat rd_e_berdindr2 | Country x Sector -> Region | - |
| | Regional Innovation (RIS) | EC Regional Innovation Scoreboard | NUTS-2 | - |
| **Institutions** | QoG Index (EQI) | Univ. Gothenburg QoG | NUTS-2 | - |
| | Climate Mitigation Laws | QoG Environmental Indicators | National | - |
| **Diversification** | HHI Employment | Computed from Eurostat SBS | NUTS-2 | + |

### Removed from original

| What | Why |
|---|---|
| Supply Chain dimension | Zero regional variation; ambiguous direction; no comparable index includes it at NUTS-2 |
| Wage Per Hour | Ambiguous direction (Reviewer 2) |
| Per-enterprise denominator | Economically indefensible (Reviewer 2) |
| Capital Stock Based Productivity | Replaced by GFCF per employee |
| Scenario/Hazard element | Disconnected from equation (Reviewer 3); treated as fixed baseline |
| Stranded Asset Proxy | Correlated 0.95 with Fossil Share (double-counting) |

### Aggregation

TRI = Exposure^0.5 x Vulnerability^0.5 (geometric mean, non-compensatory)

Vulnerability = equal-weight mean of 6 dimension scores, each normalised
to [0.01, 0.99] by sector.

---

## 9. Known limitations

1. **Scope 3 emissions** are not included. Scope 2 is approximated with
   country-level grid emission factors.
2. **Fossil Share and Renewables Share** are national-level (no sub-national
   industrial energy mix data exists in Eurostat).
3. **EQI** covers 154 of 237 target regions; missing values are imputed.
4. **GFCF** requires winsorisation for Ireland; the underlying distortion
   from multinational profit-shifting cannot be fully corrected.
5. **Policy Pressure** has no regional variation (sector-level only).
6. **Climate Mitigation Laws** has no regional variation (national-level).
7. The index is **static** (single cross-section, primarily 2022 data).
8. **Supply chain vulnerability** is not directly measured. The concept is
   partially captured by Policy Pressure (CBAM) in the Exposure component.
9. **Regional energy consumption** is downscaled from national data using
   employment weights, which assumes energy use is proportional to
   employment.

---

## 10. Reproducibility

The full pipeline is implemented in R using the targets package:

```
Code and data/
  _targets.R           # Pipeline definition (18 targets)
  R/
    utils.R            # Shared helpers
    01_create_data.R   # Data creation functions (Eurostat API, ENSPRESO, QoG)
    02_harmonize.R     # Combine sector + non-sector data
    03_reshape.R       # NUTS recombination + complete grid
    04_normalize.R     # Per-employee + min-max normalisation
    05_aggregate.R     # Dimension scores, vulnerability, TRI
    06_visualize.R     # Maps + radar charts
    07_sensitivity.R   # Robustness checks
```

To reproduce: `setwd("Code and data"); library(targets); tar_make()`

All data sources are open-access (Eurostat API, JRC ENSPRESO, QoG
Institute).
