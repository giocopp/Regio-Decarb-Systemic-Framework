# Transition Risk Index for EU Regional Manufacturing

A systemic framework for assessing the risk of decarbonisation to regional
manufacturing activities in the European Union.

## Overview

This repository contains the data, code, and manuscript for a composite
indicator (the **Transition Risk Index**, TRI) that measures how exposed and
vulnerable EU NUTS-2 regions are to the costs and disruptions of industrial
decarbonisation policy.

The TRI combines:

- **Exposure** (3 sub-indicators): Scope 1 GHG emissions, Scope 2 emissions
  (approximated), and regulatory policy pressure (EU ETS + CBAM coverage).
- **Vulnerability** (6 dimensions, 13 indicators): Energy, Labour, Finance,
  Technology, Institutions, and Diversification.

Coverage: **237 NUTS-2 regions** across the EU-27, disaggregated by **10
manufacturing subsectors** (NACE Rev. 2 groups C10-C12 through C31-C33)
plus total manufacturing.

Risk is aggregated via a geometric mean:
**TRI = Exposure^0.5 x Vulnerability^0.5**, ensuring non-compensatory
behaviour (a region must score high on both exposure *and* vulnerability to
be classified as high-risk).

## Repository structure

```
Regio-Decarb-Systemic-Framework/
|
|-- README.md                    # This file
|-- .gitignore
|-- Regio-Decarb-Systemic-Framework.Rproj
|
|-- Climate Policy/              # Manuscript files
|   |-- CPmain.docx              # Main paper
|   |-- CPSI.docx                # Supplementary information
|   +-- CPCL.docx                # Cover letter
|
|-- Review/                      # Revision materials
|   |-- Comments-CP.docx         # Reviewer comments
|   |-- ResubmitPlan-CP.docx     # Response plan
|   |-- LitRev-CP.docx           # Literature review notes
|   +-- Methodology-Notes.md     # Detailed methodology decisions
|
+-- Code and data/               # Analysis (self-contained)
    |
    |-- _targets.R               # Pipeline definition (targets)
    |
    |-- R/                       # Function files (sourced by targets)
    |   |-- utils.R              # Shared helpers and constants
    |   |-- 01_create_data.R     # Download & process raw inputs
    |   |-- 02_harmonize.R       # Combine sector + non-sector data
    |   |-- 03_reshape.R         # NUTS recombination, complete grid
    |   |-- 04_normalize.R       # Per-employee, min-max [0.01, 0.99]
    |   |-- 05_aggregate.R       # Dimension scores, TRI computation
    |   |-- 06_visualize.R       # Maps and radar charts
    |   +-- 07_sensitivity.R     # Robustness checks
    |
    |-- Code/                    # Standalone scripts (legacy/reference)
    |   |-- 1A-non-sector-data.R ... 6-sensitivity-analysis.R
    |   +-- Create Initial Data/ # Scripts to rebuild Initial data/
    |
    |-- Initial data/            # Input datasets
    |   |-- base_data_plus.xlsx  # NUTS-2 region reference list
    |   |-- Regional_Employment_Weights.xlsx
    |   |-- RIS_2023.xlsx        # Regional Innovation Scoreboard
    |   |-- Sector data/         # Sector x region indicators (17 xlsx)
    |   +-- Non sector data/     # Region-level indicators (12 xlsx)
    |       |-- ENSPRESO_Integrated_Data/  # JRC RE potential (manual download)
    |       |-- qog_eureg.csv              # QoG regional data (manual download)
    |       +-- qog_ei_eureg.csv           # QoG environmental (manual download)
    |
    |-- Derived data/            # Intermediate outputs (reproducible)
    |-- Final data/              # Risk_data.xlsx, Sensitivity_Analysis.xlsx
    +-- Figures/                 # Figures 3-6 (PNG, 600 DPI)
```

## How to reproduce

### Requirements

- **R >= 4.5** with the following packages:
  `targets`, `tarchetypes`, `dplyr`, `tidyr`, `readxl`, `writexl`,
  `stringr`, `janitor`, `purrr`, `ggplot2`, `sf`, `giscoR`,
  `RColorBrewer`, `patchwork`, `fmsb`, `scales`, `restatapi`
- Internet connection (for Eurostat API calls on first run)

### External data (not auto-downloaded)

Three files must be downloaded manually and placed in `Code and data/Initial data/Non sector data/`:

| File | Source | Download from |
|------|--------|---------------|
| `qog_eureg.csv` | QoG EU Regional Dataset (wide, NUTS-2) | [University of Gothenburg](https://www.gu.se/en/quality-government/qog-data/data-downloads/eu-regional-dataset) |
| `qog_ei_eureg.csv` | QoG Environmental Indicators Dataset | Same website |
| `ENSPRESO_Integrated_Data/ENSPRESO_Integrated_NUTS2_Data.csv` | JRC ENSPRESO | [EU Science Hub](https://data.jrc.ec.europa.eu/) (search "ENSPRESO") |

### Run the pipeline

```r
setwd("Code and data")
library(targets)

# Full run (first time, ~1 minute with API calls):
tar_make()

# Subsequent runs skip cached steps:
tar_make()  # only re-runs what changed

# View dependency graph:
tar_visnetwork()

# Load specific results:
tar_read(risk_data)
tar_read(sensitivity_results)
```

### Pipeline targets

| Target | Description |
|--------|-------------|
| `empl_weights` | Regional employment by NUTS-2 x sector (Eurostat API) |
| `hhi_data` | Industrial concentration index per region |
| `policy_pressure` | ETS/CBAM regulatory exposure per sector |
| `scope2_data` | Scope 2 emissions approximation |
| `qog_data` | Quality of Government index (EQI) |
| `climate_laws` | Climate mitigation laws count per country |
| `re_potential` | Renewable energy potential (ENSPRESO) |
| `non_sector_data` | Harmonised region-level indicators |
| `sector_data` | Harmonised sector x region indicators |
| `all_data_long` | Combined long-format dataset |
| `data_reshaped` | Complete region x sector x indicator grid |
| `data_normalized` | Min-max normalised, per-employee adjusted |
| `risk_data` | Final TRI with all dimensions and risk bands |
| `sensitivity_results` | Robustness checks (alpha, weights, LOO) |
| `figure_maps` | Figures 3-4: EU maps |
| `figure_radars` | Figures 5-6: radar comparison charts |

## Data sources

All data is open-access:

| Source | Datasets used | Access |
|--------|--------------|--------|
| **Eurostat** | env_ac_ainah_r2, nrg_bal_c, sbs_r_nuts2021, nama_10r_2gfcf, lfst_r_sla_ga, rd_e_berdindr2, ext_tec09, demo_r_d2jan, nrg_inf_epcrw | [Eurostat API](https://ec.europa.eu/eurostat) via `restatapi` |
| **EEA** | Grid emission factors (gCO2/kWh, 2022) | Hardcoded from EEA CSI 049 |
| **EU regulations** | ETS Directive 2003/87/EC, CBAM Regulation 2023/956 | Hardcoded sector coverage |
| **JRC** | ENSPRESO renewable energy potentials | [EU Science Hub](https://data.jrc.ec.europa.eu/) |
| **EC** | Regional Innovation Scoreboard 2023 | Included in `Initial data/` |
| **QoG Institute** | EU Regional Dataset (EQI), Environmental Indicators | [University of Gothenburg](https://www.gu.se/en/quality-government) |
| **EU Cohesion** | Structural fund payments by NUTS-2 | [Cohesion Open Data](https://cohesiondata.ec.europa.eu/) |

## Methodology summary

The index follows the IPCC AR6 risk framework:

```
Risk = f(Exposure, Vulnerability)
     = Exposure^alpha x Vulnerability^(1-alpha),  alpha = 0.5
```

**Exposure** captures the intensity of transition pressure through three
channels: direct emissions (Scope 1), indirect energy emissions (Scope 2),
and regulatory coverage (ETS + CBAM).

**Vulnerability** captures the capacity (or lack thereof) to absorb and
adapt to transition costs across six dimensions:

| Dimension | What it captures | Key indicators |
|-----------|-----------------|----------------|
| Energy | Energy dependence and transition readiness | Fossil share, RE potential (ENSPRESO) |
| Labour | Workforce adaptability | Unemployment, skills |
| Finance | Investment capacity | GFCF per employee, Cohesion Fund payments |
| Technology | Innovation capacity | Business R&D, Regional Innovation Score |
| Institutions | Governance quality | QoG Index, climate mitigation laws |
| Diversification | Industrial breadth | Employment concentration (HHI) |

All indicators are normalised to [0.01, 0.99] via min-max scaling within
each sector. Dimension scores are equal-weight averages of their
constituent indicators. The geometric mean aggregation ensures that a
region needs both high exposure *and* high vulnerability to score as
high-risk (non-compensatory property).

See `Review/Methodology-Notes.md` for detailed decisions, literature
justifications, and known limitations.

## Sensitivity analysis

The index is tested for robustness against:
- **Alpha parameter**: 0.3 to 0.7 (baseline 0.5)
- **Dimension weights**: equal vs PCA-derived
- **Dimension influence**: leave-one-out analysis
- **Aggregation method**: geometric vs arithmetic mean

Results show high rank stability (Spearman rho > 0.95 for most tests).
See `Final data/Sensitivity_Analysis.xlsx`.

## Citation

> Coppola, G. et al. "A Systemic Framework for Assessing the Risk of
> Decarbonization to Regional Manufacturing Activities in the European
> Union." Submitted to *Climate Policy*.

## License

Code: MIT. Data: subject to original source licenses (Eurostat, JRC, QoG).
