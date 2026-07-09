# Transition Risk Index for EU Regional Manufacturing

A composite indicator measuring how exposed and vulnerable EU NUTS-2 regions are to the costs of industrial decarbonisation. Covers 237 regions across the EU-27, disaggregated by 10 manufacturing subsectors.

The TRI follows the IPCC AR6 risk framework: **TRI = Exposure^0.5 x Vulnerability^0.5**

## Repository structure

```
Regio-Decarb-Systemic-Framework/
|
|-- README.md
|-- .gitignore
|
+-- Code and data/
    |-- _targets.R               # Pipeline definition (targets)
    |-- R/                       # Function files (sourced by targets)
    |-- Code/                    # Standalone scripts (legacy/reference)
    |-- Initial data/            # Input datasets
    |-- Derived data/            # Intermediate outputs (reproducible)
    |-- Final data/              # Risk_data.xlsx, Sensitivity_Analysis.xlsx
    +-- Figures/                 # Output figures (PNG, 600 DPI)
```

## How to reproduce

**Requirements**: R >= 4.5, internet connection for Eurostat API calls on first run.

### External data (not auto-downloaded)

Three files must be downloaded manually and placed under `Code and data/Initial data/`:

| File | Source |
|------|--------|
| `Non sector data/qog_ei_eureg.csv` | [QoG Environmental Indicators](https://www.gu.se/en/quality-government/qog-data/data-downloads/environmental-indicators-dataset) |
| `Non sector data/ENSPRESO_Integrated_Data/ENSPRESO_Integrated_NUTS2_Data.csv` | [JRC ENSPRESO](https://data.jrc.ec.europa.eu/) |
| `Non sector data/TECH-RIS.xlsx` | [EC Regional Innovation Scoreboard](https://research-and-innovation.ec.europa.eu/statistics/performance-indicators/regional-innovation-scoreboards) — annual bulk Excel, no stable URL. Place the latest release at this path. |

The EQI institutions input (`Non sector data/qog_eqi_long_24.csv`, [EQI standalone release](https://www.gu.se/en/quality-government/qog-data/data-downloads/european-quality-of-government-index)) is small and committed to the repo. All other inputs are auto-downloaded from Eurostat at pipeline run-time.

### How years are chosen

Every indicator independently selects the **latest year for which Eurostat has complete EU-27 coverage** (the threshold is per-country and, for sector indicators, per (country × NACE sector)). The chosen years can differ across indicators — e.g. Unemployment may be 2024 while FIGARO-derived Scope 3 may be
2023. After each `tar_make()` run, `Final data/Coverage_Report.xlsx` lists the year each indicator picked along with its Eurostat source dataset.

If Eurostat publishes new data, simply re-run `tar_make()` (after `tar_destroy()` if you need to invalidate the cache); the year selector will pick up the new vintage automatically.

### Downscaling methodology (consistent across all national-source indicators)

- **Extensive quantities** (€, kt CO2eq, GWh, persons, count) → employment-share downscaling. For each (Country, Sector) the national value is split across the country's NUTS-2 regions in proportion to that region's share of national manufacturing employment in that sector.
- **Intensive quantities** (ratios, indices, percentages) → uniform replication. Each NUTS-2 region of a country receives the same national value.

### Run the pipeline

```r
setwd("Code and data")
library(targets)
tar_make()
```

### Reproducibility model

The committed xlsx files in `Code and data/Initial data/` and `Code and data/Final data/`, together with the PNGs in `Code and data/Figures/`, are the **published snapshot**. Anyone who clones the repo at the paper's submission commit (or tag) sees the exact numbers that appear in the manuscript without re-running anything.

Re-running `tar_make()` overwrites those files with whatever vintage Eurostat currently publishes. The pipeline is intentionally rolling: every indicator picks its own latest year with ≥95% EU-27 NUTS-2 coverage (see `Coverage_Report.xlsx` after each run). The git diff between two commits is then the audit trail of how the rolling vintage moves over time.

For a strict replication of the paper:
1. `git checkout <submission-tag>` (or the relevant commit hash)
2. Read the xlsx files directly. No `tar_make()` needed.

For an updated run against the latest Eurostat data:
1. `tar_destroy()` to clear the cache
2. `tar_make()` to refetch and rebuild everything
3. Inspect `Final data/Coverage_Report.xlsx` to see which year each indicator picked.
