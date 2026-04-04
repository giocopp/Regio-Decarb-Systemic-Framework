# Transition Risk Index for EU Regional Manufacturing

A composite indicator measuring how exposed and vulnerable EU NUTS-2 regions
are to the costs of industrial decarbonisation. Covers 237 regions across
the EU-27, disaggregated by 10 manufacturing subsectors.

The TRI follows the IPCC AR6 risk framework:
**TRI = Exposure^0.5 x Vulnerability^0.5**

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

Three files must be downloaded manually and placed in `Code and data/Initial data/Non sector data/`:

| File | Source |
|------|--------|
| `qog_eureg.csv` | [QoG EU Regional Dataset](https://www.gu.se/en/quality-government/qog-data/data-downloads/eu-regional-dataset) |
| `qog_ei_eureg.csv` | Same website |
| `ENSPRESO_Integrated_Data/ENSPRESO_Integrated_NUTS2_Data.csv` | [JRC ENSPRESO](https://data.jrc.ec.europa.eu/) |

### Run the pipeline

```r
setwd("Code and data")
library(targets)
tar_make()
```
