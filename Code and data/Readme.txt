README — Code and data
Project: A Systemic Framework for Assessing the Risk of Decarbonization to Regional Manufacturing Activities in the European Union

What this repo does
-------------------
Builds the Transition‑Risk Index (TRI) for EU manufacturing at NUTS‑2 × NACE Rev.2, and reproduces the article's Figures 3–6:
• Exposure (emissions by region/sector)
• Vulnerability (composite index)
• Risk = Exposure ⊗ Vulnerability
Outputs include intermediate tables, the final analysis dataset, and publication figures.

Folder of interest
------------------
Code and data/
  ├─ Code/                    # R scripts (run in order 1A → 5)
  │  └─ Create Initial Data/  # scripts to (re)build Initial data/ from Eurostat API with imputation
  ├─ Initial data/            # Eurostat inputs imputed for missing values
  ├─ Derived data/            # intermediates
  ├─ Final data/              # final analysis-ready tables
  └─ Figures/                 # exported figures

Quick start
-----------
1) Open R (or RStudio) and set the working directory to "Code and data/Code".
   Example: setwd(".../Code and data/Code")
2) Initial data/ contains the required Eurostat inputs referenced by the scripts.
   To reproduce the inputs in Initial data/, run the scripts in Code/Create Initial Data/.
   These scripts download the required datasets from the respective API and impute missing values; outputs are written to ../Initial data/.
3) Run scripts sequentially (they use relative paths to ../Raw data, ../Derived data, etc.):
   1A-non-sector-data.R  →  1B-sector-data.R  →  1C-all-data.R  →  2-reshape-data.R  →  3-normalize-data-by-n-enterpr.R  →  4-risk-aggregation.R  →  5A-results-maps.R, 5B-results-radar.R

What each script does
---------------------
Create Initial Data — Recreate inputs
• Download source tables from the Eurostat API or the Localised DSP, apply light cleaning, and impute missing values.
• Write the resulting inputs to Initial data/ for the analysis pipeline.

1A / 1B / 1C — Build the unified base
• Read individual Eurostat datasets (some sectoral, some only regional).
• Harmonize, aggregate, and align them into a single analysis-ready schema.
• Write aggregated outputs to Derived data/ (and/or Final data/ as needed).

2 — Reshape and enrich
• Reshapes the combined data and adds metadata.
• Output: Derived data/2_All_data_long_READY.xlsx (all raw indicators in tidy long format, with indicator names and values).

3 — Normalize (per‑employee & min–max)
• Divide selected indicators by number of employees (per‑employee normalization).
• Apply min–max normalization to [0.01, 0.99].
• Exposure keeps real zeros (zeros remain zero).
• Write normalized tables to Derived data/ or Final data/.

4 — Aggregate indices
• Exposure: multi‑component (GHG intensity, Scope 2, Policy Pressure from ETS/CBAM).
• Vulnerability: build dimension scores (Energy, Labour, Finance, Supply Chain, Technology, Institutions, Diversification).
  – Within each dimension: equal‑weight mean of directionally aligned, [0.01,0.99]‑scaled indicators.
  – Dimension scores are re‑scaled to [0.01,0.99].
  – Stranded Asset Proxy (Fossil_Share × GFCF) added to Finance dimension.
• Aggregate Vulnerability: equal‑weight mean of all dimensions.
• TRI (Risk): combine Exposure (E) and Vulnerability (V) via a weighted geometric rule with α = 0.5 in the baseline.
  – Policy‑intuitive properties: high E & high V → high risk; imbalances penalized (non‑compensatory).
• Output: Final data/ (main analysis tables).

6 — Sensitivity analysis
• Alpha sensitivity: vary α in {0.3, 0.4, 0.5, 0.6, 0.7}.
• Weighting sensitivity: equal vs PCA‑derived dimension weights.
• Leave‑one‑dimension‑out: assess influence of each dimension on rankings.
• Aggregation sensitivity: geometric vs arithmetic mean.

5A / 5B — Visualize results
• 5A: maps and distribution plots for Exposure, Vulnerability, and Risk → Figures 3 & 4.
• 5B: comparative/radar profiles for selected countries/regions/subsectors → Figures 5 & 6.
• Outputs saved to Figures/.

Data flow (at a glance)
-----------------------
Initial data → (1A–1C) Aggregated base → (2) Tidy long file → (3) Normalized indicators → (4) Composite indices → (5) Figures
                  |                       |                                                |
                  v                       v                                                v
            Derived data/       2_All_data_long_READY.xlsx                       Final data/ & Figures/

Assumptions & conventions
-------------------------
• Geography: EU NUTS‑2 regions; Sector: NACE Rev.2 manufacturing subsectors.
• Equal weights by default where no evidence supports alternatives.
• All indicators directionally aligned so that higher = greater transition difficulty.
• Relative paths assume working directory = Code/.

Reproducing the article
-----------------------
• Optionally run the codes from the Code/Create Initial Data subfolder
• Run 1A → 5B without interruption to regenerate:
  – Figure 3: Exposure, Vulnerability, Risk maps (total manufacturing).
  – Figure 4: Vulnerability dimensions (Energy, Labour, Finance, Supply Chain, Technology).
  – Figure 5: Drivers of risk—highest vs. lowest risk regions (example: Germany & Greece).
  – Figure 6: Subsector case (e.g., basic metals) by selected regions.
• Final tables for the paper live in Final data/. Figures export to Figures/.

Requirements
------------
• R (version per your environment).
• Install any missing packages listed at the top of each script (e.g., install.packages("...")).

Troubleshooting
---------------
• "File not found": check that the previous script finished and wrote its outputs to the expected folder.
• Paths: confirm getwd() ends with /Code so relative paths resolve to ../Raw data, ../Derived data, etc.
• Reruns: optionally clear Derived data/, Final data/, and Figures/ before a clean rebuild.

Provenance & citation
---------------------
• Inputs: Eurostat and related sources cited in the paper and headers of the scripts.
• Methods: OECD composite‑indicator guidance; IPCC AR6 risk framing (see paper references).
• If you use this code, please cite the article:
  A Systemic Framework for Assessing the Risk of Decarbonization to Regional Manufacturing Activities in the European Union.
