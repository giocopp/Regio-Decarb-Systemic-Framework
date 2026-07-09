# EUTL installation-level data (EUETS.info / Abrell 2024)

Source for the **ETS component** of the carbon-cost-at-risk Exposure: installation-level verified emissions + free allocation, geocoded to NUTS-2 / NUTS-3.

## Provenance
- Dataset: Jan Abrell, *Database for the European Union Transaction Log* (EUTL), release `eutl_2024_202410` (compiled 2024-11-06).
- Download: `https://euets-info-public.s3.eu-central-1.amazonaws.com/eutl_2024_202410.zip` (URL hardcoded in `pyeutl/utils.py`; index at https://www.euets.info/download).
- Accessed: 2026-06-08. Latest year with real verified emissions: **2023**.
- Cite: Abrell, J. (2024). *Database for the European Union Transaction Log*. euets.info.
- License: publicly provided for research; confirm terms at euets.info before redistribution.

## Files (derived; regenerate with `data_builders/ets_geocode.R`)
- `ets_installations_geocoded.csv` — one row per EU27 manufacturing installation: `id, Country_ID, activity_id, Sector_ID, nace_id, NUTS3_ID, NUTS_ID, verified, allocatedFree`.
- `ets_nuts2_sector.csv` — NUTS-2 × sector: `ets_emis_t` (verified, t), `alloc_free_t`. **ETS-component quantity.**
- `ets_nuts3_sector.csv` — NUTS-3 × sector (extra granularity).
- `ets_country_sector_freealloc.csv` — Country × sector `free_alloc_share` (for the price).

## Method (`data_builders/ets_geocode.R`)
- Manufacturing = (a) EUTL **process activity codes 21–44** (validated against the official `activity_type.csv`), mapped to C16-C18 / C19-C20 / C23 / C24 by activity, **plus (b) activity-20 fuel-combustion installations attributed to a manufacturing sector via the installation NACE code** (divisions 10–33 → the 11 sub-sectors). NACE-35 power/heat, non-manufacturing NACE, and combustion installations without a NACE code (≈10%) stay excluded.
- Point-in-polygon of `latitudeGoogle`/`longitudeGoogle` onto giscoR NUTS-3 (2021, res "03"), EPSG:3035; NUTS-2 = first 4 chars; Croatia `HR02/HR05/HR06 → HR04` to match the 230-region grid.
- 6,154 EU27 manufacturing installations (4,179 process + 1,975 activity-20 combustion with manufacturing NACE), **100% matched**, 0 country mismatches.

## Raw data (NOT committed)
The raw zip (`installation.csv` 5 MB, `compliance.csv` 61 MB, `transaction.csv` 164 MB) is not version-controlled. Re-download from the URL above and extract to `/tmp/eutl_validation/` (or edit `RAW` in `data_builders/ets_geocode.R`).
