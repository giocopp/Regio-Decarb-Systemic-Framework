# Exposure revision — carbon-cost-at-risk (Option A)

Working branch: `exposure-carbon-cost-at-risk`. This note records the agreed
design; it is a plan, **not yet implemented**. Nothing here changes the
`main` pipeline until the new code is written and validated.

## 1. Decision

Reframe **Exposure** from a coverage-weighted emissions average to a
**carbon-cost-at-risk** quantity: emissions (quantity) priced by the carbon
price (price), so that ETS and CBAM are internalised as a **cost**, not a
0–2 coverage dummy (`R/01_create_data.R::create_policy_pressure`).

Adopt **Option A — cross-sector normalisation** as the primary index, because
a carbon price varies (at most) by sector/country and has **zero within-sector
variation**; under the current within-sector `range01` it would cancel out
exactly (verified: dropping `Policy_Pressure` or switching `+`→`×` leaves the
index identical, cor = 1.0). Within-sector rankings are retained as a
**stratified analysis** layer in the paper (re-rank / re-normalise within
`Sector_ID` post hoc).

## 2. Concept

Exposure is a **single two-factor product**:

```
Exposure_{r,s} = Emissions_{r,s} × Costs_{s}
```

- **Emissions** (quantity): Scope 1 from EUTL, installation-level → NUTS-2 ×
  sector. Carries the **regional** variation.
- **Costs** (price): one carbon-cost/price factor internalising **ETS + CBAM**.
  Carries the **sector/policy** variation.

It is **not** a sum of separate ETS and CBAM terms, and free allocation is
**not** folded into the emissions side. Under Option A (pooled normalisation)
both factors drive the index: the product varies by region (via Emissions) and
by sector (via Costs).

## 3. Two implementations to build and compare

**Primary — EUTL-based effective carbon cost (ETS / direct term).**
EU Transaction Log gives **installation-level verified emissions and free
allocation** with 4-digit NACE + location. Effective ETS cost:
`(verified_emissions − free_allocation) · EUA_price`, aggregated to
**NUTS-2 × NACE**. Bonus over the current method: installation locations give
the ETS cost a **real regional distribution** instead of employment-share
downscaling (same rationale as the existing EDGAR Scope-1 sensitivity).

**Comparison — benchmark-based sector price (public, lighter).**
`P_ETS_s = EUA_price · (1 − free_allocation_share_s)` from public sector
benchmarks, + CBAM term `= CBAM_coverage_s · EUA_price`. Sector-level only,
no installation/regional detail; fully reproducible from public figures.

Both use a **single EU-wide EUA price series** (EUTL holds quantities/
allowances, *not* the market price — the EUA price comes from EEX/EEA/Ember).

## 4. Precisions / caveats (do not lose these)

- **Free-allocation direction**: the *cost faced* rises with the **auctioned**
  (non-free) share → use `(1 − free_allocation_share)`, **not**
  `× free_allocation_share`. More free allocation = lower cost = lower exposure.
- **EUTL = direct/ETS (Scope 1) only.** It does **not** contain Scope 2,
  Scope 3, or CBAM-embedded import emissions. The **CBAM term needs a separate
  source** (trade × carbon intensity — closest to the existing FIGARO/Scope 3
  approach), or stays a `coverage × EUA` proxy for now. [OPEN DECISION #2]
- **EUTL coverage is concentrated** in heavy sectors (≈ C19-C20, C23, C24,
  parts of C16-C18). Light sectors (textiles, electronics, motor vehicles)
  have little/no ETS coverage → near-zero ETS cost. This is correct, but
  shapes interpretation and the cross-sector spread.
- **Free allocation phases out 2026–2034** as CBAM phases in → the effective
  price is **year-dependent**. Pick and document a reference year aligned with
  the emissions vintage. [OPEN DECISION #3]
- **Non-Eurostat sources** (EUTL, EUA price, benchmarks). Precedent exists in
  the project (EEA/Ember EFs, JRC ENSPRESO, QoG, EDGAR, EC RIS) but document
  the addition; keep it out of any LCA / product-lifecycle territory.

## 5. Open decisions (resolve before/while coding)

1. **Normalisation consistency in `√E · √V`.** If Exposure is pooled
   (cross-sector) but Vulnerability stays within-sector, the two enter the
   geometric Risk on different scales. Options: (a) pool **both** E and V for
   the primary cross-sector Risk; (b) pool E only and justify; (c) compute a
   cross-sector Risk as primary + a within-sector Risk as the analysis layer.
   **This is the pivotal fork.**
2. **CBAM term source** (trade × intensity vs coverage × EUA proxy).
3. **Reference year** for EUA price + free allocation.
4. **EUTL → NACE crosswalk** to the 12 manufacturing aggregates — verify a
   sample maps cleanly to C19-C20, C23, C24, etc. before committing.

## 6. Implementation plan (keep `main` reproducible)

- Add the new exposure as a **variant**, not a replacement: new creator(s)
  for the carbon-price/cost terms, a new `aggregate_risk` path or argument,
  and a new normalisation scope flag (within-sector vs pooled).
- Reuse `range01` but parameterise the grouping (drop `group_by(Sector_ID)`
  for the pooled variant).
- Add a sensitivity-table row comparing baseline TRI vs carbon-cost-at-risk
  TRI (Spearman), and EUTL vs benchmark-based price (Spearman), per §14.
- Keep `Policy_Pressure` available for backward comparison.

## 7. To verify before writing data code (anti-hallucination)

- [ ] Download an EUTL sample; confirm fields (verified emissions, free
      allocation, NACE, installation location/NUTS) and the NACE crosswalk.
- [ ] Obtain the EUA annual price series and chosen reference year.
- [ ] Locate the public free-allocation benchmark shares per sector.
- [ ] Confirm CBAM product list → NACE mapping for the import term.

## 8. Updates — 2026-06 (decisions resolved + verified data facts)

**Normalisation (open decision #1): RESOLVED → (a).** Pool **both** Exposure
and Vulnerability for the primary cross-sector Risk; within-sector rankings are
a stratified analysis layer in the paper.

**Exposure = two priced terms (not "EUTL emissions" alone):**
- **ETS (direct):** from EUTL, `(verified_emissions − free_allocation) ×
  EUA_price`. The point of EUTL is that it carries **both** verified emissions
  **and** free allocation — use the free-allocation field; this is the ETS
  *cost*, not raw emissions.
- **CBAM (imports):** covered-goods imports × embedded carbon intensity ×
  EUA, built on the FIGARO / `Import_ExtraEU` machinery. **OPEN — direction:**
  CBAM is a *cost* to EU importers of covered goods but *protection* for EU
  *producers* of covered goods (offsets leakage as free allocation phases out).
  Decide which channel the index represents (mirrors the `Export_ExtraEU`
  direction debate Reviewer 2 raised).

**EUTL — verified facts (web search, 2026-06):**
- Fields present: activity type, physical address, verified emissions,
  allocated + surrendered allowances → emissions **and** free allocation. Good.
- **No native NACE.** EUTL uses its own ETS activity classification, *distinct
  from NACE*. Needs an ETS-activity→NACE crosswalk (or a curated source, e.g.
  Jan Abrell's EUETS.info) and validation against our 12 aggregates; not 1:1.
- **No pre-coded NUTS.** Installations carry a physical address → must geocode
  to NUTS-2. This is exactly what buys the regional ETS-cost distribution, but
  it is real work.
- **Covered subset only.** ETS ≈ 36% of EU GHG; 20 MW combustion threshold +
  listed heavy activities (steel, cement, lime, glass, ceramics, pulp/paper,
  aluminium, refineries, petrochemicals, ammonia/nitric/adipic). Light sectors
  (textiles, electronics, motor vehicles, food) are largely absent → near-zero
  ETS cost. This is *correct* for a cost measure, and means heavy industry will
  dominate the cross-sector Exposure gradient (the gradient within-sector
  normalisation was hiding).

**Recommended sequencing:** build and validate the **ETS term first** (EUTL net
cost → pooled cross-sector index; compare Spearman vs baseline TRI), **then**
add the CBAM term (harder data + unresolved direction). De-risks the more
debatable piece.

## 9. Build status (latest)

Final structure (real-unit carbon cost at risk, pooled):
`Exposure_raw = Scope1_t × P_ETS + CBAM_emb_t × P_CBAM`, then `Exposure = range01(Exposure_raw)` **POOLED** (no `group_by(Sector_ID)`).

- **Engine — DONE.** `R/exposure_cost.R::assemble_exposure_cost()` (pooled,
  multiplicative; `within_sector=` flag for diagnostics). `free_alloc_share` is a
  **per-row** column, so it already supports country- or region-level price
  variation. Verified on synthetic data: pooled → price matters (ρ=0.50);
  within-sector → price washes out (ρ=1, max|diff|=1e-16). Test:
  `prototypes/test_exposure_cost.R`.
- **CBAM leg — DONE.** `compute_cbam_leg()` → NUTS-2 × sector embodied carbon in
  extra-EU imports of covered goods (FIGARO 2023): 2,819 cells, 235 NUTS-2,
  259.4 Mt, 0 NAs; top cells Lombardia/NRW/Veneto/Cataluña (steel, chemicals).
  Price factor `cbam_cov` is **EU-wide** (Reg. 2023/956) — the regulation
  applies uniformly, so CBAM price does not vary by country.
- **ETS leg — IN PROGRESS.** Data in hand: `Initial data/eea_t_eu-emission-
  trading-scheme_.../ETS_Database_April_2026.xlsx` (country × `main_activity_code`
  × year; has "1.1 Freely allocated allowances" and "2. Verified emissions").
  **DECISION: ETS price factor varies at COUNTRY × SECTOR** —
  `free_alloc_share[country, sector] = freely_allocated / verified`, so P_ETS
  carries cross-country (within-sector) variation. Remaining: map EEA
  `main_activity_code` → NACE → 12 sectors using the activity-code labels in the
  bundled user-manual PDF (NOT from memory); handle the cross-cutting
  "Combustion of fuels" code; join on Country_ID × Sector_ID.
- **Vintage:** price/free-allocation = latest (2024/25); quantities at latest
  available (FIGARO 2023, Scope 1 latest Eurostat) — document the mismatch.

## 10. First full assembly — result + open items

ETS leg + country×sector free-allocation + CBAM leg now assembled end-to-end
(`prototypes/ets_assembly.R`). Inputs: EEA `ETS_Database_April_2026.xlsx` (2024,
GR→EL), crosswalk `ets_activity_nace_crosswalk.csv`, FIGARO 2023, `scope1_data`.

- **`free_alloc_share` by country × sector** built (90 cells). EU means: C23
  0.96, C24 0.94, C16-C18 0.87, C19-C20 0.79 (heavy free allocation).
- **Pooled multiplicative Exposure** computed (2,820 cells, 0 NAs). Top cells:
  Lombardia (steel/chemicals), Cataluña, Zuid-Holland, German chemical belt,
  Île-de-France — EU heavy-industry heartlands.
- **CBAM cost (0.13) > ETS net cost (0.07)** at 2024 free-allocation levels
  (ETS heavily discounted by free allowances); this flips as free allocation
  phases out 2026–2034 — a key sensitivity to run.
- **Spearman vs baseline within-sector Exposure = 0.07** — a genuinely different
  (cross-sector) index, by design (Option A).

**Open items:**
1. **Crosswalk validation.** `ets_activity_nace_crosswalk.csv` labels are
   standard EU ETS Annex I + magnitude-corroborated, NOT from an official label
   file (the bundle/EEA pages don't expose code→label). Validate against the
   viewer's "Main Activity Sector Name". Code 10 vs 50 (aviation/legacy) and the
   combustion code (20) treatment especially.
2. **"C" total-manufacturing aggregate** currently ≈ 0 (no process code maps to
   "C", and cbam_cov not set for "C"). Needs aggregate handling.
3. Only 4 sectors carry ETS, 3 carry CBAM → other 7 manufacturing sectors ≈ 0
   exposure (correct for a cost measure; the heavy-industry gradient).
4. ETS quantity = total Scope 1 (slightly > ETS-covered portion for heavy
   sectors); EUA scalar = 1 (ranking-irrelevant); free_alloc_share capped at 1
   (over-allocated → P_ETS = 0, no negative exposure).
5. Next: pooled Vulnerability → full TRI; free-allocation phase-out sensitivity.

## 11. Full TRI + sensitivity — done (a, b, c)

`prototypes/build_cost_tri.R` → `Final data/Risk_data_carbon_cost_PROTOTYPE.csv`.
`normalize_indicators()` gained a backward-compatible `pool=` flag.

- **(b) Pooled TRI.** Exposure (cost, pooled) × Vulnerability (dimensions
  re-normalised **pooled** over the 11 sub-sectors), `Risk = range01(√E·√V)`
  pooled. Top cells: ITC4 (Lombardia) C19-C20 & C24, ES51 (Cataluña), German
  chemical belt, plus high-vulnerability EL30 (Attica). Spearman vs baseline
  within-sector TRI = 0.64 (Vulnerability carries the shared signal; exposure
  reshuffles).
- **(a) "C" total fixed.** C = per-region roll-up of sub-sector carbon costs,
  range01 across regions (separate from the sub-sector pool, since C is the
  aggregate). 235/235 non-zero; top = Lombardia.
- **(c) Free-allocation phase-out sensitivity.** As free allocation goes
  100%→50%→0%, the **ETS share of total carbon cost rises 35% → 72% → 82%**:
  the index is **CBAM-dominated today, ETS-dominated after phase-out**. Ranking
  is robust (Spearman current-vs-full-phase-out = 0.90).

**KEY SCOPING CONSEQUENCE (needs a call):** only **796 of 2,497** sub-sector
cells have positive carbon-cost risk; the other 1,701 are zero-exposure (the 7
light sectors face neither ETS-process nor CBAM cost). So this is effectively a
**heavy-industry carbon-cost risk index** (≈ C16-C18, C19-C20, C23, C24). This
follows directly from "exposure = carbon cost," but it is a substantive change
from the 12-sector baseline — decide whether that scope is intended.

**Still prototype, not wired into `_targets`:** the build is a script; refactor
`compute_fa()`/`build_tri()` into `R/exposure_cost.R` + targets once the scope
question is settled. EUA scalar = 1 (ranking-irrelevant).

## 12. Combined exposure (emissions + carbon cost) — keeps ALL sectors

DECISION (supersedes the §11 scope question): Exposure is a **composite of two
pooled facets**, not carbon cost alone:

```
Exposure = range01_pooled( w·Emissions_n  +  (1−w)·CarbonCost_n ),   w = 0.5
  Emissions_n  = pooled-normalised mean(Scope1, Scope2, Scope3)   (ALL sectors)
  CarbonCost_n = pooled-normalised (Scope1·P_ETS + CBAM_emb·P_CBAM) (priced sectors)
```

This resolves the original +/× question: **emissions is the additive base
(every sector is exposed in proportion to what it emits); policy enters
multiplicatively as carbon cost; the two facets are averaged.**

Result (`prototypes/build_cost_tri.R` → `Final data/Risk_data_carbon_cost_PROTOTYPE.csv`):
- **2,496 / 2,497 cells positive** (was 796) — **all 11 sub-sectors covered**.
  Per-sector mean exposure: C19-C20 > C24 > C23 > C10-C12 (food) > … >
  C13-C15 (textiles). Heavy industry tops; light sectors retained via emissions.
- Spearman vs baseline TRI = 0.64; vs **emissions-only = 0.95** (policy/cost
  still shifts the ranking but the index is emissions-dominated at w=0.5).
- Phase-out sensitivity still holds (Spearman current-vs-full = 0.99; less
  sensitive than the cost-only index because cost is now a smaller share).

**Key tunable:** `w` (emissions vs carbon-cost weight). w=0.5 → emissions-
dominated; lower w → policy matters more. Decide the weight (or report a small
sensitivity over w).

**Next:** pick `w`; refactor prototype → `R/exposure_cost.R` + `_targets`;
maps/figures; optional EUA € value for an interpretable cost.

## 13. Log transform on the exposure facets

Both facets are right-skewed (a few mega-emitters dominate), so plain min-max
crushed most cells near 0. Each facet is now `rescale(log1p(raw))` before the
0.5/0.5 average: `Exposure = rescale( 0.5·rescale(log Emissions) +
0.5·rescale(log CarbonCost) )` (log1p preserves true zeros). Effect (verified):
Exposure **sd 0.06 → 0.22**, **IQR 0.03 → 0.37** — much more variability;
sectors now spread chemicals 0.73 … textiles 0.17 (was 0.115 … 0.013). Top
cells shift toward high-vulnerability regions (Catalonia, Attica, Lombardia);
Spearman vs baseline 0.47 (down from 0.64 — log lets vulnerability matter more
relative to a few huge emitters). Implemented in `prototypes/build_cost_tri.R`.
