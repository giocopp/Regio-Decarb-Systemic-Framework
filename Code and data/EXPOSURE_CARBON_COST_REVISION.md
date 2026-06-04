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

Carbon cost at risk maps **scopes to instruments** (consistent with the
existing comment in `create_policy_pressure`: "ETS prices direct emissions,
CBAM prices the carbon content of imports"):

```
CarbonCost_{r,s} ≈  Scope1_{r,s} · P_ETS_{s}        (direct, net of free allocation)
                  + CoveredImports_{r,s} · P_CBAM_{s} (embedded carbon in imports)
```

i.e. a **sum of quantity × price terms**, not a single product. Scope 2 is
priced indirectly via the electricity price already embedded in the grid-EF
Scope 2 series; Scope 3 (other than CBAM imports) is largely unpriced.

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
