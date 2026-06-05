# Literature supporting the carbon-cost-at-risk Exposure

Consolidated references that justify each methodological choice in the redesigned
Exposure (see `EXPOSURE_CARBON_COST_REVISION.md` for the methodology itself).
Feeds the paper's Methodology and updated Literature Review.

**Provenance / caveat.** Sources were gathered via academic (Consensus) and web
searches during the redesign; links are included so each can be verified. A few
items (Vrontisi et al.; Sokolowski et al.; JRC Coal Regions in Transition) are
carried from the project's existing review (`Review/LitRev-CP.docx`) and should be
cross-checked there for full bibliographic detail. Citation counts/years are as
returned by the search tools at time of writing.

---

## 1. Risk as a conjunctive (multiplicative) function — IPCC AR6

The index follows the IPCC AR6 framing of risk as a function of **hazard,
exposure and vulnerability**, which are *conjunctive*: risk materialises only
where the components co-occur. This is the conceptual basis for combining
Exposure and Vulnerability **multiplicatively** (`TRI = E^0.5 · V^0.5`) rather
than additively.

- Simpson, N. et al. (2021). *A framework for complex climate change risk assessment.* One Earth. [link](https://consensus.app/papers/details/7870bcc2bfe75325b451f9f8b65bdf1d/?utm_source=claude_desktop)
- Sharma, J. & Ravindranath, N.H. (2019). *Applying the IPCC 2014 framework for hazard-specific vulnerability assessment under climate change.* Environmental Research Communications. [link](https://consensus.app/papers/details/a41d525c6d9258189e07368776cc6c0b/?utm_source=claude_desktop)
- Fuchs, S. et al. (2024). *The ambiguity in IPCC's risk diagram raises explanatory challenges.* Natural Hazards. [link](https://consensus.app/papers/details/1ccc5d0d386b596a85919eba1200b688/?utm_source=claude_desktop) — *caution: naive superposition of the three components is criticised; supports doing the combination deliberately.*

## 2. Multiplicative / non-compensatory aggregation (× not +)

Defining Exposure as a product (emissions × carbon price) rather than an average
is a **non-compensatory** choice: a sector with emissions but no carbon price —
or vice versa — should not score as exposed. The composite-indicator literature
treats additive aggregation as fully *compensatory* and geometric/multiplicative
aggregation as the standard *partially/non-compensatory* alternative.

- OECD & JRC — Nardo, M., Saisana, M., Saltelli, A., Tarantola, S. (2008). *Handbook on Constructing Composite Indicators: Methodology and User Guide.* [PDF](https://knowledge4policy.ec.europa.eu/sites/default/files/jrc47008_handbook_final.pdf) — the canonical reference on compensability and aggregation; also the basis for §6 (data transformation).
- Munda, G. & Nardo, M. (2009). *Noncompensatory/nonlinear composite indicators for ranking countries: a defensible setting.* Applied Economics. [link](https://consensus.app/papers/details/1b0a312436e75af99abe772c8d3dadbc/?utm_source=claude_desktop)
- Munda, G. & Saisana, M. (2011). *Methodological considerations on regional sustainability assessment based on multicriteria and sensitivity analysis.* Regional Studies. [link](https://consensus.app/papers/details/08982a1b36625d64871323757ee867ab/?utm_source=claude_desktop) — non-compensatory aggregation on **EU regional** composites specifically.
- Greco, S., Ishizaka, A., Tasiou, M., Torrisi, G. (2018). *On the methodological framework of composite indices: a review of weighting, aggregation, and robustness.* Social Indicators Research. [link](https://consensus.app/papers/details/1446d7b52cf9532193235a86005aa799/?utm_source=claude_desktop)
- Rogge, N. (2017). *Composite indicators as generalized benefit-of-the-doubt weighted averages.* European Journal of Operational Research. [link](https://consensus.app/papers/details/4d4de7dfc4915bf78656c3cde5e7b5cf/?utm_source=claude_desktop)
- Dialga, I. & Giang, L.T.H. (2017). *Highlighting methodological limitations in the steps of composite indicators construction.* Social Indicators Research. [link](https://consensus.app/papers/details/65eac637bf0759ad9b9495953ef372dc/?utm_source=claude_desktop) — geometric vs linear materially changes rankings (motivates the sensitivity test).
- Mariani, F. et al. (2022). *Aggregating composite indicators through the geometric mean: a penalization approach.* Computation. [link](https://consensus.app/papers/details/e914f2e747a859ba87705d782f6a9eee/?utm_source=claude_desktop) — remedy if geometric aggregation over-penalises low components.

## 3. Exposure as "carbon cost at risk" (emissions × carbon price)

The core reframing — Exposure as the financial cost of emissions under carbon
pricing (quantity × price) — is the standard object of climate-transition stress
testing and carbon-premium asset pricing.

- Battiston, S., Mandel, A., Monasterolo, I., Schütze, F., Visentin, G. (2017). *A climate stress-test of the financial system.* Nature Climate Change. See also the **Climate Policy Relevant Sectors (CPRS)** taxonomy. [CPRS project](https://www.df.uzh.ch/en/people/professor/battiston/projects/CPRS.html)
- Bolton, P. & Kacperczyk, M. (2023). *Global pricing of carbon-transition risk.* The Journal of Finance. [link](https://onlinelibrary.wiley.com/doi/10.1111/jofi.13272) — the carbon premium rises with policy stringency (emissions × policy interact, not add).

## 4. ETS + CBAM as the carbon price; free allocation

ETS and CBAM are the two operative EU carbon-pricing instruments. CBAM
certificates are priced at the EUA price; free allocation discounts the effective
ETS rate today and is phased out 2026–2034 in parallel with CBAM — the basis for
the phase-out sensitivity.

- European Commission — Carbon Border Adjustment Mechanism (Reg. (EU) 2023/956). [EC CBAM](https://taxation-customs.ec.europa.eu/carbon-border-adjustment-mechanism_en)
- EEA — EU ETS data viewer (Union Registry: verified emissions, free allocation by country × activity). [EEA](https://www.eea.europa.eu/en/analysis/maps-and-charts/emissions-trading-viewer-1-dashboards)
- OECD (2025). *Effective Carbon Rates.* [OECD](https://www.oecd.org/en/publications/effective-carbon-rates-2025_a5a5d71f-en.html) — for context on effective vs nominal carbon prices.

## 5. CBAM and embodied carbon in trade (MRIO)

The CBAM leg = embodied carbon in extra-EU imports of covered goods, computed
from the FIGARO multi-regional input–output tables — consistent with the
established MRIO approach to CBAM, including a CMCC-authored application.

- Rocchi, P., Campo Lobato, E., **Di Bella, A.**, Bosetti, V. (2026). *Expanding carbon pricing boundaries and the EU CBAM: insights into China and India.* Journal of Cleaner Production. [SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4997200) · [ScienceDirect](https://www.sciencedirect.com/science/article/pii/S0959652626001915) — FIGARO/FIDELIO MRIO; the CMCC reference for the CBAM/embodied-trade approach.
- Amendola, M. (2025). *Winners and losers of the EU carbon border adjustment mechanism. An intra-EU issue?* Energy Economics. [link](https://consensus.app/papers/details/448d1ffbe1985f3abca01ec229cc2525/?utm_source=claude_desktop) — MRIO; pronounced intra-EU redistribution.
- Clora, F. et al. (2023). *Alternative carbon border adjustment mechanisms in the European Union and international responses.* Energy Policy. [link](https://consensus.app/papers/details/cbcc343e911252d9b9d1030c6f05eb53/?utm_source=claude_desktop) — Scope-1 vs Scope-1&2 import-intensity basis (a modelling choice for our CBAM leg).
- Zhong, J. & Pei, J. (2023). *Carbon border adjustment mechanism: a systematic literature review.* Climate Policy. [link](https://consensus.app/papers/details/681af0cc27b25452ae64d78e18544495/?utm_source=claude_desktop)
- Böhringer, C. et al. (2022). *Potential impacts and challenges of border carbon adjustments.* Nature Climate Change. [link](https://consensus.app/papers/details/faf907ce9138522a94d4141851a53133/?utm_source=claude_desktop)
- FIGARO (Eurostat MRIO) and the JRC **FIDELIO** model. [FIDELIO/JRC](https://publications.jrc.ec.europa.eu/repository/handle/JRC141962)

## 6. Cross-sector (pooled) normalisation and the log transform

Pooling the normalisation across sectors ("Option A") is required for a
between-sector price signal to survive; min-max is invariant to within-group
constants, so a within-sector scheme erases any sector-level price. Extensive,
right-skewed quantities (emissions, carbon cost) are log-transformed before
scaling — standard practice for skewed indicators per the JRC Handbook.

- Nardo et al. (2008), *Handbook* (above) — normalisation methods and treatment of skewed data / outliers.
- (Mechanism verified internally: within-sector min-max washes out a sector-level price; see `EXPOSURE_CARBON_COST_REVISION.md` §1.)

## 7. Positioning vs other regional transition-risk indices

Comparators that structure regional transition risk multiplicatively
(Hazard × Exposure × Vulnerability) and against which our index is benchmarked:

- *Towards a just transition: Identifying EU regions at a socioeconomic risk of the low-carbon transition* (2024). [ScienceDirect](https://www.sciencedirect.com/science/article/pii/S2666278724000059)
- Vrontisi et al. (2024); Sokolowski et al. (2022); JRC **Coal Regions in Transition** — *as cited in the project's existing review (`Review/LitRev-CP.docx`); confirm bibliographic detail there.*
- Regional Competitiveness Index (RCI 2.0), EC/JRC — broad regional benchmark used for comparison.

---

## Quick map: choice → primary support

| Methodological choice | Primary references |
|---|---|
| Multiplicative `E^0.5·V^0.5` (conjunctive risk) | IPCC AR6 (Simpson 2021; Sharma 2019) |
| Exposure = emissions × price (non-compensatory) | Nardo et al. 2008; Munda 2009; Munda & Saisana 2011; Greco 2018; Rogge 2017 |
| Carbon cost at risk | Battiston et al. 2017 (CPRS); Bolton & Kacperczyk 2023 |
| ETS+CBAM pricing, free-allocation phase-out | EC CBAM Reg. 2023/956; EEA ETS viewer |
| CBAM via embodied carbon in imports (MRIO) | Rocchi/Di Bella et al. 2026; Amendola 2025; Clora et al. 2023 |
| Pooled normalisation + log of skewed indicators | Nardo et al. 2008 |
| Comparators / positioning | Vrontisi et al. 2024; JRC Coal Regions; RCI 2.0 |
