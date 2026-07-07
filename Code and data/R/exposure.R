# exposure.R — covered-carbon Exposure and the headline Risk index (TRI).
#
#   Exposure_raw = ets_emis_t + CBAM_emb_tCO2          # covered carbon, tCO2
#   Exposure     = norm(Exposure_raw), POOLED across all cells.
#
# Exposure is a per-cell covered-carbon volume (tonnes CO2): EUTL verified ETS
# emissions + embodied carbon in extra-EU imports of CBAM-covered goods. No
# carbon price is applied: a single EU-wide EUA price is a common scalar that
# cancels under any monotone normalisation, so it never moved the ranking. The
# earlier priced formulation (EUA x free-allocation/CBAM coverage) and its
# 2024-2034 phase-in trajectory were removed (see git history). norm = "minmax"
# (tri_norm_mode); true-zero cells keep Exposure = 0; pooled normalisation
# throughout.

.eu27_codes <- function()
  c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
    "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")

# FIGARO industry code -> 12 manufacturing aggregates (using-sector).
.figaro_nace_map <- function() tibble::tribble(
  ~ind,     ~Sector_ID,
  "C10-12","C10-C12","C13-15","C13-C15",
  "C16","C16-C18","C17","C16-C18","C18","C16-C18",
  "C19","C19-C20","C20","C19-C20",
  "C21","C21-C22","C22","C21-C22",
  "C23","C23","C24","C24",
  "C25","C25+C28","C28","C25+C28",
  "C29","C29-C30","C30","C29-C30",
  "C26","C26-C27","C27","C26-C27",
  "C31_32","C31-C33","C33","C31-C33")

# Headline-TRI vulnerability dimensions (4). Supply_Chain (Import_ExtraEU) is
# intentionally absent (imports already enter the CBAM component of Exposure).
# Diversification (= Sector_Concentration alone) was dropped 2026-07-03
# (decision with supervisor): under POOLED normalisation the indicator encodes
# which sectors are large everywhere (food ~0.27, machinery ~0.29 vs chemicals
# ~0.07 sector means), i.e. sector size, not regional specialisation — and it
# was a single-indicator pillar. Sector_Concentration remains computed and
# stays a dimension of the legacy within-sector baseline (05_aggregate.R).
.vuln_dims <- function() list(
  Energy       = c("Energy_Consumption", "Fossil_Share",
                   "Renewables_Share", "RE_Potential"),
  Labour       = c("Unemployment_Rate", "Labour_Market_Slack",
                   "Highly_Skilled_Workers"),
  Technology   = c("BERD", "Regional_Innovation"),
  Institutions = c("QoG_Index", "Climate_Mitigation_Laws")
)


# ── Input readers ────────────────────────────────────────────────────────────

#' Latest FIGARO IO + GHG cache pair written by create_scope3().
#' @return Named character vector c(io = path, ghg = path).
figaro_cache_files <- function(dir = "Initial data/Non sector data") {
  io_f  <- list.files(dir, pattern = "^FIGARO_naio_10_fcp_ii4_\\d{4}\\.rds$")
  ghg_f <- list.files(dir, pattern = "^FIGARO_env_ac_ghgfp_\\d{4}\\.rds$")
  yr <- function(f) as.integer(sub(".*_(\\d{4})\\.rds$", "\\1", f))
  years <- intersect(yr(io_f), yr(ghg_f))
  if (length(years) == 0) {
    stop("No FIGARO cache rds pair under '", dir,
         "'. Run the scope3 target first (create_scope3 writes the cache).",
         call. = FALSE)
  }
  y <- max(years)
  c(io  = file.path(dir, sprintf("FIGARO_naio_10_fcp_ii4_%d.rds", y)),
    ghg = file.path(dir, sprintf("FIGARO_env_ac_ghgfp_%d.rds", y)))
}

#' EUTL geocoded verified emissions, NUTS-2 x sector (tonnes, EUTL vintage).
read_ets_nuts2 <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, ets_emis_t) |>
    tibble::as_tibble()
}

# ── CBAM leg ─────────────────────────────────────────────────────────────────

#' CBAM downscaling weights. The HEADLINE uses employment shares for every
#' sector (`heavy_sectors = character(0)`): employment locates the importing
#' users of the covered inputs, matches the pipeline's canonical downscaling
#' rule (METHODOLOGY §5), and is the allocation that survives the external
#' trade validation (ρ = 0.91 vs observed regional imports and no material
#' physical-ceiling breach, vs ρ = 0.69 with breaches for the plant-emission
#' hybrid — METHODOLOGY §14). The default `heavy_sectors` builds that hybrid
#' (plant-emission shares for the four ETS sectors), retained ONLY as the
#' `cbam_leg_hybrid` sensitivity variant. Heavy-sector weights span the full
#' country grid with explicit 0 for plant-less regions — otherwise the
#' downscaler's Sector-C fallback refills them and inflates the total.
build_cbam_weights <- function(empl_weights, ets_geo,
                               heavy_sectors = c("C16-C18", "C19-C20",
                                                 "C23", "C24")) {
  ew2 <- recombine_empl_nuts(empl_weights)
  grid <- ew2 |>
    dplyr::filter(Sector_ID == "C") |>
    dplyr::distinct(Country_ID, NUTS_ID)

  geo_share <- ets_geo |>
    dplyr::filter(Sector_ID %in% heavy_sectors) |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::mutate(weight = ets_emis_t / sum(ets_emis_t, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, weight)

  geo_keys <- dplyr::distinct(geo_share, Country_ID, Sector_ID)
  geo_w <- geo_keys |>
    dplyr::inner_join(grid, by = "Country_ID", relationship = "many-to-many") |>
    dplyr::left_join(geo_share, by = c("Country_ID", "NUTS_ID", "Sector_ID")) |>
    dplyr::mutate(weight = dplyr::coalesce(weight, 0)) |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::mutate(weight = weight / sum(weight)) |>
    dplyr::ungroup()

  emp_w <- ew2 |>
    dplyr::select(Country_ID, NUTS_ID, Sector_ID, weight) |>
    dplyr::anti_join(geo_keys, by = c("Country_ID", "Sector_ID"))

  dplyr::bind_rows(geo_w, emp_w)
}

#' The 64 FIGARO producing industries (same universe as create_scope3(),
#' R/01_create_data.R).
.figaro_real_industries <- function()
  c("A01","A02","A03","B","C10-12","C13-15","C16","C17","C18",
    "C19","C20","C21","C22","C23","C24","C25","C26","C27","C28",
    "C29","C30","C31_32","C33","D35","E36","E37-39","F","G45",
    "G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60",
    "J61","J62_63","K64","K65","K66","L","M69_70","M71","M72",
    "M73","M74_75","N77","N78","N79","N80-82","O84","P85","Q86",
    "Q87_88","R90-92","R93","S94","S95","S96","T","U")

#' Total cradle-to-gate embodied emission intensity (tCO2 per MEUR of output),
#' the full footprint f^T (I - A)^-1 for every (origin, FIGARO industry) — direct
#' + ALL upstream tiers, incl. electricity. Same MRIO machinery as
#' create_scope3() (R/01_create_data.R); here the total footprint (not
#' upstream-only). Used by compute_cbam_leg(intensity = "embodied"). See
#' Review/EXPOSURE_CARBON_COST_REVISION.md §23.
.figaro_embodied_intensity <- function(io, ghg) {
  real_ind <- .figaro_real_industries()
  io  <- dplyr::mutate(io,  dplyr::across(c(c_orig, c_dest, ind_ava, ind_use),
                                          as.character))
  ghg <- dplyr::mutate(ghg, dplyr::across(c(c_orig, nace_r2), as.character))
  regions <- sort(setdiff(unique(io$c_orig), "DOM"))
  N   <- length(regions) * length(real_ind)
  idx <- expand.grid(region = regions, ind = real_ind, stringsAsFactors = FALSE)
  ky  <- function(r, i) paste(r, i, sep = "|")
  k_of <- stats::setNames(seq_len(N), ky(idx$region, idx$ind))
  fd_use <- c("P3_S13","P3_S14","P3_S15","P5M","P51G")

  zr <- io |>
    dplyr::filter(c_orig != "DOM", c_orig %in% regions, c_dest %in% regions,
                  ind_ava %in% real_ind, ind_use %in% real_ind,
                  !is.na(values), values > 0)
  Z <- Matrix::sparseMatrix(i = k_of[ky(zr$c_orig, zr$ind_ava)],
                            j = k_of[ky(zr$c_dest, zr$ind_use)],
                            x = zr$values, dims = c(N, N))
  fdr <- io |>
    dplyr::filter(c_orig != "DOM", c_orig %in% regions, c_dest %in% regions,
                  ind_ava %in% real_ind, ind_use %in% fd_use, !is.na(values)) |>
    dplyr::group_by(c_orig, ind_ava) |>
    dplyr::summarise(fd = sum(values, na.rm = TRUE), .groups = "drop")
  x  <- as.numeric(Matrix::rowSums(Z))
  fk <- k_of[ky(fdr$c_orig, fdr$ind_ava)]
  x[fk] <- x[fk] + fdr$fd
  x_inv <- ifelse(x > 0, 1 / x, 0)
  A <- as.matrix(Z %*% Matrix::Diagonal(N, x_inv))
  L <- solve(diag(N) - A)

  e <- ghg |>
    dplyr::group_by(c_orig, nace_r2) |>
    dplyr::summarise(emis_kt = sum(values, na.rm = TRUE), .groups = "drop")
  f  <- numeric(N)
  ke <- k_of[ky(e$c_orig, e$nace_r2)]
  ok <- !is.na(ke)
  f[ke[ok]] <- e$emis_kt[ok] * 1000
  f <- ifelse(x > 0, f / x, 0)                   # direct tCO2 / MEUR
  e_total <- as.numeric(crossprod(f, L))          # f^T L : full footprint / MEUR

  tibble::tibble(c_orig = idx$region, ind_ava = idx$ind, f = e_total)
}

#' CBAM-leg quantity: embodied carbon in extra-EU imports of CBAM-covered goods,
#' downscaled to NUTS-2 x sector. `intensity = "direct"` (headline) uses the
#' origin's Scope-1 intensity; `intensity = "embodied"` uses the full
#' cradle-to-gate footprint (f^T (I-A)^-1) as the robustness variant motivated in
#' Review/EXPOSURE_CARBON_COST_REVISION.md §23 and
#' Review/EXPOSURE_CARBON_COST_LITERATURE.md §5b (Tanaka 2025; Su 2022).
#' Caveats: FIGARO 2-digit industries are broader than the exact CBAM goods, and
#' all embodied import carbon is counted in full (no deduction for carbon
#' already priced in the origin country).
#'
#' @param io  FIGARO io tibble (ind_use, ind_ava, c_dest, c_orig, values), MEUR
#' @param ghg FIGARO ghg tibble (c_orig, c_dest, nace_r2, values), kt CO2eq
#' @param empl_weights tibble (Country_ID, NUTS_ID, Sector_ID, weight)
#' @param covered_goods FIGARO origin industries treated as CBAM-covered
#' @param intensity "direct" (Scope-1, headline) or "embodied" (full footprint)
#' @return tibble NUTS_ID, Country_ID, Sector_ID, CBAM_emb_tCO2 (11 manufacturing
#'   sub-sectors only; the "C" total is rolled up by the caller, not here)
compute_cbam_leg <- function(io, ghg, empl_weights,
                             covered_goods = c("C20","C23","C24"),
                             eu27 = .eu27_codes(),
                             intensity = c("direct", "embodied")) {
  intensity <- match.arg(intensity)
  io  <- dplyr::mutate(io,  dplyr::across(c(c_orig, c_dest, ind_ava, ind_use),
                                          as.character))
  ghg <- dplyr::mutate(ghg, dplyr::across(c(c_orig, nace_r2), as.character))
  nonEU    <- setdiff(unique(io$c_orig), c(eu27, "DOM"))
  nace_map <- .figaro_nace_map()

  if (intensity == "embodied") {
    f_tab <- .figaro_embodied_intensity(io, ghg) |>
      dplyr::filter(c_orig %in% nonEU, ind_ava %in% covered_goods)
  } else {
    output <- io |>
      dplyr::group_by(c_orig, ind_ava) |>
      dplyr::summarise(output = sum(values, na.rm = TRUE), .groups = "drop")
    emis <- ghg |>
      dplyr::group_by(c_orig, nace_r2) |>
      dplyr::summarise(emis_kt = sum(values, na.rm = TRUE), .groups = "drop")
    f_tab <- output |>
      dplyr::filter(c_orig %in% nonEU, ind_ava %in% covered_goods) |>
      dplyr::left_join(emis, by = c("c_orig", "ind_ava" = "nace_r2")) |>
      dplyr::mutate(f = dplyr::if_else(output > 0, (emis_kt * 1000) / output, 0)) |>
      dplyr::select(c_orig, ind_ava, f)
  }

  imp <- io |>
    dplyr::filter(c_orig %in% nonEU, ind_ava %in% covered_goods,
                  c_dest %in% eu27, ind_use %in% nace_map$ind, values > 0) |>
    dplyr::left_join(f_tab, by = c("c_orig", "ind_ava")) |>
    dplyr::mutate(emb_tCO2 = values * f)

  cbam <- imp |>
    dplyr::left_join(nace_map, by = c("ind_use" = "ind")) |>
    dplyr::group_by(Country_ID = c_dest, Sector_ID) |>
    dplyr::summarise(CBAM_emb_tCO2 = sum(emb_tCO2, na.rm = TRUE), .groups = "drop")

  downscale_national_to_nuts2(cbam, empl_weights, "CBAM_emb_tCO2") |>
    tibble::as_tibble()
}


# ── Exposure engine ──────────────────────────────────────────────────────────

#' Normalise the raw covered-carbon volume to [0, 1] under the chosen scheme.
#' "log" = range01(log1p(.)), "minmax" = range01(.), "rank" = percentile rank.
.norm_exposure <- function(raw, norm) {
  switch(norm,
         log    = range01(log1p(pmax(raw, 0))),
         minmax = range01(pmax(raw, 0)),
         rank   = prank(raw),
         stop("unknown norm: ", norm))
}

#' Assemble pooled Exposure = normalised covered carbon volume.
#'
#' Exposure_raw is the cell's covered carbon volume in tonnes CO2: EUTL verified
#' ETS emissions + embodied carbon in extra-EU imports of CBAM-covered goods. No
#' carbon price is applied (a single EU-wide EUA price is a common scalar that
#' cancels under normalisation — see file header).
#'
#' @param df tibble with NUTS_ID, Country_ID, Sector_ID, ets_emis_t
#'   (EUTL verified emissions, tonnes) and CBAM_emb_tCO2 (tonnes)
#' @param norm "minmax" (the wired headline, tri_norm_mode), "log", or "rank"
#' @param within_sector diagnostic only: if TRUE normalize within Sector_ID
#' @return df plus Exposure_raw (covered tCO2) and Exposure
assemble_exposure <- function(df, within_sector = FALSE, norm = "minmax") {
  norm <- match.arg(norm, c("log", "minmax", "rank"))
  d <- df |>
    dplyr::mutate(Exposure_raw = ets_emis_t + CBAM_emb_tCO2)
  if (within_sector) {
    d <- d |>
      dplyr::group_by(Sector_ID) |>
      dplyr::mutate(Exposure = .norm_exposure(Exposure_raw, norm)) |>
      dplyr::ungroup()
  } else {
    d <- d |> dplyr::mutate(Exposure = .norm_exposure(Exposure_raw, norm))
  }
  # true-zero cells stay at 0 under every norm (rank would price them)
  d |>
    dplyr::mutate(Exposure = dplyr::if_else(Exposure_raw == 0, 0, Exposure)) |>
    tibble::as_tibble()
}


# ── Pooled Vulnerability (5 dimensions) ──────────────────────────────────────

#' Pooled 4-dimension Vulnerability for the headline TRI ("rank" ranks
#' the top level; otherwise min-max — log applies to the raw exposure only).
#' Each dimension is re-normalised to [0,1] before the final average.
build_vulnerability_pooled <- function(data_reshaped, empl_weights,
                                       norm = "log") {
  dims <- .vuln_dims()
  vw <- normalize_indicators(dplyr::filter(data_reshaped, Sector_ID != "C"),
                             empl_weights, pool = TRUE)$wide
  for (nm in names(dims)) {
    vars <- intersect(dims[[nm]], names(vw))
    if (length(vars) == 0) next
    for (vv in vars) vw <- impute_with_median(vw, vv)
    vw[[paste0("Vuln_", nm)]] <-
      rowMeans(dplyr::select(vw, dplyr::all_of(vars)), na.rm = TRUE)
  }
  # Re-normalise each dimension before averaging — the pooled analogue of the
  # legacy per-dimension re-normalisation (§9). Without this step dimensions
  # entered the mean with raw variances and effective influence was very
  # unequal (2026-07-03 audit: correlation with the composite 0.81 for
  # Technology vs 0.11 for Energy). preserve_zeros = FALSE: bounded scores.
  for (nm in names(dims)) {
    col <- paste0("Vuln_", nm)
    if (col %in% names(vw))
      vw[[col]] <- range01(vw[[col]], preserve_zeros = FALSE)
  }
  vtop <- if (identical(norm, "rank")) prank
          else function(x) range01(x, preserve_zeros = FALSE)
  vw |>
    dplyr::mutate(
      Vulnerability = vtop(rowMeans(dplyr::across(dplyr::starts_with("Vuln_")),
                                    na.rm = TRUE))
    ) |>
    tibble::as_tibble()
}


# ── Risk-index builder ───────────────────────────────────────────────────────

#' Join the carbon-volume inputs (geocoded ETS + CBAM-embodied) onto the
#' vulnerability grid. Missing legs are true zeros. Carries the 5 Vuln_*
#' dimension scores and pers_employed through for the decomposition figures.
assemble_risk_panel <- function(vuln, ets_geo, cbam_leg) {
  vuln |>
    dplyr::select(dplyr::any_of(c("NUTS_ID", "NUTS_Name", "Country_ID",
                                  "Sector_ID", "Sector_Name",
                                  "pers_employed", "Vulnerability")),
                  dplyr::starts_with("Vuln_")) |>
    dplyr::left_join(ets_geo, by = c("NUTS_ID", "Country_ID", "Sector_ID")) |>
    dplyr::left_join(cbam_leg |>
                       dplyr::select(NUTS_ID, Sector_ID, CBAM_emb_tCO2),
                     by = c("NUTS_ID", "Sector_ID")) |>
    dplyr::mutate(
      ets_emis_t    = dplyr::coalesce(ets_emis_t, 0),
      CBAM_emb_tCO2 = dplyr::coalesce(CBAM_emb_tCO2, 0)
    )
}

#' Covered-carbon Risk index for the 11 sub-sectors.
#' Risk_norm = range01(sqrt(E) * sqrt(V)), pooled; zero-volume cells get
#' Exposure 0 -> Risk NA -> "Zero Risk" band downstream.
build_risk_tri <- function(vuln, ets_geo, cbam_leg, norm = "minmax") {
  assemble_risk_panel(vuln, ets_geo, cbam_leg) |>
    assemble_exposure(norm = norm) |>
    dplyr::mutate(
      E         = dplyr::if_else(Exposure == 0, NA_real_, Exposure),
      Risk_norm = range01(sqrt(E) * sqrt(Vulnerability))
    ) |>
    dplyr::select(-E)
}

#' "C" roll-up: per-region sums of the carbon-volume legs (and pers_employed),
#' normalised across regions; Vulnerability = regional mean of sub-sector
#' scores, re-normalised; Vuln_* dimensions = regional means (kept on the
#' bounded [0.01, 0.99] indicator scale).
rollup_risk_C <- function(tri_subsector, vuln, norm = "minmax") {
  vtop <- if (identical(norm, "rank")) prank
          else function(x) range01(x, preserve_zeros = FALSE)

  Craw <- tri_subsector |>
    dplyr::group_by(NUTS_ID, NUTS_Name, Country_ID) |>
    dplyr::summarise(
      dplyr::across(c(ets_emis_t, CBAM_emb_tCO2, Exposure_raw, pers_employed),
                    \(x) sum(x, na.rm = TRUE)),
      dplyr::across(dplyr::starts_with("Vuln_"), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop")

  Cv <- vuln |>
    dplyr::group_by(NUTS_ID, Country_ID) |>
    dplyr::summarise(Vulnerability = mean(Vulnerability, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(Vulnerability = vtop(Vulnerability))

  Craw |>
    dplyr::mutate(Sector_ID = "C",
                  Exposure  = .norm_exposure(Exposure_raw, norm),
                  Exposure  = dplyr::if_else(Exposure_raw == 0, 0, Exposure)) |>
    dplyr::inner_join(Cv, by = c("NUTS_ID", "Country_ID")) |>
    dplyr::mutate(
      E         = dplyr::if_else(Exposure == 0, NA_real_, Exposure),
      Risk_norm = range01(sqrt(E) * sqrt(Vulnerability))
    ) |>
    dplyr::select(-E)
}

#' Full headline panel (-> Final data/Risk_data.{xlsx,csv}):
#' 11 sub-sectors + C roll-up, Scope 1/2/3 reference columns, the 5 Vuln_*
#' dimension scores and pers_employed (for the decomposition figures),
#' quintile bands. Exposure legs are published as exposure_ETS /
#' exposure_CBAM / exposure_total (tonnes CO2).
build_risk_data <- function(vuln, ets_geo, cbam_leg,
                            data_reshaped, norm = "minmax") {
  tri <- build_risk_tri(vuln, ets_geo, cbam_leg, norm = norm)
  C_tri <- rollup_risk_C(tri, vuln, norm = norm)

  # Scope 1/2/3 carried for analysis only — not part of the index
  scopes_raw <- data_reshaped |>
    dplyr::filter(Indicator %in% c("Scope1_Emissions", "Scope2_Emissions",
                                   "Scope3_Emissions")) |>
    dplyr::group_by(NUTS_ID, Sector_ID, Indicator) |>
    dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = Indicator, values_from = Value)

  band_labels <- c("Very Low", "Low", "Medium", "High", "Very High")

  dplyr::bind_rows(tri, C_tri) |>
    dplyr::mutate(Sector_Name = sector_name_map[Sector_ID]) |>
    dplyr::left_join(scopes_raw, by = c("NUTS_ID", "Sector_ID")) |>
    dplyr::mutate(
      Risk_Band = dplyr::if_else(
        is.na(Risk_norm), "Zero Risk",
        as.character(cut(Risk_norm, breaks = seq(0, 1, by = 0.20),
                         include.lowest = TRUE, labels = band_labels))
      )
    ) |>
    dplyr::select(NUTS_ID, NUTS_Name, Country_ID, Sector_ID, Sector_Name,
                  Scope1_Emissions, Scope2_Emissions, Scope3_Emissions,
                  exposure_ETS = ets_emis_t, exposure_CBAM = CBAM_emb_tCO2,
                  exposure_total = Exposure_raw,
                  Exposure, Vulnerability, dplyr::starts_with("Vuln_"),
                  Risk_norm, Risk_Band, pers_employed) |>
    tibble::as_tibble()
}


# ── Sensitivity rows for the headline Risk index ─────────────────────────────

#' Headline-TRI sensitivity: headline vs raw-emissions baseline, normalisation
#' variants, and CBAM intensity. Uses .sens_row() from 07_sensitivity.R
#' (pooled + mean within-sector Spearman).
run_risk_sensitivity <- function(risk_data, vuln, ets_geo,
                                 cbam_leg, risk_data_raw_emissions,
                                 norm = "minmax", cbam_leg_embodied = NULL,
                                 cbam_leg_hybrid = NULL) {

  sub <- risk_data |> dplyr::filter(Sector_ID != "C")

  # 1. vs the raw-emissions baseline TRI (legacy index)
  cmp_base <- sub |>
    dplyr::select(NUTS_ID, Sector_ID, Risk_headline = Risk_norm) |>
    dplyr::inner_join(risk_data_raw_emissions |>
                        dplyr::select(NUTS_ID, Sector_ID,
                                      Risk_base = Risk_norm),
                      by = c("NUTS_ID", "Sector_ID"))
  row_base <- .sens_row("Headline TRI vs raw-emissions TRI",
                        cmp_base, "Risk_headline", "Risk_base")

  # 2. normalisation variants of the headline
  norm_rows <- purrr::map_dfr(setdiff(c("log", "minmax", "rank"), norm),
                              function(alt) {
    tri_alt <- build_risk_tri(vuln, ets_geo, cbam_leg, norm = alt)
    d <- sub |>
      dplyr::select(NUTS_ID, Sector_ID, Risk_headline = Risk_norm) |>
      dplyr::inner_join(tri_alt |>
                          dplyr::select(NUTS_ID, Sector_ID,
                                        Risk_alt = Risk_norm),
                        by = c("NUTS_ID", "Sector_ID"))
    .sens_row(paste0("Headline TRI norm: ", norm, " vs ", alt),
              d, "Risk_headline", "Risk_alt")
  })

  # 3. CBAM intensity: headline direct vs full-embodied footprint
  #    (Review/EXPOSURE_CARBON_COST_REVISION.md §23)
  row_emb <- NULL
  if (!is.null(cbam_leg_embodied)) {
    tri_emb <- build_risk_tri(vuln, ets_geo, cbam_leg_embodied, norm = norm)
    d_emb <- sub |>
      dplyr::select(NUTS_ID, Sector_ID, Risk_headline = Risk_norm) |>
      dplyr::inner_join(tri_emb |>
                          dplyr::select(NUTS_ID, Sector_ID, Risk_emb = Risk_norm),
                        by = c("NUTS_ID", "Sector_ID"))
    row_emb <- .sens_row("Headline TRI CBAM intensity: direct vs embodied",
                         d_emb, "Risk_headline", "Risk_emb")
  }

  # 4. CBAM allocation: employment-only weights (headline, adopted after the
  #    external trade validation — METHODOLOGY §14) vs the plant-emission
  #    hybrid retained as the sensitivity variant
  row_alloc <- NULL
  if (!is.null(cbam_leg_hybrid)) {
    tri_alloc <- build_risk_tri(vuln, ets_geo, cbam_leg_hybrid, norm = norm)
    d_alloc <- sub |>
      dplyr::select(NUTS_ID, Sector_ID, Risk_headline = Risk_norm) |>
      dplyr::inner_join(tri_alloc |>
                          dplyr::select(NUTS_ID, Sector_ID,
                                        Risk_alloc = Risk_norm),
                        by = c("NUTS_ID", "Sector_ID"))
    row_alloc <- .sens_row("Headline TRI CBAM allocation: employment-only vs plant-emission hybrid",
                           d_alloc, "Risk_headline", "Risk_alloc")
  }

  dplyr::bind_rows(row_base, norm_rows, row_emb, row_alloc)
}
