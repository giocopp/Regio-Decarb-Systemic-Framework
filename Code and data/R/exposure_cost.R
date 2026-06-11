# exposure_cost.R — carbon-cost-at-risk Exposure and the headline TRI.
#
#   Exposure_raw = ets_emis_t * EUA * (1 - free_alloc_share)
#                + CBAM_emb_tCO2 * EUA * cbam_cov
#   Exposure     = norm(Exposure_raw), POOLED across cells (within-sector
#                  min-max would cancel any sector-level price exactly)
#
# Specification and rationale: METHODOLOGY.md §9.1, §10.1.

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

# Cost-TRI vulnerability dimensions. Supply_Chain (Import_ExtraEU) is
# intentionally absent: imports are already priced in the CBAM leg.
.cost_vuln_dims <- function() list(
  Energy          = c("Energy_Consumption", "Fossil_Share",
                      "Renewables_Share", "RE_Potential"),
  Labour          = c("Unemployment_Rate", "Labour_Market_Slack",
                      "Highly_Skilled_Workers"),
  Technology      = c("BERD", "Regional_Innovation"),
  Institutions    = c("QoG_Index", "Climate_Mitigation_Laws"),
  Diversification = c("Sector_Concentration")
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

#' EUTL country x sector free-allocation share, capped at 1
#' (over-allocation => zero effective ETS cost, not negative exposure).
read_ets_freealloc <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    dplyr::transmute(Country_ID, Sector_ID,
                     free_alloc_share = pmin(free_alloc_share, 1)) |>
    tibble::as_tibble()
}


# ── CBAM leg ─────────────────────────────────────────────────────────────────

#' Hybrid CBAM downscaling weights: geocoded plant emission shares for the
#' four ETS sectors, employment shares otherwise. Heavy-sector weights span
#' the full country grid with explicit 0 for plant-less regions — otherwise
#' the downscaler's Sector-C fallback refills them and inflates the total.
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

#' CBAM-leg quantity: embodied (direct) carbon in extra-EU imports of CBAM-
#' covered goods, downscaled to NUTS-2 x sector. See prototypes/cbam_leg_prototype.R
#' for caveats (FIGARO 2-digit > exact CBAM goods; direct intensity only).
#'
#' @param io  FIGARO io tibble (ind_use, ind_ava, c_dest, c_orig, values), MEUR
#' @param ghg FIGARO ghg tibble (c_orig, c_dest, nace_r2, values), kt CO2eq
#' @param empl_weights tibble (Country_ID, NUTS_ID, Sector_ID, weight)
#' @param covered_goods FIGARO origin industries treated as CBAM-covered
#' @return tibble NUTS_ID, Country_ID, Sector_ID, CBAM_emb_tCO2 (11 manufacturing
#'   sub-sectors only; the "C" total is rolled up by the caller, not here)
compute_cbam_leg <- function(io, ghg, empl_weights,
                             covered_goods = c("C20","C23","C24"),
                             eu27 = .eu27_codes()) {
  nonEU    <- setdiff(unique(as.character(io$c_orig)), c(eu27, "DOM"))
  nace_map <- .figaro_nace_map()

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

#' Normalise a raw carbon cost to [0, 1] under the chosen scheme.
#' "log" = range01(log1p(.)), "minmax" = range01(.), "rank" = percentile rank.
.norm_exposure <- function(raw, norm) {
  switch(norm,
         log    = range01(log1p(pmax(raw, 0))),
         minmax = range01(pmax(raw, 0)),
         rank   = prank(raw),
         stop("unknown norm: ", norm))
}

#' Assemble pooled, multiplicative carbon-cost-at-risk Exposure.
#'
#' @param df tibble with NUTS_ID, Country_ID, Sector_ID, ets_emis_t
#'   (EUTL verified emissions, tonnes), free_alloc_share, CBAM_emb_tCO2, cbam_cov
#' @param eua_price scalar EUA EUR/tCO2 (a common scalar -> does not affect
#'   the ranking, only the EUR interpretation of the cost columns)
#' @param norm "log" (headline), "minmax", or "rank"; `log_scale` is the
#'   legacy switch kept for the prototype scripts (TRUE -> "log").
#' @param within_sector diagnostic only: if TRUE normalize within Sector_ID
#'   (which makes any sector-level price wash out — for comparison)
#' @return df plus P_ETS, P_CBAM, ETS_cost, CBAM_cost, Exposure_raw, Exposure
assemble_exposure_cost <- function(df, eua_price = 1, log_scale = TRUE,
                                   within_sector = FALSE,
                                   norm = if (log_scale) "log" else "minmax") {
  norm <- match.arg(norm, c("log", "minmax", "rank"))
  d <- df |>
    dplyr::mutate(
      P_ETS        = eua_price * (1 - free_alloc_share),
      P_CBAM       = eua_price * cbam_cov,
      ETS_cost     = ets_emis_t * P_ETS,
      CBAM_cost    = CBAM_emb_tCO2 * P_CBAM,
      Exposure_raw = ETS_cost + CBAM_cost
    )
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

#' Pooled 5-dimension Vulnerability for the carbon-cost TRI ("rank" ranks
#' the top level; otherwise min-max — log applies to the raw cost only).
build_vulnerability_pooled <- function(data_reshaped, empl_weights,
                                       norm = "log") {
  dims <- .cost_vuln_dims()
  vw <- normalize_indicators(dplyr::filter(data_reshaped, Sector_ID != "C"),
                             empl_weights, pool = TRUE)$wide
  for (nm in names(dims)) {
    vars <- intersect(dims[[nm]], names(vw))
    if (length(vars) == 0) next
    for (vv in vars) vw <- impute_with_median(vw, vv)
    vw[[paste0("Vuln_", nm)]] <-
      rowMeans(dplyr::select(vw, dplyr::all_of(vars)), na.rm = TRUE)
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


# ── TRI builder ──────────────────────────────────────────────────────────────

#' Join the cost inputs onto the vulnerability grid for one policy state.
#' @param fa_mult multiplier on the observed free-allocation share
#'   (1 = today's allocation, 0 = full phase-in)
#' @param cbam_factor CBAM coverage factor in [0, 1] (Reg. 2023/956 ramp)
assemble_cost_panel <- function(vuln, ets_geo, ets_freealloc, cbam_leg,
                                fa_mult = 0, cbam_factor = 1) {
  fa <- ets_freealloc |>
    dplyr::transmute(Country_ID, Sector_ID,
                     free_alloc_share = free_alloc_share * fa_mult)
  vuln |>
    dplyr::select(dplyr::any_of(c("NUTS_ID", "NUTS_Name", "Country_ID",
                                  "Sector_ID", "Sector_Name", "Vulnerability"))) |>
    dplyr::left_join(ets_geo, by = c("NUTS_ID", "Country_ID", "Sector_ID")) |>
    dplyr::left_join(cbam_leg |>
                       dplyr::select(NUTS_ID, Sector_ID, CBAM_emb_tCO2),
                     by = c("NUTS_ID", "Sector_ID")) |>
    dplyr::left_join(fa, by = c("Country_ID", "Sector_ID")) |>
    dplyr::mutate(
      ets_emis_t       = dplyr::coalesce(ets_emis_t, 0),
      CBAM_emb_tCO2    = dplyr::coalesce(CBAM_emb_tCO2, 0),
      free_alloc_share = dplyr::coalesce(free_alloc_share, 1),
      cbam_cov         = cbam_factor
    )
}

#' Carbon-cost TRI for the 11 sub-sectors at one policy state.
#' Risk_norm = range01(sqrt(E) * sqrt(V)), pooled; zero-cost cells get
#' Exposure 0 -> Risk NA -> "Zero Risk" band downstream.
build_cost_tri <- function(vuln, ets_geo, ets_freealloc, cbam_leg,
                           eua_price = 1, fa_mult = 0, cbam_factor = 1,
                           norm = "log") {
  assemble_cost_panel(vuln, ets_geo, ets_freealloc, cbam_leg,
                      fa_mult = fa_mult, cbam_factor = cbam_factor) |>
    assemble_exposure_cost(eua_price = eua_price, norm = norm) |>
    dplyr::mutate(
      E         = dplyr::if_else(Exposure == 0, NA_real_, Exposure),
      Risk_norm = range01(sqrt(E) * sqrt(Vulnerability))
    ) |>
    dplyr::select(-E)
}

#' "C" roll-up: per-region sums of the raw cost legs, normalised across
#' regions; Vulnerability = regional mean of sub-sector scores.
rollup_cost_C <- function(tri_subsector, vuln, norm = "log") {
  vtop <- if (identical(norm, "rank")) prank
          else function(x) range01(x, preserve_zeros = FALSE)

  Craw <- tri_subsector |>
    dplyr::group_by(NUTS_ID, NUTS_Name, Country_ID) |>
    dplyr::summarise(dplyr::across(c(ets_emis_t, CBAM_emb_tCO2,
                                     ETS_cost, CBAM_cost, Exposure_raw),
                                   \(x) sum(x, na.rm = TRUE)),
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

#' Full headline panel (-> Final data/Risk_data_carbon_cost.{xlsx,csv}):
#' 11 sub-sectors + C roll-up, reference scope columns, quintile bands.
build_risk_data_cost <- function(vuln, ets_geo, ets_freealloc, cbam_leg,
                                 data_reshaped, eua_price = 64.8,
                                 norm = "log") {
  tri <- build_cost_tri(vuln, ets_geo, ets_freealloc, cbam_leg,
                        eua_price = eua_price, fa_mult = 0, cbam_factor = 1,
                        norm = norm)
  C_tri <- rollup_cost_C(tri, vuln, norm = norm)

  # Scope 1/2/3 carried for analysis only — not priced in the index
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
                  ets_emis_t, CBAM_emb_tCO2, free_alloc_share,
                  ETS_cost_EUR = ETS_cost, CBAM_cost_EUR = CBAM_cost,
                  Carbon_cost_EUR = Exposure_raw,
                  Exposure, Vulnerability, Risk_norm, Risk_Band) |>
    tibble::as_tibble()
}


# ── Hazard trajectory (2024-2034 phase-in) ───────────────────────────────────

#' CBAM phase-in factor F(t) per Reg. (EU) 2023/956 Art. 31 + revised ETS
#' Directive Art. 10a: free allocation = base*(1-F), CBAM coverage = F.
#' 2023-2025 transitional (F = 0, reporting only).
cbam_phase_factor <- function(year) {
  f <- c(`2026` = 0.025, `2027` = 0.05, `2028` = 0.10, `2029` = 0.225,
         `2030` = 0.485, `2031` = 0.61, `2032` = 0.735, `2033` = 0.86,
         `2034` = 1.0)
  if (year <= 2025) return(0)
  if (year >= 2034) return(1)
  as.numeric(f[as.character(year)])
}

#' Cost trajectory along the legislated phase-in path: total cost (EUR bn at
#' the given EUA price), ETS share, number of priced cells, and Spearman of
#' each year's ranking vs the 2034 headline.
build_cost_trajectory <- function(vuln, ets_geo, ets_freealloc, cbam_leg,
                                  eua_price = 64.8,
                                  years = c(2024, 2026, 2028, 2030, 2032, 2034),
                                  norm = "log") {
  headline <- build_cost_tri(vuln, ets_geo, ets_freealloc, cbam_leg,
                             eua_price = eua_price, fa_mult = 0,
                             cbam_factor = 1, norm = norm)

  purrr::map_dfr(years, function(y) {
    Fy <- cbam_phase_factor(y)
    rk <- build_cost_tri(vuln, ets_geo, ets_freealloc, cbam_leg,
                         eua_price = eua_price, fa_mult = 1 - Fy,
                         cbam_factor = Fy, norm = norm)
    jj <- headline |>
      dplyr::select(NUTS_ID, Sector_ID, rH = Risk_norm) |>
      dplyr::inner_join(rk |> dplyr::select(NUTS_ID, Sector_ID, r = Risk_norm),
                        by = c("NUTS_ID", "Sector_ID")) |>
      dplyr::filter(!is.na(rH) & !is.na(r))
    tibble::tibble(
      year        = y,
      free_alloc  = round(1 - Fy, 3),
      CBAM_factor = Fy,
      cost_EURbn  = round(sum(rk$Exposure_raw, na.rm = TRUE) / 1e9, 1),
      ETS_share   = round(sum(rk$ETS_cost, na.rm = TRUE) /
                          sum(rk$ETS_cost + rk$CBAM_cost, na.rm = TRUE), 2),
      n_priced    = sum(rk$Exposure > 0, na.rm = TRUE),
      rho_vs_2034 = round(cor(jj$r, jj$rH, method = "spearman"), 3)
    )
  })
}


# ── Sensitivity rows for the carbon-cost TRI ─────────────────────────────────

#' Cost-TRI sensitivity: headline vs emissions baseline, normalisation
#' variants, and the 2024 policy state. Uses .sens_row() from
#' 07_sensitivity.R (pooled + mean within-sector Spearman).
run_sensitivity_cost <- function(risk_data_cost, vuln, ets_geo,
                                 ets_freealloc, cbam_leg, risk_data,
                                 eua_price = 64.8, norm = "log") {

  sub <- risk_data_cost |> dplyr::filter(Sector_ID != "C")

  # 1. vs the emissions-based baseline TRI (legacy index)
  cmp_base <- sub |>
    dplyr::select(NUTS_ID, Sector_ID, Risk_cost = Risk_norm) |>
    dplyr::inner_join(risk_data |>
                        dplyr::select(NUTS_ID, Sector_ID,
                                      Risk_base = Risk_norm),
                      by = c("NUTS_ID", "Sector_ID"))
  row_base <- .sens_row("Cost TRI vs emissions-baseline TRI",
                        cmp_base, "Risk_cost", "Risk_base")

  # 2. normalisation variants of the headline
  norm_rows <- purrr::map_dfr(setdiff(c("log", "minmax", "rank"), norm),
                              function(alt) {
    tri_alt <- build_cost_tri(vuln, ets_geo, ets_freealloc, cbam_leg,
                              eua_price = eua_price, fa_mult = 0,
                              cbam_factor = 1, norm = alt)
    d <- sub |>
      dplyr::select(NUTS_ID, Sector_ID, Risk_cost = Risk_norm) |>
      dplyr::inner_join(tri_alt |>
                          dplyr::select(NUTS_ID, Sector_ID,
                                        Risk_alt = Risk_norm),
                        by = c("NUTS_ID", "Sector_ID"))
    .sens_row(paste0("Cost TRI norm: ", norm, " vs ", alt),
              d, "Risk_cost", "Risk_alt")
  })

  # 3. headline (2034 full phase-in) vs today's policy state (2024)
  tri_2024 <- build_cost_tri(vuln, ets_geo, ets_freealloc, cbam_leg,
                             eua_price = eua_price, fa_mult = 1,
                             cbam_factor = 0, norm = norm)
  d24 <- sub |>
    dplyr::select(NUTS_ID, Sector_ID, Risk_cost = Risk_norm) |>
    dplyr::inner_join(tri_2024 |>
                        dplyr::select(NUTS_ID, Sector_ID,
                                      Risk_2024 = Risk_norm),
                      by = c("NUTS_ID", "Sector_ID"))
  row_2024 <- .sens_row("Cost TRI: 2034 headline vs 2024 policy state",
                        d24, "Risk_cost", "Risk_2024")

  dplyr::bind_rows(row_base, norm_rows, row_2024)
}
