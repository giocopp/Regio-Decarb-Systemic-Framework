# Carbon-cost-at-risk Exposure engine (pooled). Depends on range01() from utils.R.
#   Exposure_raw = ets_emis_t * EUA*(1-free_alloc_share) + CBAM_emb_tCO2 * EUA*cbam_cov
#   Exposure = range01(Exposure_raw), or range01(log1p(.)) when log_scale=TRUE.

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
  # 11 sub-sectors only — no "C" aggregate here (the caller rolls it up, so we
  # avoid a double-counting total row in this function's output).

  downscale_national_to_nuts2(cbam, empl_weights, "CBAM_emb_tCO2") |>
    tibble::as_tibble()
}

#' Assemble pooled, multiplicative carbon-cost-at-risk Exposure.
#'
#' @param df tibble with NUTS_ID, Country_ID, Sector_ID, ets_emis_t
#'   (EEA ETS-covered verified emissions, tonnes), free_alloc_share,
#'   CBAM_emb_tCO2, cbam_cov
#' @param eua_price scalar EUA EUR/tCO2 (default 1; a common scalar -> does not
#'   affect the ranking, only the EUR interpretation)
#' @param within_sector diagnostic only: if TRUE normalize within Sector_ID
#'   (which makes the sector-level price wash out — for comparison)
#' @return df plus P_ETS, P_CBAM, ETS_cost, CBAM_cost, Exposure_raw, Exposure
assemble_exposure_cost <- function(df, eua_price = 1, log_scale = TRUE,
                                   within_sector = FALSE) {
  d <- df |>
    dplyr::mutate(
      P_ETS        = eua_price * (1 - free_alloc_share),
      P_CBAM       = eua_price * cbam_cov,
      ETS_cost     = ets_emis_t * P_ETS,
      CBAM_cost    = CBAM_emb_tCO2 * P_CBAM,
      Exposure_raw = ETS_cost + CBAM_cost,
      .scaled      = if (log_scale) log1p(pmax(Exposure_raw, 0)) else Exposure_raw
    )
  if (within_sector) {
    d <- d |> dplyr::group_by(Sector_ID) |>
      dplyr::mutate(Exposure = range01(.scaled)) |> dplyr::ungroup()
  } else {
    d <- d |> dplyr::mutate(Exposure = range01(.scaled))   # POOLED (Option A)
  }
  d |> dplyr::select(-.scaled) |> tibble::as_tibble()
}
