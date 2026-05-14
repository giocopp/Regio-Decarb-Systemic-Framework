# 07_sensitivity.R — TRI robustness checks. Each test perturbs one
# construction choice and reports Spearman rho vs the baseline ranking.


#' Geometric TRI: Risk = E^alpha * V^(1-alpha).
compute_tri <- function(e, v, a) e^a * v^(1 - a)


#' Run the sensitivity battery. With `edgar_scope1` supplied, appends the
#' EDGAR-based Scope 1 tests for C19-C20, C23, C24.
run_sensitivity <- function(risk_data, edgar_scope1 = NULL) {

  baseline <- risk_data |>
    dplyr::filter(!is.na(Risk_norm)) |>
    dplyr::select(NUTS_ID, Sector_ID, Risk_norm_baseline = Risk_norm,
                  Exposure, Vulnerability)

  vuln_cols <- grep("^Vuln_", names(risk_data), value = TRUE)
  message("Vulnerability dimensions: ", paste(vuln_cols, collapse = ", "))

  # ───────────────────────────────────────────────

  # A. ALPHA SENSITIVITY
  # ───────────────────────────────────────────────
  alpha_grid <- c(0.30, 0.40, 0.50, 0.60, 0.70)

  alpha_results <- purrr::map_dfr(alpha_grid, function(a) {
    d <- baseline |>
      dplyr::mutate(Risk_sa = compute_tri(Exposure, Vulnerability, a)) |>
      dplyr::group_by(Sector_ID) |>
      dplyr::mutate(Risk_sa_norm = range01(Risk_sa)) |>
      dplyr::ungroup()

    rho <- cor(d$Risk_norm_baseline, d$Risk_sa_norm,
               use = "pairwise.complete.obs", method = "spearman")

    tibble::tibble(test = paste0("Alpha = ", a),
                   spearman_rho = round(rho, 4))
  })

  # ───────────────────────────────────────────────
  # B. PCA VS EQUAL WEIGHTS
  # ───────────────────────────────────────────────
  vuln_data <- risk_data |>
    dplyr::filter(!is.na(Risk_norm)) |>
    dplyr::select(NUTS_ID, Sector_ID, Exposure, Risk_norm,
                  dplyr::all_of(vuln_cols))

  vuln_matrix <- vuln_data |>
    dplyr::select(dplyr::all_of(vuln_cols)) |>
    as.matrix()

  complete_rows <- stats::complete.cases(vuln_matrix)
  pca_fit <- stats::prcomp(vuln_matrix[complete_rows, ],
                           center = TRUE, scale. = TRUE)

  # Use absolute PC1 loadings as weights
  pca_weights <- abs(pca_fit$rotation[, 1])
  pca_weights <- pca_weights / sum(pca_weights)

  pca_result <- vuln_data |>
    dplyr::mutate(
      Vulnerability_PCA = as.numeric(
        as.matrix(dplyr::pick(dplyr::all_of(vuln_cols))) %*% pca_weights
      )
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Vulnerability_PCA = range01(Vulnerability_PCA)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Risk_PCA = compute_tri(Exposure, Vulnerability_PCA, 0.5)
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Risk_PCA_norm = range01(Risk_PCA)) |>
    dplyr::ungroup()

  rho_pca <- cor(pca_result$Risk_norm, pca_result$Risk_PCA_norm,
                 use = "pairwise.complete.obs", method = "spearman")

  pca_row <- tibble::tibble(test = "PCA weights",
                            spearman_rho = round(rho_pca, 4))

  # ───────────────────────────────────────────────
  # C. LEAVE-ONE-DIMENSION-OUT
  # ───────────────────────────────────────────────
  loo_results <- purrr::map_dfr(vuln_cols, function(drop_col) {
    remaining <- setdiff(vuln_cols, drop_col)

    d <- risk_data |>
      dplyr::filter(!is.na(Risk_norm)) |>
      dplyr::mutate(
        Vulnerability_LOO = rowMeans(
          dplyr::pick(dplyr::all_of(remaining)), na.rm = TRUE
        )
      ) |>
      dplyr::group_by(Sector_ID) |>
      dplyr::mutate(Vulnerability_LOO = range01(Vulnerability_LOO)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Risk_LOO = compute_tri(Exposure, Vulnerability_LOO, 0.5)
      ) |>
      dplyr::group_by(Sector_ID) |>
      dplyr::mutate(Risk_LOO_norm = range01(Risk_LOO)) |>
      dplyr::ungroup()

    rho <- cor(d$Risk_norm, d$Risk_LOO_norm,
               use = "pairwise.complete.obs", method = "spearman")

    tibble::tibble(test = paste0("Drop ", drop_col),
                   spearman_rho = round(rho, 4))
  })

  # ───────────────────────────────────────────────
  # D. GEOMETRIC VS ARITHMETIC AGGREGATION
  # ───────────────────────────────────────────────
  arith <- risk_data |>
    dplyr::filter(!is.na(Risk_norm)) |>
    dplyr::mutate(
      Risk_arith = 0.5 * Exposure + 0.5 * Vulnerability
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Risk_arith_norm = range01(Risk_arith)) |>
    dplyr::ungroup()

  rho_agg <- cor(arith$Risk_norm, arith$Risk_arith_norm,
                 use = "pairwise.complete.obs", method = "spearman")

  agg_row <- tibble::tibble(test = "Arithmetic mean",
                            spearman_rho = round(rho_agg, 4))

  # ───────────────────────────────────────────────
  # E. Z-SCORE NORMALIZATION
  # ───────────────────────────────────────────────
  zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

  z_data <- risk_data |>
    dplyr::filter(!is.na(Risk_norm)) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("Exposure", vuln_cols)), zscore
    )) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Vulnerability_Z = rowMeans(
        dplyr::pick(dplyr::all_of(vuln_cols)), na.rm = TRUE
      )
    ) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(
      Vulnerability_Z = range01(Vulnerability_Z),
      Exposure_Z      = range01(Exposure)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(Risk_Z = compute_tri(Exposure_Z, Vulnerability_Z, 0.5)) |>
    dplyr::group_by(Sector_ID) |>
    dplyr::mutate(Risk_Z_norm = range01(Risk_Z)) |>
    dplyr::ungroup()

  rho_z <- cor(z_data$Risk_norm, z_data$Risk_Z_norm,
               use = "pairwise.complete.obs", method = "spearman")

  zscore_row <- tibble::tibble(test = "Z-score normalization",
                               spearman_rho = round(rho_z, 4))

  # ───────────────────────────────────────────────
  # F. EDGAR-BASED SCOPE 1 SENSITIVITY (C19-C20, C23, C24)
  # ───────────────────────────────────────────────
  edgar_rows <- if (!is.null(edgar_scope1)) {
    run_edgar_sensitivity(risk_data, edgar_scope1)
  } else {
    tibble::tibble(test = character(0), spearman_rho = numeric(0))
  }

  # ───────────────────────────────────────────────
  # COMBINE
  # ───────────────────────────────────────────────
  dplyr::bind_rows(alpha_results, pca_row, loo_results,
                   agg_row, zscore_row, edgar_rows)
}
