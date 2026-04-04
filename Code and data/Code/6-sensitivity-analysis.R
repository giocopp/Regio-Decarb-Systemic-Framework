library(tidyverse)
library(readxl)
library(writexl)

###
# ── SENSITIVITY ANALYSIS ─────────────────────────────────────────
# Tests robustness of TRI to methodological choices.
# Addresses Reviewer 3's request for sensitivity checks.
# Run AFTER step 4 (requires Risk_data.xlsx).
# ──────────────────────────────────────────────────────────────────

data <- read_excel("Code and data/Final data/Risk_data.xlsx")

# ── Helper functions ─────────────────────────────────────────────
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.50, length(x)))
  scaled <- 0.01 + 0.98 * (x - rng[1]) / diff(rng)
  scaled[x == 0] <- 0
  scaled
}

compute_tri <- function(exposure, vulnerability, alpha) {
  exposure^alpha * vulnerability^(1 - alpha)
}

# Baseline reference: Risk_norm from the main analysis
baseline <- data %>%
  filter(!is.na(Risk_norm)) %>%
  select(NUTS_ID, Sector_ID, Risk_norm_baseline = Risk_norm,
         Exposure, Vulnerability)

vuln_cols <- names(data)[grepl("^Vuln_", names(data))]
cat("Vulnerability dimensions:", paste(vuln_cols, collapse = ", "), "\n")

# ══════════════════════════════════════════════════════════════════
# A. ALPHA SENSITIVITY
# ══════════════════════════════════════════════════════════════════
cat("\n=== A. Alpha Sensitivity ===\n")

alpha_grid <- c(0.30, 0.40, 0.50, 0.60, 0.70)

alpha_results <- map_dfr(alpha_grid, function(a) {
  d <- baseline %>%
    mutate(
      Risk_sa = compute_tri(Exposure, Vulnerability, a)
    ) %>%
    group_by(Sector_ID) %>%
    mutate(Risk_sa_norm = range01(Risk_sa)) %>%
    ungroup()

  rho <- cor(d$Risk_norm_baseline, d$Risk_sa_norm,
             use = "pairwise.complete.obs", method = "spearman")

  tibble(alpha = a, spearman_rho = round(rho, 4))
})

cat("Rank correlations vs baseline (alpha=0.5):\n")
print(as.data.frame(alpha_results))

# ══════════════════════════════════════════════════════════════════
# B. WEIGHTING SENSITIVITY (Equal vs PCA-derived)
# ══════════════════════════════════════════════════════════════════
cat("\n=== B. Weighting Sensitivity ===\n")

vuln_data <- data %>%
  filter(!is.na(Risk_norm)) %>%
  select(NUTS_ID, Sector_ID, all_of(vuln_cols))

# PCA on the dimension scores
vuln_matrix <- vuln_data %>% select(all_of(vuln_cols)) %>% as.matrix()
complete_rows <- complete.cases(vuln_matrix)
pca_fit <- prcomp(vuln_matrix[complete_rows, ], center = TRUE, scale. = TRUE)

cat("PCA variance explained:\n")
var_exp <- summary(pca_fit)$importance[2, ]
cat(paste(sprintf("  PC%d: %.1f%%", 1:length(var_exp), var_exp * 100), collapse = "\n"), "\n")

# Use PC1 loadings as weights
pca_weights <- abs(pca_fit$rotation[, 1])
pca_weights <- pca_weights / sum(pca_weights)
cat("\nPCA-derived weights:\n")
for (i in seq_along(vuln_cols)) {
  cat(sprintf("  %-25s %.3f (equal: %.3f)\n",
              vuln_cols[i], pca_weights[i], 1/length(vuln_cols)))
}

# Compute PCA-weighted vulnerability
pca_vuln <- data %>%
  filter(!is.na(Risk_norm)) %>%
  mutate(
    Vulnerability_PCA = as.numeric(
      as.matrix(across(all_of(vuln_cols))) %*% pca_weights
    )
  ) %>%
  group_by(Sector_ID) %>%
  mutate(Vulnerability_PCA = range01(Vulnerability_PCA)) %>%
  ungroup()

# Compute TRI with PCA weights
pca_vuln <- pca_vuln %>%
  mutate(
    Risk_PCA = compute_tri(Exposure, Vulnerability_PCA, 0.5)
  ) %>%
  group_by(Sector_ID) %>%
  mutate(Risk_PCA_norm = range01(Risk_PCA)) %>%
  ungroup()

rho_pca <- cor(pca_vuln$Risk_norm, pca_vuln$Risk_PCA_norm,
               use = "pairwise.complete.obs", method = "spearman")
cat(sprintf("\nSpearman rank correlation (equal vs PCA weights): %.4f\n", rho_pca))

# ══════════════════════════════════════════════════════════════════
# C. LEAVE-ONE-DIMENSION-OUT
# ══════════════════════════════════════════════════════════════════
cat("\n=== C. Leave-One-Dimension-Out ===\n")

loo_results <- map_dfr(vuln_cols, function(drop_col) {
  remaining <- setdiff(vuln_cols, drop_col)

  d <- data %>%
    filter(!is.na(Risk_norm)) %>%
    mutate(
      Vulnerability_LOO = rowMeans(across(all_of(remaining)), na.rm = TRUE)
    ) %>%
    group_by(Sector_ID) %>%
    mutate(Vulnerability_LOO = range01(Vulnerability_LOO)) %>%
    ungroup() %>%
    mutate(
      Risk_LOO = compute_tri(Exposure, Vulnerability_LOO, 0.5)
    ) %>%
    group_by(Sector_ID) %>%
    mutate(Risk_LOO_norm = range01(Risk_LOO)) %>%
    ungroup()

  rho <- cor(d$Risk_norm, d$Risk_LOO_norm,
             use = "pairwise.complete.obs", method = "spearman")

  tibble(dropped = drop_col, spearman_rho = round(rho, 4))
})

cat("Impact of dropping each dimension:\n")
print(as.data.frame(loo_results %>% arrange(spearman_rho)))

# ══════════════════════════════════════════════════════════════════
# D. AGGREGATION: GEOMETRIC vs ARITHMETIC MEAN
# ══════════════════════════════════════════════════════════════════
cat("\n=== D. Aggregation Sensitivity ===\n")

arith <- data %>%
  filter(!is.na(Risk_norm)) %>%
  mutate(
    Risk_arith = 0.5 * Exposure + 0.5 * Vulnerability
  ) %>%
  group_by(Sector_ID) %>%
  mutate(Risk_arith_norm = range01(Risk_arith)) %>%
  ungroup()

rho_agg <- cor(arith$Risk_norm, arith$Risk_arith_norm,
               use = "pairwise.complete.obs", method = "spearman")
cat(sprintf("Spearman rank correlation (geometric vs arithmetic): %.4f\n", rho_agg))

# ══════════════════════════════════════════════════════════════════
# SUMMARY TABLE
# ══════════════════════════════════════════════════════════════════
cat("\n\n========================================\n")
cat("       SENSITIVITY ANALYSIS SUMMARY     \n")
cat("========================================\n\n")

summary_table <- bind_rows(
  alpha_results %>% mutate(test = paste0("Alpha = ", alpha)) %>% select(test, spearman_rho),
  tibble(test = "PCA weights", spearman_rho = round(rho_pca, 4)),
  loo_results %>% mutate(test = paste0("Drop ", dropped)) %>% select(test, spearman_rho),
  tibble(test = "Arithmetic mean", spearman_rho = round(rho_agg, 4))
)

print(as.data.frame(summary_table))

# ── Save results ─────────────────────────────────────────────────
writexl::write_xlsx(summary_table, "Code and data/Final data/Sensitivity_Analysis.xlsx")
cat("\nSaved: Sensitivity_Analysis.xlsx\n")
