library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

###

data <- read_excel("Code and data/Derived data/3_Normalized_data_wide.xlsx")

# ── Exposure: multi-component (GHG + Scope2 + Policy Pressure) ──
exposure_vars <- c("GHG_Emissions", "Scope2_Emissions", "Policy_Pressure")

# ── Vulnerability: 7 dimensions ─────────────────────────────────
dimension <- list(
  Energy          = c("Energy_Consumption", "Fossil_Share", "Renewables_Share", "RE_Potential"),
  Labour          = c("Unemployment_Rate", "Labour_Market_Slack", "Highly_Skilled_Workers"),
  Finance         = c("Gross_Fixed_Capital_Formation"),
  Supply_Chain    = c("Import_ExtraEU"),
  Technology      = c("BERD", "Regional_Innovation"),
  Institutions    = c("QoG_Index", "Climate_Mitigation_Laws"),
  Diversification = c("HHI_Employment")
)

# Note: Stranded_Asset_Proxy (Fossil_Share x GFCF) was tested but removed
# because it correlates 0.95 with Fossil_Share (already in Energy dimension),
# creating double-counting. Stranded asset risk is discussed qualitatively.

# ── Check which indicators actually exist in the data ────────────
all_vars <- c(exposure_vars, unlist(dimension))
missing_vars <- setdiff(all_vars, names(data))
if (length(missing_vars) > 0) {
  cat("WARNING: Missing indicators (will be ignored):", paste(missing_vars, collapse = ", "), "\n")
  # Remove missing vars from definitions
  exposure_vars <- intersect(exposure_vars, names(data))
  for (p in names(dimension)) {
    dimension[[p]] <- intersect(dimension[[p]], names(data))
    if (length(dimension[[p]]) == 0) {
      cat("  Dropping empty dimension:", p, "\n")
      dimension[[p]] <- NULL
    }
  }
}

cat("Exposure variables:", paste(exposure_vars, collapse = ", "), "\n")
cat("Vulnerability dimensions:\n")
for (p in names(dimension)) {
  cat("  ", p, ":", paste(dimension[[p]], collapse = ", "), "\n")
}

# ── Impute missing values with sector-country or EU median ───────
impute_with_median <- function(df, col) {
  df %>%
    group_by(Country_ID, Sector_ID) %>%
    mutate(temp_median = median(.data[[col]], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      !!rlang::sym(col) := dplyr::if_else(
        is.na(.data[[col]]), temp_median, .data[[col]]
      )
    ) %>%
    select(-temp_median)
}

all_existing <- c(exposure_vars, unlist(dimension))

for (var in all_existing) {
  data <- impute_with_median(data, var)
}

# ── Helper to re-scale x to [0.01, 0.99] ────────────────────────
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.50, length(x)))
  scaled <- 0.01 + 0.98 * (x - rng[1]) / diff(rng)
  scaled[x == 0] <- 0          # keep zeros at zero
  scaled
}

# ── Compute composite Exposure from sub-indicators ───────────────
data <- data %>%
  mutate(Exposure = rowMeans(across(all_of(exposure_vars)), na.rm = TRUE)) %>%
  group_by(Sector_ID) %>%
  mutate(Exposure = range01(Exposure)) %>%
  ungroup()

# ── Dimension scores: Vuln_Energy ... Vuln_Diversification ───────
for (p in names(dimension)) {
  data <- data %>%
    mutate(!!paste0("Vuln_", p) :=
             rowMeans(across(all_of(dimension[[p]])), na.rm = TRUE))
}

# ── Normalise each Vuln dimension by sector ──────────────────────
data <- data %>%
  group_by(Sector_ID) %>%
  mutate(across(starts_with("Vuln_"), range01)) %>%
  ungroup()

# ── Vulnerability index (simple average of all dimensions) ───────
data <- data %>%
  mutate(Vulnerability =
           rowMeans(across(starts_with("Vuln_")), na.rm = TRUE)) %>%
  group_by(Sector_ID) %>%
  mutate(Vulnerability = range01(Vulnerability)) %>%
  ungroup()

# ── TRI = Exposure^alpha * Vulnerability^(1-alpha) ───────────────
alpha <- 0.50
data <- data %>%
  mutate(
    Exposure  = if_else(Exposure == 0, NA_real_, Exposure),
    Risk_raw  = Exposure^alpha * Vulnerability^(1 - alpha)
  ) %>%
  group_by(Sector_ID) %>%
  mutate(Risk_norm = range01(Risk_raw)) %>%
  ungroup()

# ── Risk bands ───────────────────────────────────────────────────
breaks_geo <- seq(0, 1, by = 0.20)
band_labels <- c("Very Low", "Low", "Medium", "High", "Very High")

data <- data %>%
  mutate(
    Risk_Band = cut(
      Risk_norm,
      breaks = breaks_geo,
      include.lowest = TRUE,
      labels = band_labels
    )
  ) %>%
  select(-Risk_raw)

data <- data %>%
  mutate(
    Risk_Band = case_when(
      is.na(Risk_norm) ~ "Zero Risk",
      TRUE ~ as.character(Risk_Band)
    )
  )

# ── Summary statistics ───────────────────────────────────────────
cat("\n=== Risk Summary ===\n")
cat("Total observations:", nrow(data), "\n")
cat("Risk band distribution:\n")
print(table(data$Risk_Band, useNA = "ifany"))

cat("\nExposure stats:\n")
print(summary(data$Exposure))
cat("\nVulnerability stats:\n")
print(summary(data$Vulnerability))
cat("\nTRI stats:\n")
print(summary(data$Risk_norm))

# ── Write ───���────────────────────────────────────────────────────
writexl::write_xlsx(data, "Code and data/Final data/Risk_data.xlsx")
write.csv(data, "Code and data/Final data/Risk_data.csv", row.names = FALSE)
cat("\nSaved: Risk_data.xlsx and Risk_data.csv\n")
