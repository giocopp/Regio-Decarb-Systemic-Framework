# ── Create-QoG_Institutions.R ─────────────────────────────────────
# Processes the European Quality of Government Index (EQI) from
# the QoG Institute at University of Gothenburg.
#
# Data source: QoG EU Regional Dataset (wide format, NUTS-2)
#   Downloaded from: https://www.gu.se/en/quality-government/qog-data/data-downloads/eu-regional-dataset
#   File: Code and data/Initial data/Non sector data/qog_eureg.csv
#
# Reference: Charron, Dijkstra & Lapuente (2014, 2021)
#   "Mapping the Regional Divide in Europe"
#
# EQI survey waves available: 2010, 2013, 2017
# We use 2017 (most recent available wave).
# ──────────────────────────────────────────────────────────────────

library(dplyr)
library(writexl)

eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","EL","HU","IE","IT",
          "LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")

# ── 1) Read QoG data ─────────────────────────────────────────────
qog_path <- "Code and data/Initial data/Non sector data/qog_eureg.csv"
if (!file.exists(qog_path)) {
  stop("QoG data file not found at: ", qog_path,
       "\nDownload from: https://www.gu.se/en/quality-government/qog-data/data-downloads/eu-regional-dataset")
}

cat("Reading QoG data...\n")
qog <- read.csv(qog_path, stringsAsFactors = FALSE)
cat("Total rows:", nrow(qog), "x", ncol(qog), "columns\n")

# ── 2) Use latest EQI survey wave (2017) ─────────────────────────
# Check available EQI years
eqi_by_year <- tapply(!is.na(qog$eqi_score_nuts2), qog$year, sum)
eqi_years <- eqi_by_year[eqi_by_year > 0]
cat("EQI survey years:", paste(names(eqi_years), collapse = ", "), "\n")

# Use 2017 (latest available wave)
eqi_year <- 2017
if (!(eqi_year %in% as.numeric(names(eqi_years)))) {
  eqi_year <- max(as.numeric(names(eqi_years)))
  cat("Warning: 2017 not available, using", eqi_year, "instead\n")
}

qog_filt <- qog %>%
  filter(year == eqi_year, !is.na(eqi_score_nuts2), !is.na(nuts2)) %>%
  select(NUTS_ID = nuts2, EQI = eqi_score_nuts2)

cat("Year", eqi_year, ":", nrow(qog_filt), "regions with EQI scores\n")
cat("EQI range:", round(min(qog_filt$EQI), 3), "to", round(max(qog_filt$EQI), 3), "\n")

# ── 3) Filter to EU-27 NUTS-2 ────────────────────────────────────
qog_clean <- qog_filt %>%
  filter(nchar(NUTS_ID) == 4, substr(NUTS_ID, 1, 2) %in% eu27)

cat("EU-27 NUTS-2 regions:", nrow(qog_clean), "\n")
cat("EQI mean:", round(mean(qog_clean$EQI), 3), "\n")

# ── 4) Format output ─────────────────────────────────────────────
qog_out <- qog_clean %>%
  transmute(
    Country_CD   = substr(NUTS_ID, 1, 2),
    Country_Name = NA_character_,
    NUTS_ID      = NUTS_ID,
    NUTS_Name    = NA_character_,
    Sector_CD    = NA_character_,
    Sector_ID    = NA_character_,
    Component    = "Vulnerability",
    Dimension    = "Institutions",
    Variable     = "QoG_Index",
    Unit         = "EQI Score",
    Value        = round(EQI, 6),
    Value_Norm   = NA_real_
  )

cat("Output rows:", nrow(qog_out), "\n")

# ── 5) Save ──────────────────────────────────────────────────────
writexl::write_xlsx(
  qog_out,
  "Code and data/Initial data/Non sector data/INST-QoG.xlsx"
)
cat("Saved: Code and data/Initial data/Non sector data/INST-QoG.xlsx\n")
