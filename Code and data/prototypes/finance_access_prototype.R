# finance_access_prototype.R — candidate "access to finance" vulnerability
# indicators at NUTS-2, built 2026-07-03. NOT wired into the pipeline: this
# script produces the evidence tables for the go/no-go decision.
#
# Two access channels, both open data:
#   PHYSICAL: bank branches. OSM (amenity=bank, pinned snapshot via ohsome
#     API) gives the WITHIN-country distribution; the ECB structural series
#     "Number of offices, world" (SSI.A.{cc}.122C.N40.1.A1.Z0Z.Z, annual,
#     2025 available; magnitudes verified: IT 19,182 / DE 17,306 / NL 980)
#     gives the official national TOTAL. Calibrated regional branches =
#     ECB total x OSM regional share -> removes cross-country OSM
#     completeness bias (same pattern as the CBAM component: official
#     national totals, open-data regional distribution).
#     The discontinued "Number of branches, domestic" (N10, ends 2020) is
#     kept as a cross-check column.
#   DIGITAL: internet-banking usage, Eurostat isoc_r_iuse_i I_IUBK
#     (NUTS-2, 213 regions, 2021-2025), NUTS-1/national fallback for the rest.
#
# Validation outputs (printed + prototypes/finance_access_output.csv):
#   F1 completeness: OSM country totals vs ECB offices (ratio table)
#   F2 direction:    country means vs ECB SAFE "financing obstacles" (FOB)
#   F3 independence: correlations with Exposure, Vulnerability, income
#   TODO (manual anchor, like ISTAT in cbam_trade_validation.R): Banca
#   d'Italia publishes branch counts by province (BDS: "sportelli") — export
#   and correlate for the within-country check.
#
# OSM data (c) OpenStreetMap contributors, ODbL. Counts per region are
# aggregate facts; attribute OSM in any figure using them.
#
# ── VERDICT (2026-07-07, after the full run): NOT ADOPTED ────────────────────
# F1: OSM/ECB completeness 0.25 (BG) - 1.99 (CY): raw cross-country OSM counts
#     unusable; the ECB-total x OSM-share calibration works mechanically.
# F2: the direction check FAILS. Branches/10k vs SAFE firm-reported financing
#     obstacles is sign-UNSTABLE at country level (n=12 machine-readable
#     countries): -0.22 averaging the last 3 waves, +0.27 on the latest wave
#     alone. The cross-country branch-density gradient does not track firm
#     obstacles (digital-substitution / banking-structure confound), so a
#     POOLED regional indicator cannot be given a defensible orientation.
# I_IUBK (digital channel) dropped separately: it measures HOUSEHOLD
#     behaviour (index carries no household indicators) and shows ~no relation
#     to firm obstacles (-0.08).
# Firm-side alternatives checked: SAFE = country-level only, machine-readable
#     for 12 euro-area countries (AT BE DE EL ES FI FR IE IT NL PT SK; the
#     EU-27 annual wave lives in EC report annexes, not the API). EIBIS
#     (country x sector, incl. manufacturing) download portal is a JSF
#     session app (jsessionid URLs) - no stable scriptable endpoint found.
# => No defensible regional access-to-finance construct from open data.
#    This file + finance_access_output.csv stand as the due-diligence
#    evidence backing METHODOLOGY §11.1 (no Finance dimension).
#
# Run from "Code and data/":  Rscript prototypes/finance_access_prototype.R

suppressMessages({library(dplyr); library(tidyr); library(readr)
                  library(sf); library(giscoR); library(jsonlite)})

PIN_TIME <- "2026-01-01"   # OSM snapshot (clipped to ohsome extent if needed)
OUT <- "prototypes/finance_access_output.csv"
eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
          "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")

## ── region grid: the 230 index regions; HR04 queried as HR02+HR05+HR06 ────
grid <- read.csv("Final data/Risk_data.csv") |>
  distinct(NUTS_ID) |> pull(NUTS_ID) |> sort()
q_ids <- c(setdiff(grid, "HR04"), "HR02", "HR05", "HR06")

n2 <- gisco_get_nuts(nuts_level = "2", year = "2021", resolution = "10") |>
  filter(NUTS_ID %in% q_ids) |> select(NUTS_ID)
stopifnot(nrow(n2) == length(q_ids))

## ── A. OSM branch counts via ohsome (one POST per country) ────────────────
meta <- fromJSON("https://api.ohsome.org/v1/metadata")
t_max <- substr(meta$extractRegion$temporalExtent$toTimestamp, 1, 10)
t_use <- min(PIN_TIME, t_max)
cat("ohsome snapshot:", t_use, "(requested", PIN_TIME, ", extent to", t_max, ")\n")

osm_file <- "prototypes/osm_bank_counts_nuts2.csv"
if (file.exists(osm_file)) {
  osm <- read_csv(osm_file, show_col_types = FALSE)
  cat("using cached", osm_file, "-", nrow(osm), "regions\n")
} else {
  # one region per request via the plain /elements/count endpoint: slower
  # but unambiguous (no group-id mapping) and robust to server timeouts.
  # Incremental cache: each region is appended as soon as it returns.
  cache <- "prototypes/osm_bank_counts_cache.csv"
  have <- if (file.exists(cache)) read_csv(cache, show_col_types = FALSE)$NUTS_ID else character(0)
  for (id in setdiff(q_ids, have)) {
    sub <- n2 |> filter(NUTS_ID == id)
    gj <- tempfile(fileext = ".geojson")
    st_write(sub, gj, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
    val <- NA_real_
    for (try in 1:3) {
      out <- system2("curl",
        c("-s", "--max-time", "120", "-X", "POST",
          shQuote("https://api.ohsome.org/v1/elements/count"),
          "--data-urlencode", shQuote(paste0("bpolys@", gj)),
          "--data-urlencode",
          shQuote("filter=amenity=bank and (type:node or type:way)"),
          "--data-urlencode", shQuote(paste0("time=", t_use))),
        stdout = TRUE)
      j <- tryCatch(fromJSON(paste(out, collapse = "")), error = function(e) NULL)
      if (!is.null(j$result$value)) { val <- j$result$value[1]; break }
      Sys.sleep(c(5, 20, 60)[try])
    }
    if (is.na(val)) { cat("  !!", id, "failed after 3 tries\n"); next }
    write_csv(tibble(NUTS_ID = id, osm_banks = val, snapshot = t_use),
              cache, append = file.exists(cache))
    cat("  ", id, val, "\n")
    Sys.sleep(0.7)
  }
  osm <- read_csv(cache, show_col_types = FALSE) |> select(NUTS_ID, osm_banks)
  stopifnot(!any(duplicated(osm$NUTS_ID)))
  # recombine HR04
  hr <- osm |> filter(NUTS_ID %in% c("HR02","HR05","HR06")) |>
    summarise(NUTS_ID = "HR04", osm_banks = sum(osm_banks))
  osm <- osm |> filter(!NUTS_ID %in% c("HR02","HR05","HR06")) |> bind_rows(hr)
  osm$snapshot <- t_use
  write_csv(osm, osm_file)
  cat("wrote", osm_file, "\n")
}

## ── B. ECB national anchors ───────────────────────────────────────────────
ecb_csv <- function(key) {
  u <- paste0("https://data-api.ecb.europa.eu/service/data/SSI/", key,
              "?format=csvdata")
  tryCatch(read_csv(u, show_col_types = FALSE), error = function(e) NULL)
}
ecb <- bind_rows(lapply(eu27, function(cc) {
  cc_ecb <- ifelse(cc == "EL", "GR", cc)   # ECB uses GR
  d <- ecb_csv(paste0("A.", cc_ecb, ".122C.N40.1.A1.Z0Z.Z"))
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d |> filter(!is.na(OBS_VALUE)) |> slice_max(TIME_PERIOD, n = 1) |>
    transmute(Country_ID = cc, ecb_offices = OBS_VALUE, ecb_year = TIME_PERIOD)
}))
cat("ECB offices anchor: ", nrow(ecb), "countries, years",
    paste(range(ecb$ecb_year), collapse = "-"), "\n")

## ── C. population + calibrated branches per 10k ───────────────────────────
suppressMessages(library(restatapi))
pop <- get_eurostat_data("demo_r_d2jan",
        filters = list(sex = "T", age = "TOTAL"), date_filter = 2023:2025,
        exact_match = TRUE, label = FALSE) |> as_tibble() |>
  mutate(geo = as.character(geo), values = as.numeric(values)) |>
  filter(nchar(geo) == 4, substr(geo, 1, 2) %in% eu27, !is.na(values)) |>
  mutate(time = as.character(time)) |> group_by(geo) |>
  slice_max(time, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(NUTS_ID = geo, pop = values)
hr_pop <- pop |> filter(NUTS_ID %in% c("HR02","HR05","HR06")) |>
  summarise(NUTS_ID = "HR04", pop = sum(pop))
pop <- pop |> filter(!NUTS_ID %in% c("HR02","HR05","HR06")) |> bind_rows(hr_pop)

d <- osm |> mutate(Country_ID = substr(NUTS_ID, 1, 2)) |>
  group_by(Country_ID) |> mutate(osm_share = osm_banks / sum(osm_banks)) |>
  ungroup() |>
  left_join(ecb, by = "Country_ID") |>
  mutate(branches_cal = ecb_offices * osm_share) |>
  left_join(pop, by = "NUTS_ID") |>
  mutate(branches_per10k = 1e4 * branches_cal / pop)

## ── D. digital access: I_IUBK with NUTS-1 / national fallback ─────────────
iu_raw <- get_eurostat_data("isoc_r_iuse_i", filters = list(indic_is = "I_IUBK"),
        date_filter = 2022:2025, exact_match = TRUE, label = FALSE) |>
  as_tibble() |>
  mutate(geo = as.character(geo), values = as.numeric(values),
         time = as.character(time)) |> filter(!is.na(values))
latest_of <- function(df) df |> group_by(geo) |>
  slice_max(time, n = 1, with_ties = FALSE) |> ungroup()
iu2 <- latest_of(iu_raw |> filter(nchar(geo) == 4)) |>
  transmute(NUTS_ID = geo, iubk = values)
d <- d |> left_join(iu2, by = "NUTS_ID")
n1 <- latest_of(iu_raw |> filter(nchar(geo) == 3)) |>
  transmute(n1 = geo, iubk_n1 = values)
n0 <- latest_of(iu_raw |> filter(nchar(geo) == 2)) |>
  transmute(n0 = geo, iubk_n0 = values)
d <- d |>
  mutate(n1 = substr(NUTS_ID, 1, 3), n0 = substr(NUTS_ID, 1, 2)) |>
  left_join(n1, by = "n1") |> left_join(n0, by = "n0") |>
  mutate(iubk_src = case_when(!is.na(iubk) ~ "nuts2",
                              !is.na(iubk_n1) ~ "nuts1",
                              !is.na(iubk_n0) ~ "national",
                              TRUE ~ "missing"),
         iubk = coalesce(iubk, iubk_n1, iubk_n0)) |>
  select(-n1, -n0, -iubk_n1, -iubk_n0)
cat("I_IUBK source mix:\n"); print(table(d$iubk_src))

## ── E. draft access score (indicative; pooled min-max, both reversed) ─────
r01 <- function(x) (x - min(x, na.rm = TRUE)) /
                   (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
d <- d |> mutate(access_draft = 1 - (r01(branches_per10k) + r01(iubk)) / 2)

## ── F. validation tables ──────────────────────────────────────────────────
cat("\n== F1: OSM completeness vs ECB offices (country totals) ==\n")
f1 <- d |> group_by(Country_ID) |>
  summarise(osm = sum(osm_banks), ecb = first(ecb_offices),
            ecb_year = first(ecb_year)) |>
  mutate(ratio = round(osm / ecb, 2)) |> arrange(ratio)
print(as.data.frame(f1), row.names = FALSE)

cat("\n== F2: direction check vs ECB SAFE financing obstacles (FOB) ==\n")
safe <- tryCatch({
  u <- paste0("https://data-api.ecb.europa.eu/service/data/SAFE/",
              "H.......FOB....?format=csvdata&lastNObservations=1")
  read_csv(u, show_col_types = FALSE)
}, error = function(e) NULL)
if (!is.null(safe) && nrow(safe) > 0) {
  agg <- safe |>
    filter(REF_AREA %in% c(eu27, "GR")) |>
    mutate(Country_ID = ifelse(REF_AREA == "GR", "EL", REF_AREA))
  # keep the most aggregate firm breakdowns available per country
  for (v in c("FIRM_SIZE","FIRM_SECTOR","FIRM_TURNOVER","FIRM_AGE","FIRM_OWNERSHIP"))
    if (v %in% names(agg)) {
      tot <- agg |> count(.data[[v]]) |> slice_max(n, n = 1) |> pull(1)
      agg <- agg |> filter(.data[[v]] == tot[1])
    }
  safe_c <- agg |> group_by(Country_ID) |>
    summarise(safe_fob = mean(OBS_VALUE, na.rm = TRUE))
  f2 <- d |> group_by(Country_ID) |>
    summarise(br10k = mean(branches_per10k, na.rm = TRUE),
              iubk = mean(iubk, na.rm = TRUE),
              access_draft = mean(access_draft, na.rm = TRUE)) |>
    inner_join(safe_c, by = "Country_ID")
  cat("countries matched:", nrow(f2), " (SAFE dims held at most-aggregate codes)\n")
  for (v in c("br10k","iubk","access_draft"))
    cat(sprintf("  spearman(%s, SAFE obstacles) = %+.2f\n", v,
        cor(f2[[v]], f2$safe_fob, method = "spearman")))
} else cat("SAFE pull failed - direction check to be run separately\n")

cat("\n== F3: independence / overlap ==\n")
rd <- read.csv("Final data/Risk_data.csv") |> filter(Sector_ID == "C") |>
  select(NUTS_ID, Exposure, Vulnerability)
inc <- get_eurostat_data("nama_10r_2hhinc",
        filters = list(unit = "EUR_HAB", direct = "BAL", na_item = "B6N"),
        date_filter = 2021:2023, exact_match = TRUE, label = FALSE) |>
  as_tibble() |>
  mutate(geo = as.character(geo), values = as.numeric(values),
         time = as.character(time)) |>
  filter(nchar(geo) == 4, !is.na(values)) |> group_by(geo) |>
  slice_max(time, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(NUTS_ID = geo, income = values)
f3 <- d |> left_join(rd, by = "NUTS_ID") |> left_join(inc, by = "NUTS_ID")
for (p in list(c("access_draft","Exposure"), c("access_draft","Vulnerability"),
               c("access_draft","income"), c("branches_per10k","iubk"),
               c("branches_per10k","income"), c("iubk","income")))
  cat(sprintf("  spearman(%s, %s) = %+.2f  (n=%d)\n", p[1], p[2],
      cor(f3[[p[1]]], f3[[p[2]]], method = "spearman", use = "pair"),
      sum(complete.cases(f3[[p[1]]], f3[[p[2]]]))))

write_csv(d |> select(NUTS_ID, Country_ID, osm_banks, osm_share, ecb_offices,
                      ecb_year, branches_cal, pop, branches_per10k,
                      iubk, iubk_src, access_draft), OUT)
cat("\nwrote", OUT, "\n")
