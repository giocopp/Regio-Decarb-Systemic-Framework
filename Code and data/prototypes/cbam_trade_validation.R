# cbam_trade_validation.R — external validation of the CBAM-leg regional
# allocation against official regional trade statistics.
#
# The CBAM leg allocates national embodied-import carbon of the covered goods
# (FIGARO origin industries C20/C23/C24) to NUTS-2 with hybrid weights:
# geocoded plant-emission shares for the four heavy sectors, employment
# shares otherwise (build_cbam_weights). This script tests that allocation
# against the observed regional geography of extra-EU imports:
#
#   modeled  : region share of national extra-EU import VALUE of covered
#              goods implied by the pipeline weights (FIGARO 2023 flows ×
#              the exact weights the index uses), per good and total
#   observed : region share of extra-EU imports from official statistics
#
# Observed inputs:
#   (a) COARSE ANCHOR (committed): ISTAT extra-EU imports by region, CPA
#       section C (all manufacturing) + ALL, 2023-2024 —
#       prototypes/observed_imports_IT_sectionC_2023.csv, pulled 2026-07-02
#       from SDMX flow DF_DCSE_CPA_ATECO2007_COE_A_Q_CONJ_REG
#       (https://esploradati.istat.it/SDMXWS/rest/data/...; the regional
#       SDMX flows publish CPA sections only — section C ≠ covered goods,
#       so this anchor tests the broad import geography, not the goods mix).
#   (b) DIVISION LEVEL (manual export, used automatically when present):
#       prototypes/observed_imports_IT_divisions.csv
#       prototypes/observed_imports_ES_divisions.csv
#       columns: region_nuts2, cpa_division (20|23|24), year,
#       import_value_eur   — region_nuts2 in NUTS-2021 codes.
#       Recipes:
#       IT — https://esploradati.istat.it/coeweb/databrowser/ : imports,
#            value, partner "extra Ue 27", year 2023, merce = CPA divisions
#            20 / 23 / 24, per regione -> export CSV and reshape.
#       ES — https://datacomex.comercio.es/ : comercio declarado por CCAA
#            (aduanas), importaciones, extra-UE27, 2023, TARIC chapters
#            25 (->23), 31 (->20), 72+73+76 (->24) -> export and reshape.
#
# Caveats to report with any correlation:
#   - trade statistics record the declared region of destination/origin;
#     headquarters and logistics hubs distort regional attribution
#   - product-classified trade vs industry-classified model flows
#   - EUR value vs embodied tCO2 (the model adds origin carbon intensity
#     on top of the value geography; value is the like-for-like basis)
#
# Run from "Code and data/":  Rscript prototypes/cbam_trade_validation.R

suppressMessages({library(dplyr); library(tidyr); library(readr)})
source("R/utils.R"); source("R/exposure.R")

ew  <- targets::tar_read(empl_weights)
geo <- targets::tar_read(ets_geo)
fc  <- targets::tar_read(figaro_cache)
io  <- readRDS(fc[[1]]) |>
  mutate(across(c(c_orig, c_dest, ind_ava, ind_use), as.character))

eu27  <- .eu27_codes()
goods <- c("C20", "C23", "C24")
nonEU <- setdiff(unique(io$c_orig), c(eu27, "DOM"))
nmap  <- .figaro_nace_map()
w     <- build_cbam_weights(ew, geo)     # the exact weights the pipeline uses

# ── modeled regional shares of national extra-EU covered-good imports ──────
model_shares <- function(cc) {
  imp <- io |>
    filter(c_orig %in% nonEU, ind_ava %in% goods, c_dest == cc,
           ind_use %in% nmap$ind, values > 0) |>
    left_join(nmap, by = c("ind_use" = "ind")) |>
    group_by(good = ind_ava, Sector_ID) |>
    summarise(MEUR = sum(values), .groups = "drop")

  out <- imp |>
    inner_join(w |> filter(Country_ID == cc), by = "Sector_ID",
               relationship = "many-to-many") |>
    mutate(val = MEUR * weight) |>
    group_by(good, NUTS_ID) |>
    summarise(modeled_MEUR = sum(val), .groups = "drop") |>
    group_by(good) |>
    mutate(modeled_share = modeled_MEUR / sum(modeled_MEUR)) |>
    ungroup()

  # allocation must preserve the national totals exactly
  tot_model <- out |> group_by(good) |> summarise(m = sum(modeled_MEUR))
  stopifnot(all(abs(tot_model$m - imp |> group_by(good) |>
                      summarise(MEUR = sum(MEUR)) |> pull(MEUR)) < 1e-6))
  out
}

m_it <- model_shares("IT")
m_es <- model_shares("ES")
m_it_tot <- m_it |>
  group_by(NUTS_ID) |>
  summarise(modeled_MEUR = sum(modeled_MEUR), .groups = "drop") |>
  mutate(good = "TOTAL", modeled_share = modeled_MEUR / sum(modeled_MEUR))

# ── observed (a): ISTAT section-C anchor, 2023 ─────────────────────────────
# ISTAT territory codes are pre-2010 NUTS labels; map + assert the label.
it_map <- tribble(
  ~istat, ~NUTS_ID, ~expect,
  "ITC1","ITC1","Piemonte",       "ITC2","ITC2","Aosta",
  "ITC3","ITC3","Liguria",        "ITC4","ITC4","Lombardia",
  "ITD10","ITH1","Bolzano",       "ITD20","ITH2","Trento",
  "ITD3","ITH3","Veneto",         "ITD4","ITH4","Friuli",
  "ITD5","ITH5","Emilia",         "ITE1","ITI1","Toscana",
  "ITE2","ITI2","Umbria",         "ITE3","ITI3","Marche",
  "ITE4","ITI4","Lazio",          "ITF1","ITF1","Abruzzo",
  "ITF2","ITF2","Molise",         "ITF3","ITF3","Campania",
  "ITF4","ITF4","Puglia",         "ITF5","ITF5","Basilicata",
  "ITF6","ITF6","Calabria",       "ITG1","ITG1","Sicilia",
  "ITG2","ITG2","Sardegna"
)

obs_raw <- read_csv("prototypes/observed_imports_IT_sectionC_2023.csv",
                    show_col_types = FALSE, name_repair = "minimal")
names(obs_raw) <- sub(":.*$", "", names(obs_raw))
obs <- obs_raw |>
  transmute(area = REF_AREA, cpa = CPA_ATECO2007_COE,
            year = TIME_PERIOD, value = OBS_VALUE) |>
  separate(area, c("istat", "label"), sep = ": ", extra = "merge") |>
  filter(year == 2023, grepl("^C:", cpa))

dropped <- obs |> filter(!istat %in% it_map$istat)
if (nrow(dropped) > 0)
  cat("dropped territory codes (not NUTS-2 units):",
      paste(unique(dropped$istat), collapse = ", "), "\n")
obs <- obs |> inner_join(it_map, by = "istat")
bad <- obs |>
  filter(!mapply(grepl, expect, label,
                 MoreArgs = list(ignore.case = TRUE)))
if (nrow(bad) > 0) stop("territory label mismatch: ",
                        paste(bad$istat, bad$label, collapse = " | "))
obs <- obs |> transmute(NUTS_ID, obs_eur = value) |>
  mutate(obs_share = obs_eur / sum(obs_eur))

# ── compare: modeled covered-goods total vs observed section-C ─────────────
cmp <- inner_join(m_it_tot, obs, by = "NUTS_ID") |>
  arrange(desc(modeled_share))
cat("\n=== IT: modeled covered-goods import shares vs observed section-C",
    "extra-EU import shares (2023) ===\n")
cat("regions matched:", nrow(cmp), "\n")
cat(sprintf("Spearman rho: %.3f | Pearson r: %.3f\n",
            cor(cmp$modeled_share, cmp$obs_share, method = "spearman"),
            cor(cmp$modeled_share, cmp$obs_share)))
print(as.data.frame(cmp |>
  transmute(NUTS_ID, modeled = round(modeled_share, 3),
            observed = round(obs_share, 3),
            diff = round(modeled_share - obs_share, 3))))

# per-good detail against the same section-C anchor (informative only)
for (g in goods) {
  d <- m_it |> filter(good == g) |> inner_join(obs, by = "NUTS_ID")
  cat(sprintf("  %s vs section-C anchor: Spearman %.3f\n",
              g, cor(d$modeled_share, d$obs_share, method = "spearman")))
}

# ── ceiling test ────────────────────────────────────────────────────────────
# A region's covered-good imports cannot exceed its TOTAL manufacturing
# imports. Regions where the modeled allocation breaks that ceiling are
# over-allocated regardless of the goods mix (basis caveats: FIGARO MRIO
# valuation vs customs cif; declarant-region attribution in the observed
# data can also understate plant regions whose imports are declared at a
# headquarters elsewhere).
ceil <- cmp |>
  mutate(modeled_eur = modeled_MEUR * 1e6,
         ceiling_ratio = modeled_eur / obs_eur) |>
  filter(ceiling_ratio > 1) |>
  arrange(desc(ceiling_ratio))
if (nrow(ceil) > 0) {
  cat("\nregions where modeled covered-good imports EXCEED total observed\n",
      "section-C imports (physical ceiling):\n")
  print(as.data.frame(ceil |>
    transmute(NUTS_ID, modeled_bn = round(modeled_eur/1e9, 2),
              observed_sectionC_bn = round(obs_eur/1e9, 2),
              ratio = round(ceiling_ratio, 2))))
}

# ── observed (b): division-level files, if present ─────────────────────────
good_of_div <- c(`20` = "C20", `23` = "C23", `24` = "C24")
for (cc in c("IT", "ES")) {
  f <- sprintf("prototypes/observed_imports_%s_divisions.csv", cc)
  if (!file.exists(f)) {
    cat(sprintf("\n[%s divisions] %s not found - export it per the header recipe.\n", cc, f))
    next
  }
  od <- read_csv(f, show_col_types = FALSE) |>
    filter(year == 2023) |>
    mutate(good = good_of_div[as.character(cpa_division)]) |>
    group_by(good, NUTS_ID = region_nuts2) |>
    summarise(obs_eur = sum(import_value_eur), .groups = "drop") |>
    group_by(good) |> mutate(obs_share = obs_eur / sum(obs_eur)) |> ungroup()
  mm <- if (cc == "IT") m_it else m_es
  cat(sprintf("\n=== %s: per-good validation (division-level observed) ===\n", cc))
  for (g in goods) {
    d <- inner_join(mm |> filter(good == g), od |> filter(good == g),
                    by = "NUTS_ID")
    cat(sprintf("  %s: n=%d  Spearman %.3f  Pearson %.3f\n", g, nrow(d),
                cor(d$modeled_share, d$obs_share, method = "spearman"),
                cor(d$modeled_share, d$obs_share)))
  }
}

# ── write outputs ───────────────────────────────────────────────────────────
out <- bind_rows(
  m_it |> mutate(country = "IT"),
  m_it_tot |> mutate(country = "IT"),
  m_es |> mutate(country = "ES"),
  m_es |> group_by(NUTS_ID) |>
    summarise(modeled_MEUR = sum(modeled_MEUR), .groups = "drop") |>
    mutate(good = "TOTAL", modeled_share = modeled_MEUR / sum(modeled_MEUR),
           country = "ES")
) |>
  left_join(obs |> mutate(country = "IT", good = "TOTAL"),
            by = c("country", "good", "NUTS_ID"))
write_csv(out, "prototypes/cbam_trade_validation_output.csv")
cat("\nwrote prototypes/cbam_trade_validation_output.csv\n")
