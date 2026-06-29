# income_capacity_prototype.R — regional income / GDP per capita as a
# socioeconomic ADAPTIVE-CAPACITY indicator for Vulnerability (reversed: higher
# income -> lower vulnerability).
#
# Motivation: the regional just-transition literature consistently uses an
# income / fiscal-capacity axis of adaptive capacity — Gasparini et al. (2025)
# use regional taxable income; Vrontisi et al. (2024) income levels; Raimi et al.
# (2022) fiscal capacity — which the current 5-dimension composite (Energy,
# Labour, Technology, Institutions, Diversification) does NOT carry. This is the
# data-feasible, literature-aligned alternative to reviving a Finance dimension
# (no clean NUTS-2 x NACE transition-finance-access series exists; Calcaterra et
# al. 2024 differentiate the cost of capital only by country). See
# LITERATURE_GATHERED.md section H.
#
# STATUS: prototype — NOT wired into the index. Sizes the indicator and its
# regional spread. The development sandbox had no Eurostat egress, so this has
# not been executed; run from "Code and data/":
#   Rscript prototypes/income_capacity_prototype.R
# Confirm the unit code on the first run (EUR_HAB = euro per inhabitant; the
# PPS_* per-inhabitant units give a price-level-adjusted, cross-country-
# comparable alternative and are usually preferable for an EU-wide composite).

suppressMessages({library(restatapi); library(dplyr)})
source("R/utils.R")   # eu27, pick_latest_complete_year

this_yr <- as.integer(format(Sys.Date(), "%Y"))
raw <- get_eurostat_data(
  id          = "nama_10r_2gdp",
  filters     = list(unit = "EUR_HAB"),     # GDP per inhabitant, current EUR
  date_filter = seq(this_yr - 4L, this_yr),
  exact_match = TRUE, label = FALSE
) |>
  as_tibble() |>
  mutate(geo = as.character(geo), values = as.numeric(values))

reg <- raw |> filter(nchar(geo) == 4, substr(geo, 1, 2) %in% eu27)
pick <- pick_latest_complete_year(reg, geo_dim = "geo", value_col = "values",
                                  expected_geos = sort(unique(reg$geo)),
                                  max_years_back = 4L)
gdp <- reg |>
  mutate(.y = as.integer(as.character(time))) |>
  filter(.y == pick$year) |>
  transmute(NUTS_ID = geo, Country_ID = substr(geo, 1, 2), gdp_eur_hab = values)

cat(sprintf("nama_10r_2gdp EUR_HAB, year %d: %d NUTS-2 regions\n",
            pick$year, nrow(gdp)))
cat(sprintf("GDP/cap (EUR): min %s | median %s | max %s\n",
            format(min(gdp$gdp_eur_hab, na.rm = TRUE), big.mark = ","),
            format(median(gdp$gdp_eur_hab, na.rm = TRUE), big.mark = ","),
            format(max(gdp$gdp_eur_hab, na.rm = TRUE), big.mark = ",")))

# As an adaptive-capacity indicator: reversed min-max (higher income -> lower
# vulnerability contribution). Intensive -> no per-employee division.
rng <- range(gdp$gdp_eur_hab, na.rm = TRUE)
gdp <- gdp |>
  mutate(cap01        = (gdp_eur_hab - rng[1]) / (rng[2] - rng[1]),
         vuln_contrib = 1 - cap01)

cat("\nLowest income -> HIGHEST vulnerability contribution:\n")
print(gdp |> arrange(desc(vuln_contrib)) |> head(8) |>
        transmute(NUTS_ID, gdp_eur_hab, vuln_contrib = round(vuln_contrib, 2)))
cat("\nHighest income -> LOWEST vulnerability contribution:\n")
print(gdp |> arrange(vuln_contrib) |> head(8) |>
        transmute(NUTS_ID, gdp_eur_hab, vuln_contrib = round(vuln_contrib, 2)))

cat("\nTo wire as an indicator: replicate across the empl_weights (region x sector)",
    "\ngrid, Indicator = 'Regional_Income', reversed orientation, intensive (no",
    "\nper-employee). Candidate homes: a new 'Capacity' dimension, or fold into",
    "\nLabour alongside the skills/unemployment indicators.\n")
