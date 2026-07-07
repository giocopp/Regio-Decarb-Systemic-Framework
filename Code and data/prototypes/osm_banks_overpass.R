# osm_banks_overpass.R — fallback extractor for the finance-access prototype.
# Fetches amenity=bank elements WITH coordinates per country from Overpass
# (used 2026-07-04 because api.ohsome.org returned HTTP 503 on all count
# endpoints), then assigns them to NUTS-2 by point-in-polygon (gisco 2021,
# same machinery as the ETS geocoding). Writes the cache the main prototype
# consumes (prototypes/osm_bank_counts_nuts2.csv).
# Caveat vs ohsome: Overpass serves the LIVE database — no time pin. The
# retrieval date is recorded in the snapshot column; for the final paper
# run, redo the counts against a pinned ohsome timestamp or a dated
# Geofabrik extract. Cross-check available: LU Overpass 205 vs ohsome 206.
# OSM data (c) OpenStreetMap contributors, ODbL.
suppressMessages({library(dplyr); library(readr); library(sf)
                  library(giscoR); library(jsonlite)})

iso_of <- c(AT="AT",BE="BE",BG="BG",CY="CY",CZ="CZ",DE="DE",DK="DK",EE="EE",
            EL="GR",ES="ES",FI="FI",FR="FR",HR="HR",HU="HU",IE="IE",IT="IT",
            LT="LT",LU="LU",LV="LV",MT="MT",NL="NL",PL="PL",PT="PT",RO="RO",
            SE="SE",SI="SI",SK="SK")
dir.create("prototypes/overpass_cache", showWarnings = FALSE)

fetch_cc <- function(cc) {
  f <- sprintf("prototypes/overpass_cache/banks_%s.csv", cc)
  if (file.exists(f)) return(read_csv(f, show_col_types = FALSE))
  q <- sprintf(paste0('[out:json][timeout:300];area["ISO3166-1"="%s"]',
                      '[admin_level=2]->.a;nwr["amenity"="bank"](area.a);',
                      'out center qt;'), iso_of[[cc]])
  mirrors <- c("https://overpass-api.de/api/interpreter",
               "https://overpass.kumi.systems/api/interpreter")
  for (try in 1:6) {
    out <- system2("curl", c("-s", "--max-time", "360",
             "-A", shQuote("CMCC-decarb-risk-index/1.0 (research; contact: coppola.giorgio99@gmail.com)"),
             "-X", "POST", shQuote(mirrors[(try - 1) %% 2 + 1]),
             "--data-urlencode", shQuote(paste0("data=", q))), stdout = TRUE)
    j <- tryCatch(fromJSON(paste(out, collapse = ""), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (!is.null(j$elements)) {
      pts <- bind_rows(lapply(j$elements, function(e) {
        la <- if (!is.null(e$lat)) e$lat else e$center$lat
        lo <- if (!is.null(e$lon)) e$lon else e$center$lon
        if (is.null(la) || is.null(lo)) return(NULL)
        tibble(lat = la, lon = lo)
      }))
      write_csv(pts, f)
      return(pts)
    }
    cat("  retry", try, cc, "\n"); Sys.sleep(45 * try)
  }
  stop("Overpass failed for ", cc)
}

n2 <- gisco_get_nuts(nuts_level = "2", year = "2021", resolution = "10") |>
  filter(substr(NUTS_ID, 1, 2) %in% names(iso_of)) |>
  select(NUTS_ID) |> st_transform(3035)

res <- list()
for (cc in names(iso_of)) {
  pts <- fetch_cc(cc)
  cat(cc, nrow(pts), "banks fetched\n")
  if (nrow(pts) == 0) next
  p <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326) |> st_transform(3035)
  hit <- st_join(p, n2, join = st_intersects)
  cnt <- hit |> st_drop_geometry() |> filter(!is.na(NUTS_ID)) |> count(NUTS_ID)
  dropped <- sum(is.na(hit$NUTS_ID))
  if (dropped > 0) cat("  ", cc, ":", dropped, "points outside EU NUTS-2 (dropped)\n")
  res[[cc]] <- cnt
  Sys.sleep(2)
}
osm <- bind_rows(res) |> group_by(NUTS_ID) |>
  summarise(osm_banks = sum(n), .groups = "drop")
hr <- osm |> filter(NUTS_ID %in% c("HR02","HR05","HR06")) |>
  summarise(NUTS_ID = "HR04", osm_banks = sum(osm_banks))
osm <- osm |> filter(!NUTS_ID %in% c("HR02","HR05","HR06")) |> bind_rows(hr) |>
  mutate(snapshot = paste0("overpass-live-", Sys.Date()))
write_csv(osm, "prototypes/osm_bank_counts_nuts2.csv")
cat("wrote prototypes/osm_bank_counts_nuts2.csv -", nrow(osm), "regions,",
    sum(osm$osm_banks), "banks\n")
