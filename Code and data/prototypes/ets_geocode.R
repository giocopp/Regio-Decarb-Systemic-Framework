# Geocode EU-ETS manufacturing installations (EUETS.info/Abrell 2024, eutl_2024_202410)
# to NUTS-3/NUTS-2 and aggregate verified emissions + free allocation. Vintage 2023.
suppressMessages({library(dplyr); library(sf); library(tidyr)})

RAW <- "/tmp/eutl_validation"
OUT <- "Initial data/EUTL_euets_info"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
YEAR <- 2023

# activity_id -> sector (codes 21-44, validated against the official activity_type.csv)
sec_of <- c(`35`="C16-C18",`36`="C16-C18",
            `21`="C19-C20",`22`="C19-C20",`37`="C19-C20",`38`="C19-C20",`39`="C19-C20",
            `40`="C19-C20",`41`="C19-C20",`42`="C19-C20",`43`="C19-C20",`44`="C19-C20",
            `29`="C23",`30`="C23",`31`="C23",`32`="C23",`33`="C23",`34`="C23",
            `23`="C24",`24`="C24",`25`="C24",`26`="C24",`27`="C24",`28`="C24")
mfg  <- as.integer(names(sec_of))
eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","HR","HU","IE","IT",
          "LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","GR")

inst <- read.csv(file.path(RAW, "installation.csv"), stringsAsFactors = FALSE)
comp <- read.csv(file.path(RAW, "compliance.csv"),  stringsAsFactors = FALSE)

im <- inst |>
  filter(activity_id %in% mfg, country_id %in% eu27,
         !is.na(latitudeGoogle), !is.na(longitudeGoogle)) |>
  mutate(Sector_ID  = sec_of[as.character(activity_id)],
         Country_ID = ifelse(country_id == "GR", "EL", country_id)) |>
  select(id, Country_ID, activity_id, Sector_ID, nace_id, latitudeGoogle, longitudeGoogle)

n3 <- giscoR::gisco_get_nuts(nuts_level = "3", year = "2021", resolution = "03") |>
  st_transform(3035) |> select(NUTS3_ID = NUTS_ID)
pts <- st_as_sf(im, coords = c("longitudeGoogle","latitudeGoogle"), crs = 4326, remove = FALSE) |> st_transform(3035)
j <- st_join(pts, n3, join = st_within)
miss <- is.na(j$NUTS3_ID)
if (any(miss)) {                                  # nearest NUTS-3 within 10 km for coastal misses
  nn <- st_nearest_feature(j[miss, ], n3)
  d  <- as.numeric(st_distance(j[miss, ], n3[nn, ], by_element = TRUE))
  j$NUTS3_ID[miss] <- ifelse(d <= 10000, n3$NUTS3_ID[nn], NA)
}
g <- j |> st_drop_geometry() |>
  mutate(NUTS_ID = substr(NUTS3_ID, 1, 4),
         NUTS_ID = ifelse(NUTS_ID %in% c("HR02","HR05","HR06"), "HR04", NUTS_ID))

cat("manufacturing installations (EU27, with coords):", nrow(im), "\n")
cat("matched to a NUTS-3:", sum(!is.na(g$NUTS3_ID)),
    sprintf("(%.1f%%)", 100*mean(!is.na(g$NUTS3_ID))), "| unmatched:", sum(is.na(g$NUTS3_ID)), "\n")

cy <- comp |> filter(year == YEAR) |> transmute(id = installation_id, verified, allocatedFree)
gv <- g |> left_join(cy, by = "id") |>
  mutate(verified = coalesce(verified, 0), allocatedFree = coalesce(allocatedFree, 0))

write.csv(gv |> select(id, Country_ID, activity_id, Sector_ID, nace_id,
                       lat = latitudeGoogle, lon = longitudeGoogle,
                       NUTS3_ID, NUTS_ID, verified, allocatedFree),
          file.path(OUT, "ets_installations_geocoded.csv"), row.names = FALSE)

agg2 <- gv |> filter(!is.na(NUTS_ID)) |>
  group_by(Country_ID, NUTS_ID, Sector_ID) |>
  summarise(ets_emis_t = sum(verified), alloc_free_t = sum(allocatedFree), .groups = "drop")
write.csv(agg2, file.path(OUT, "ets_nuts2_sector.csv"), row.names = FALSE)

agg3 <- gv |> filter(!is.na(NUTS3_ID)) |>
  group_by(Country_ID, NUTS3_ID, NUTS_ID, Sector_ID) |>
  summarise(ets_emis_t = sum(verified), alloc_free_t = sum(allocatedFree), .groups = "drop")
write.csv(agg3, file.path(OUT, "ets_nuts3_sector.csv"), row.names = FALSE)

fa <- gv |> group_by(Country_ID, Sector_ID) |>
  summarise(verified = sum(verified), allocatedFree = sum(allocatedFree), .groups = "drop") |>
  mutate(free_alloc_share = ifelse(verified > 0, pmin(allocatedFree/verified, 1), NA_real_))
write.csv(fa, file.path(OUT, "ets_country_sector_freealloc.csv"), row.names = FALSE)

cat("NUTS-2xsector cells:", nrow(agg2), "| NUTS-3xsector cells:", nrow(agg3),
    "| NUTS-2 regions:", n_distinct(agg2$NUTS_ID), "| NUTS-3 regions:", n_distinct(agg3$NUTS3_ID), "\n")
cat("EU27 verified total:", round(sum(agg2$ets_emis_t)/1e6,1), "Mt\n")
print(as.data.frame(agg2 |> group_by(Sector_ID) |> summarise(Mt = round(sum(ets_emis_t)/1e6,1)) |> arrange(desc(Mt))))
