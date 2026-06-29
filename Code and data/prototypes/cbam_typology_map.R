# cbam_typology_map.R — NUTS-2 map of the directional CBAM typology.
# Regions are located by their geocoded ETS plants (ets_nuts2_sector.csv) and
# coloured by the country x covered-good type from cbam_typology_prototype.R.
# Categorical join (no employment-share downscaling). Run from "Code and data/".

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(sf); library(giscoR)})

reg <- read_csv("prototypes/cbam_typology_region.csv", show_col_types = FALSE)

sec_lab <- c("C19-C20" = "Chemicals & refining (C19-C20)",
             "C23"     = "Cement & minerals (C23)",
             "C24"     = "Basic metals (C24)")
short <- c("Home-market, import-protected (CBAM shields)" = "Shielded (import-protected)",
           "Home-market, low-trade (CBAM ~neutral)"       = "Neutral (low-trade)",
           "Export-exposed (CBAM no help abroad)"         = "Export-exposed",
           "Net importer (CBAM = cost)"                    = "Net importer (cost)")
pal <- c("Shielded (import-protected)" = "#2ca25f",
         "Neutral (low-trade)"         = "#fec44f",
         "Export-exposed"              = "#de2d26",
         "Net importer (cost)"         = "#756bb1")
reg <- reg |> mutate(type_s = factor(short[type], levels = names(pal)),
                     sec_f  = factor(sec_lab[Sector_ID], levels = sec_lab))

eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
          "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")
geo <- gisco_get_nuts(year = "2021", nuts_level = 2, resolution = "20") |>
  filter(CNTR_CODE %in% eu27) |> select(NUTS_ID, geometry)

# base (all NUTS-2) x 3 sectors, then overlay typed producing regions
base <- tidyr::expand_grid(NUTS_ID = geo$NUTS_ID, sec_f = factor(sec_lab, levels = sec_lab)) |>
  left_join(geo, by = "NUTS_ID") |> st_as_sf()
typed <- geo |> inner_join(reg, by = "NUTS_ID")

p <- ggplot() +
  geom_sf(data = base, fill = "grey92", colour = "white", linewidth = 0.05) +
  geom_sf(data = typed, aes(fill = type_s), colour = "white", linewidth = 0.05) +
  facet_wrap(~ sec_f, nrow = 1) +
  scale_fill_manual(values = pal, drop = FALSE, name = "CBAM net effect on producer") +
  coord_sf(xlim = c(-12, 34), ylim = c(34, 71), expand = FALSE) +
  labs(title = "Directional CBAM typology by covered good (regions located by ETS plants)",
       subtitle = "Grey = no covered-good ETS production in that region. Country x good type from FIGARO 2023 trade flows.",
       caption = "Prototype — directional (sign), not a euro quantification. C19-C20 panel includes refineries (not a CBAM good).") +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

out <- "../Review/Hazard_Exposure_Slides/cbam_typology_map.png"
ggsave(out, p, width = 13, height = 5.6, dpi = 300, bg = "white")
cat("wrote", out, "\n")
