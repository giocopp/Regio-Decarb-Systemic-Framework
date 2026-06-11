# Maps for the geocoded carbon-cost-at-risk TRI: Total Manufacturing (C) choropleths
# and a plant-level point map from the EUTL coordinates. Styling from R/06_visualize.R.
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork)})
source("R/06_visualize.R")
OUT <- "Figures"; dir.create(OUT, showWarnings = FALSE)

tri    <- read.csv("Final data/Risk_data_carbon_cost_PROTOTYPE.csv", stringsAsFactors = FALSE)
layers <- .get_map_layers()
nuts2 <- layers$nuts2 |>                          # recombine Croatia to match the grid (-> HR04)
  mutate(NUTS_ID = ifelse(NUTS_ID %in% c("HR02","HR05","HR06"), "HR04", NUTS_ID)) |>
  group_by(NUTS_ID) |> summarise(geometry = st_union(geometry), .groups = "drop") |> st_make_valid()

C  <- tri |> filter(Sector_ID == "C") |> mutate(Exposure = ifelse(Exposure == 0, NA_real_, Exposure))
mC <- nuts2 |> left_join(C, by = "NUTS_ID") |> st_transform(.crs_lambert)
ttl <- function(t) ggtitle(t) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p1 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Exposure","Exposure", RColorBrewer::brewer.pal(7,"Purples")) + ttl("Exposure")
p2 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Vulnerability","Vulnerability", RColorBrewer::brewer.pal(7,"Blues")) + ttl("Vulnerability")
p3 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Risk_norm","Risk", RColorBrewer::brewer.pal(7,"Reds")) + ttl("Risk")
combo <- (p1 | p2 | p3) + patchwork::plot_annotation(
  title = "Total Manufacturing - carbon-cost-at-risk (geocoded ETS + CBAM, full phase-in)",
  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))
f1 <- file.path(OUT, "Figure_exposure_carboncost_total_manufacturing.png")
ggsave(f1, combo, width = 14, height = 5, dpi = 300, bg = "white"); cat("wrote", f1, "\n")

inst <- read.csv("Initial data/EUTL_euets_info/ets_installations_geocoded.csv", stringsAsFactors = FALSE)
labs <- c("C16-C18"="Paper","C19-C20"="Chemicals/Refining","C23"="Cement/Glass/Ceramics","C24"="Basic Metals")
pts  <- inst |> filter(verified > 0) |> mutate(Sector = labs[Sector_ID]) |>
  st_as_sf(coords = c("lon","lat"), crs = 4326)
pplant <- ggplot() +
  geom_sf(data = layers$europe_bg, fill = "grey92", colour = "grey80", linewidth = 0.15) +
  geom_sf(data = layers$eu_outline, fill = NA, colour = "grey15", linewidth = 0.25) +
  geom_sf(data = pts, aes(size = verified/1e6, colour = Sector), alpha = 0.55) +
  coord_sf(crs = .crs_lambert) +
  scale_size_continuous(name = "Mt CO2 (2023)", range = c(0.2, 7)) +
  scale_colour_brewer(palette = "Set1", name = "Sector") +
  ggtitle(sprintf("EU ETS manufacturing installations geocoded to NUTS-3 (%d emitting plants, 2023)", nrow(pts))) +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11), legend.position = "right",
        plot.background = element_rect(fill = "white", colour = NA))
f2 <- file.path(OUT, "Figure_ets_installations_points.png")
ggsave(f2, pplant, width = 9, height = 8, dpi = 300, bg = "white"); cat("wrote", f2, "\n")
