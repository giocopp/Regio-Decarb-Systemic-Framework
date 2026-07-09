# Maps for the headline Risk index. The choropleth is now a pipeline target
# (figure_risk_maps); only the plant point map is run manually after tar_make().
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork)})
source("R/06_visualize.R")
OUT <- "Figures"; dir.create(OUT, showWarnings = FALSE)

tri    <- read.csv("Final data/Risk_data.csv", stringsAsFactors = FALSE)
layers <- .get_map_layers()   # layer already carries the HR04 recombination

C  <- tri |> filter(Sector_ID == "C") |> mutate(Exposure = ifelse(Exposure == 0, NA_real_, Exposure))
mC <- layers$nuts2 |> left_join(C, by = "NUTS_ID") |> st_transform(.crs_lambert)
ttl <- function(t) ggtitle(t) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
p1 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Exposure","Exposure", RColorBrewer::brewer.pal(7,"Purples")) + ttl("Exposure")
p2 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Vulnerability","Vulnerability", RColorBrewer::brewer.pal(7,"Blues")) + ttl("Vulnerability")
p3 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Risk_norm","Risk", RColorBrewer::brewer.pal(7,"Reds")) + ttl("Risk")
combo <- (p1 | p2 | p3) + patchwork::plot_annotation(
  title = "Total Manufacturing - Risk index (covered carbon: geocoded ETS + CBAM)",
  theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))
f1 <- file.path(OUT, "Figure_risk_total_manufacturing.png")
ggsave(f1, combo, width = 14, height = 5, dpi = 300, bg = "white"); cat("wrote", f1, "\n")

# Plant point map (deck slide 9). Only plants with positive verified emissions
# are drawn (4,028 of 6,154); the four heavy families are coloured, everything
# else (activity-20 combustion attributed via NACE) is grey.
inst  <- read.csv("Initial data/EUTL_euets_info/ets_installations_geocoded.csv", stringsAsFactors = FALSE)
heavy <- c("C24"     = "Basic metals (C24)",
           "C19-C20" = "Chemicals & refining (C19-C20)",
           "C23"     = "Non-metallic minerals (C23)",
           "C16-C18" = "Wood & paper (C16-C18)")
other <- "Other sectors (combustion plants)"
pal   <- c("Basic metals (C24)"             = "#E41A1C",
           "Chemicals & refining (C19-C20)" = "#377EB8",
           "Non-metallic minerals (C23)"    = "#FF7F00",
           "Wood & paper (C16-C18)"         = "#4DAF4A")
pal[other] <- "grey55"
pts <- inst |>
  filter(verified > 0) |>
  mutate(Sector = ifelse(Sector_ID %in% names(heavy), heavy[Sector_ID], other),
         Sector = factor(Sector, levels = names(pal))) |>
  st_as_sf(coords = c("lon","lat"), crs = 4326)
pplant <- ggplot() +
  geom_sf(data = layers$europe_bg, fill = "grey92", colour = "grey80", linewidth = 0.15) +
  geom_sf(data = layers$eu_outline, fill = NA, colour = "grey15", linewidth = 0.25) +
  geom_sf(data = pts, aes(size = verified/1e6, colour = Sector), alpha = 0.55) +
  coord_sf(crs = .crs_lambert) +
  scale_size_continuous(name = "Mt CO2 (2023)", range = c(0.2, 7)) +
  scale_colour_manual(values = pal, name = NULL,
                      guide = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  theme_void(base_size = 10) +
  theme(legend.position = "right",
        plot.background = element_rect(fill = "white", colour = NA))
f2 <- file.path(OUT, "Figure_ets_installations_points.png")
ggsave(f2, pplant, width = 11, height = 7, dpi = 300, bg = "white"); cat("wrote", f2, "\n")
deck_copy <- "../Review/Hazard_Exposure_Slides/ets_installations_2023.png"
if (dir.exists(dirname(deck_copy))) {
  file.copy(f2, deck_copy, overwrite = TRUE); cat("copied to", deck_copy, "\n")
}

# CBAM import-carbon choropleth (deck companion to the ETS plant map): the
# CBAM component has no plant geography — national embodied-import carbon
# downscaled to NUTS-2 by employment shares — so it is mapped as a regional
# surface (Total Manufacturing row, Mt CO2).
cb <- tri |> filter(Sector_ID == "C") |>
  mutate(cbam_Mt = exposure_CBAM / 1e6)
mB <- layers$nuts2 |> left_join(cb, by = "NUTS_ID") |> st_transform(.crs_lambert)
pcbam <- ggplot() +
  geom_sf(data = layers$europe_bg, fill = "grey92", colour = "grey80", linewidth = 0.15) +
  geom_sf(data = mB, aes(fill = cbam_Mt), colour = "grey35", linewidth = 0.1) +
  geom_sf(data = layers$eu_outline, fill = NA, colour = "grey15", linewidth = 0.25) +
  coord_sf(crs = .crs_lambert) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(7, "Oranges"),
                       name = "Mt CO2 (2023)", na.value = "grey92") +
  theme_void(base_size = 10) +
  theme(legend.position = "right",
        plot.background = element_rect(fill = "white", colour = NA))
f3 <- file.path(OUT, "Figure_cbam_import_carbon.png")
ggsave(f3, pcbam, width = 11, height = 7, dpi = 300, bg = "white"); cat("wrote", f3, "\n")
deck_copy3 <- "../Review/Hazard_Exposure_Slides/cbam_import_carbon.png"
if (dir.exists(dirname(deck_copy3))) {
  file.copy(f3, deck_copy3, overwrite = TRUE); cat("copied to", deck_copy3, "\n")
}
