# Maps for the headline Risk index. The choropleth is now a pipeline target
# (figure_risk_maps); only the plant point map is run manually after tar_make().
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork)})
source("R/06_visualize.R")
OUT <- "Figures"; dir.create(OUT, showWarnings = FALSE)

tri    <- read.csv("Final data/Risk_data.csv", stringsAsFactors = FALSE)
layers <- .get_map_layers()   # layer already carries the HR04 recombination

# Plant point map + CBAM choropleth (deck slides). Parametrised by sector:
# sector_id = NULL -> all manufacturing (deck "Total"); "C24" -> basic metals.
# Only plants with positive verified emissions are drawn; the CBAM component
# has no plant geography (employment-downscaled) and is mapped as a surface.
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

build_deck_maps <- function(sector_id = NULL, suffix = "") {
  ins <- inst |> filter(verified > 0)
  if (!is.null(sector_id)) ins <- ins |> filter(Sector_ID == sector_id)
  pts <- ins |>
    mutate(Sector = ifelse(Sector_ID %in% names(heavy), heavy[Sector_ID], other),
           Sector = factor(Sector, levels = names(pal))) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  cat(suffix, "plants plotted:", nrow(pts), "| Mt:",
      round(sum(pts$verified) / 1e6, 1), "\n")
  pplant <- ggplot() +
    geom_sf(data = layers$europe_bg, fill = "grey92", colour = "grey80", linewidth = 0.15) +
    geom_sf(data = layers$eu_outline, fill = NA, colour = "grey15", linewidth = 0.25) +
    geom_sf(data = pts, aes(size = verified/1e6, colour = Sector), alpha = 0.55) +
    coord_sf(crs = .crs_lambert) +
    scale_size_continuous(name = "Mt CO2 (2023)", range = c(0.2, 7)) +
    scale_colour_manual(values = pal[levels(droplevels(pts$Sector))], name = NULL,
                        guide = guide_legend(override.aes = list(size = 4, alpha = 1))) +
    theme_void(base_size = 10) +
    theme(legend.position = "right",
          plot.background = element_rect(fill = "white", colour = NA))
  f2 <- file.path(OUT, paste0("Figure_ets_installations_points", suffix, ".png"))
  ggsave(f2, pplant, width = 11, height = 7, dpi = 300, bg = "white"); cat("wrote", f2, "\n")
  dc <- paste0("../Review/Hazard_Exposure_Slides/ets_installations_2023", suffix, ".png")
  if (dir.exists(dirname(dc))) file.copy(f2, dc, overwrite = TRUE)

  sec_row <- if (is.null(sector_id)) "C" else sector_id
  cb <- tri |> filter(Sector_ID == sec_row) |> mutate(cbam_Mt = exposure_CBAM / 1e6)
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
  f3 <- file.path(OUT, paste0("Figure_cbam_import_carbon", suffix, ".png"))
  ggsave(f3, pcbam, width = 11, height = 7, dpi = 300, bg = "white"); cat("wrote", f3, "\n")
  dc3 <- paste0("../Review/Hazard_Exposure_Slides/cbam_import_carbon", suffix, ".png")
  if (dir.exists(dirname(dc3))) file.copy(f3, dc3, overwrite = TRUE)
}

build_deck_maps(NULL, "")
build_deck_maps("C24", "_C24")
