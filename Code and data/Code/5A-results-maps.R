pkgs <- c("tidyverse", "readxl", "sf", "giscoR", "RColorBrewer",
          "classInt", "ggnewscale", "patchwork", "here")
if (any(!pkgs %in% installed.packages()[, "Package"])) {
  install.packages(setdiff(pkgs, rownames(installed.packages())), dep = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

###

data <- readxl::read_xlsx("Code and data/Final data/Risk_data.xlsx")

vuln_vars <- c("Vuln_Energy", "Vuln_Labour", "Vuln_Finance",
               "Vuln_Supply_Chain", "Vuln_Technology",
               "Vuln_Institutions", "Vuln_Diversification",
               "Vulnerability", "Exposure", "Risk_norm")

# min / max for each sector 
safe_min <- ~ if (all(is.na(.x))) NA_real_ else min(.x, na.rm = TRUE)
safe_max <- ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)

tri_ranges <- data %>%
  dplyr::group_by(Sector_ID, Sector_Name) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(vuln_vars),
      list(min = safe_min, max = safe_max),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

sct <- c("Total Manufacturing", "Manufacturing of Basic Metal Products")

# Convert Exposure = 0 to NA
data$Exposure[data$Exposure == 0] <- NA

# Load NUTS-2 map and clean
nuts2_raw <- gisco_get_nuts(nuts_level = "2", resolution = "3", year = "2021")
eu_codes <- gisco_get_countries(region = "EU", resolution = "3", year = "2020")$CNTR_ID

nuts2_sf <- nuts2_raw |>
  filter(CNTR_CODE %in% eu_codes & !substr(NUTS_ID, 1, 3) %in% c("FRY", "ES7", "PT2")) |>
  group_by(NUTS_ID) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid()

# Clip map size
clip_bb <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = -10, ymin = 35, xmax = 35, ymax = 72), crs = sf::st_crs(4326)
))

europe_bg <- giscoR::gisco_get_countries(region = "Europe", resolution = "3", year = "2020") %>%
  dplyr::filter(!CNTR_ID %in% c("RU")) %>%                  # drop Russia
  sf::st_crop(clip_bb) %>%                                  # drop Canaries, Azores, etc.
  sf::st_make_valid()

eu_cntr_outline <- giscoR::gisco_get_countries(region = "EU", resolution = "3", year = "2020") %>%
  sf::st_crop(clip_bb) %>%                                  # crop out overseas parts
  sf::st_make_valid()

# Merge TRI data with NUTS-2 map
mapping_sf <- nuts2_sf |>
  left_join(data, by = "NUTS_ID")

# Mapping helpers
crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

nice_scale_dynamic <- function(df, var, var_lbl, brewer_name) {
  range_vals <- range(df[[var]], na.rm = TRUE)
  n_breaks <- 5
  scale_fill_gradientn(
    name    = var_lbl,
    limits  = range_vals,
    colours = rev(RColorBrewer::brewer.pal(7, brewer_name)),  # flipped color scale
    breaks  = pretty(range_vals, n = n_breaks),
    labels  = scales::percent_format(accuracy = 1),
    na.value = "grey85"
  )
}

single_map <- function(df, var, title, colours) {
  ggplot() +
    # background: other European countries in grey
    geom_sf(data = europe_bg, fill = "grey90", colour = "grey80", size = 0.15) +
    # regions: thin borders
    geom_sf(data = df, aes(fill = .data[[var]]), colour = "grey35", size = 0.05) +
    # countries: bold borders on top
    geom_sf(data = eu_cntr_outline, fill = NA, colour = "grey15", size = 0.25) +
    coord_sf(crs = crs_lambert) +
    scale_fill_gradientn(
      name    = title,
      limits  = c(0, 1),
      colours = colours,
      breaks  = seq(0, 1, length.out = 5),
      labels  = scales::percent_format(accuracy = 1),
      na.value = "grey85",
      guide = guide_colorbar(barwidth = 0.3, barheight = 2.0,
                             title.theme = element_text(face = "bold", size = 8))
    ) +
    theme_void(base_size = 9) +
    theme(
      legend.position = "right",
      legend.title    = element_text(face = "bold", size = 8),
      legend.text     = element_text(size = 7)
    )
}

# Save TRI / Exposure / Vulnerability maps (side-by-side)

output_root <- "Code and data/Figures"
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

name_map <- c(
  "Total Manufacturing" = "total manufacturing",
  "Manufacturing of Basic Metal Products" = "basic metals"
)

palettes <- list(Risk = "Reds", EXP = "Purples", VUL = "Blues")

for (s in sct) {
  sub_sf <- mapping_sf |> dplyr::filter(Sector_Name == s) |> sf::st_transform(crs_lambert)
  
  p_exp <- single_map(sub_sf, "Exposure", "Exposure", RColorBrewer::brewer.pal(7, palettes$EXP)) +
    ggtitle("Exposure") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  p_vul <- single_map(sub_sf, "Vulnerability", "Vulnerability", RColorBrewer::brewer.pal(7, palettes$VUL)) +
    ggtitle("Vulnerability") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  p_tri <- single_map(sub_sf, "Risk_norm", "Risk", RColorBrewer::brewer.pal(7, palettes$Risk)) +
    ggtitle("Risk") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  combined <- (p_exp | p_vul | p_tri) +
    plot_annotation(title = s, theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))
  
  outfile3 <- file.path(output_root, paste0("Figure_3_", name_map[[s]], ".png"))
  ggsave(outfile3, combined, width = 14, height = 5, dpi = 600)
}

# ---- Vulnerability dimensions ----
vuln_dims <- c(Energy="Vuln_Energy", Labour="Vuln_Labour",
               `Supply Chain`="Vuln_Supply_Chain", Technology="Vuln_Technology",
               Finance="Vuln_Finance", Institutions="Vuln_Institutions",
               Diversification="Vuln_Diversification")

for (s in sct) {
  sub_sf <- mapping_sf |> dplyr::filter(Sector_Name == s) |> sf::st_transform(crs_lambert)
  
  six_maps <- lapply(names(vuln_dims), function(dim_lab) {
    var <- vuln_dims[[dim_lab]]
    colors <- RColorBrewer::brewer.pal(7, "Blues")
    single_map(sub_sf, var, title = dim_lab, colours = colors)
  })
  
  panel <- wrap_plots(six_maps, ncol = 4) +
    plot_annotation(title = paste("Vulnerability Dimensions –", s),
                    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)))
  
  outfile4 <- file.path(output_root, paste0("Figure_4_", name_map[[s]], ".png"))
  ggsave(outfile4, panel, width = 14, height = 8, dpi = 600)
}
