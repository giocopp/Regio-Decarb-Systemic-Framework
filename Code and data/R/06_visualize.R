# ── 06_visualize.R ── Maps and radar charts for TRI results ───────
# Produces Figures 3-6 for the paper
# Input:  risk_data tibble (from build_risk_data, R/exposure.R), output dir
# Output: PNG files saved to output_dir; returns character vector of file paths


# ── Map helpers (internal) ────────────────────────────────────────

#' Lambert Azimuthal Equal-Area projection for Europe
.crs_lambert <- paste0(

  "+proj=laea +lat_0=52 +lon_0=10 ",
  "+x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
)

#' Bounding box to clip out overseas territories
.clip_bbox <- function() {
  sf::st_as_sfc(
    sf::st_bbox(c(xmin = -10, ymin = 35, xmax = 35, ymax = 72),
                crs = sf::st_crs(4326))
  )
}

#' Build a single choropleth map panel
.single_map <- function(df, europe_bg, eu_outline, var, title, colours) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = europe_bg,
                     fill = "grey90", colour = "grey80", linewidth = 0.15) +
    ggplot2::geom_sf(data = df,
                     ggplot2::aes(fill = .data[[var]]),
                     colour = "grey35", linewidth = 0.05) +
    ggplot2::geom_sf(data = eu_outline,
                     fill = NA, colour = "grey15", linewidth = 0.25) +
    ggplot2::coord_sf(crs = .crs_lambert) +
    ggplot2::scale_fill_gradientn(
      name     = title,
      limits   = c(0, 1),
      colours  = colours,
      breaks   = seq(0, 1, length.out = 5),
      labels   = scales::percent_format(accuracy = 1),
      na.value = "grey85",
      guide    = ggplot2::guide_colorbar(
        barwidth  = 0.3, barheight = 2.0,
        title.theme = ggplot2::element_text(face = "bold", size = 8)
      )
    ) +
    ggplot2::theme_void(base_size = 9) +
    ggplot2::theme(
      legend.position = "right",
      legend.title    = ggplot2::element_text(face = "bold", size = 8),
      legend.text     = ggplot2::element_text(size = 7)
    )
}

#' Download and prepare map layers (NUTS-2, EU outlines, Europe background)
.get_map_layers <- function() {
  nuts2_raw <- giscoR::gisco_get_nuts(
    nuts_level = "2", resolution = "3", year = "2021"
  )
  eu_codes <- giscoR::gisco_get_countries(
    region = "EU", resolution = "3", year = "2020"
  )$CNTR_ID

  clip_bb <- .clip_bbox()

  # Recombine Croatia to the index grid (HR02+HR05+HR06 -> HR04, mirroring
  # 03_reshape.R) — the giscoR layer is NUTS-2021, the data carry HR04.
  nuts2_sf <- nuts2_raw |>
    dplyr::filter(
      CNTR_CODE %in% eu_codes,
      !substr(NUTS_ID, 1, 3) %in% c("FRY", "ES7", "PT2")
    ) |>
    dplyr::mutate(NUTS_ID = ifelse(NUTS_ID %in% c("HR02", "HR05", "HR06"),
                                   "HR04", NUTS_ID)) |>
    dplyr::group_by(NUTS_ID) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_make_valid()

  europe_bg <- giscoR::gisco_get_countries(
    region = "Europe", resolution = "3", year = "2020"
  ) |>
    dplyr::filter(!CNTR_ID %in% c("RU")) |>
    sf::st_crop(clip_bb) |>
    sf::st_make_valid()

  eu_outline <- giscoR::gisco_get_countries(
    region = "EU", resolution = "3", year = "2020"
  ) |>
    sf::st_crop(clip_bb) |>
    sf::st_make_valid()

  list(nuts2 = nuts2_sf, europe_bg = europe_bg, eu_outline = eu_outline)
}


# ══════════════════════════════════════════════════════════════════
# Public functions
# ══════════════════════════════════════════════════════════════════

#' Create TRI maps (Figures 3 and 4)
#'
#' @param risk_data Tibble from build_risk_data()
#' @param output_dir Character path for saving PNGs
#' @return Character vector of saved file paths (invisibly)
plot_tri_maps <- function(risk_data, output_dir,
                          sectors = c("Total Manufacturing",
                                      "Manufacturing of Basic Metal Products")) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Download shapefiles
  layers <- .get_map_layers()

  # Replace zero Exposure with NA for mapping
  risk_data <- risk_data |>
    dplyr::mutate(Exposure = dplyr::if_else(Exposure == 0, NA_real_, Exposure))

  # Merge risk data onto NUTS-2 geometries
  mapping_sf <- layers$nuts2 |>
    dplyr::left_join(risk_data, by = "NUTS_ID")

  # Derive filename slugs from sector names
  name_map <- setNames(
    tolower(gsub("[[:space:]]+", "_", sectors)),
    sectors
  )

  # Palettes
  pal_exp  <- RColorBrewer::brewer.pal(7, "Purples")
  pal_vul  <- RColorBrewer::brewer.pal(7, "Blues")
  pal_risk <- RColorBrewer::brewer.pal(7, "Reds")

  saved <- character()

  # ── Figure 3: Exposure / Vulnerability / Risk side-by-side ──────
  for (s in sectors) {
    sub_sf <- mapping_sf |>
      dplyr::filter(Sector_Name == s) |>
      sf::st_transform(.crs_lambert)

    p_exp <- .single_map(sub_sf, layers$europe_bg, layers$eu_outline,
                         "Exposure", "Exposure", pal_exp) +
      ggplot2::ggtitle("Exposure") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

    p_vul <- .single_map(sub_sf, layers$europe_bg, layers$eu_outline,
                         "Vulnerability", "Vulnerability", pal_vul) +
      ggplot2::ggtitle("Vulnerability") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

    p_tri <- .single_map(sub_sf, layers$europe_bg, layers$eu_outline,
                         "Risk_norm", "Risk", pal_risk) +
      ggplot2::ggtitle("Risk") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

    combined <- (p_exp | p_vul | p_tri) +
      patchwork::plot_annotation(
        title = s,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
        )
      )

    outfile <- file.path(output_dir,
                         paste0("Figure_3_", name_map[[s]], ".png"))
    ggplot2::ggsave(outfile, combined, width = 14, height = 5, dpi = 600)
    saved <- c(saved, outfile)
  }

  # ── Figure 4: vulnerability dimension panels (4 dims since 2026-07-03) ──
  vuln_dims <- c(
    Energy       = "Vuln_Energy",
    Labour       = "Vuln_Labour",
    Technology   = "Vuln_Technology",
    Institutions = "Vuln_Institutions"
  )
  vuln_dims <- vuln_dims[vuln_dims %in% names(mapping_sf)]

  pal_dim <- RColorBrewer::brewer.pal(6, "Blues")

  for (s in sectors) {
    sub_sf <- mapping_sf |>
      dplyr::filter(Sector_Name == s) |>
      sf::st_transform(.crs_lambert)

    dim_maps <- purrr::map(names(vuln_dims), function(dim_lab) {
      .single_map(sub_sf, layers$europe_bg, layers$eu_outline,
                  vuln_dims[[dim_lab]], dim_lab, pal_dim)
    })

    panel <- patchwork::wrap_plots(dim_maps, ncol = 3) +
      patchwork::plot_annotation(
        title = paste("Vulnerability Dimensions \u2013", s),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5)
        )
      )

    outfile <- file.path(output_dir,
                         paste0("Figure_4_", name_map[[s]], ".png"))
    ggplot2::ggsave(outfile, panel, width = 14, height = 8, dpi = 600)
    saved <- c(saved, outfile)
  }

  invisible(saved)
}


# ── Radar chart helpers (internal) ────────────────────────────────

#' Draw one radar chart via fmsb::radarchart
.plot_one_radar <- function(row_data, radar_vars, radar_labels,
                            title_label, color) {
  vals <- as.numeric(as.data.frame(row_data)[, radar_vars])
  chart_data <- rbind(
    rep(1, length(vals)),
    rep(0, length(vals)),
    vals
  )
  colnames(chart_data) <- radar_labels
  chart_data <- as.data.frame(chart_data)

  fmsb::radarchart(
    chart_data,
    axistype    = 1,
    pcol        = color,
    pfcol       = scales::alpha(color, 0.40),
    plwd        = 2,
    cglcol      = "grey70",
    cglty       = 1,
    cglwd       = 0.8,
    axislabcol  = "grey40",
    caxislabels = seq(0, 1, 0.2),
    vlcex       = 1.3,
    title       = NULL
  )
  title(main = title_label, cex.main = 1.4, font.main = 2)
}

#' Select highest + lowest risk regions for given countries and sector
.select_extremes <- function(data, sector_name, country_ids) {
  high <- data |>
    dplyr::filter(Sector_Name == sector_name,
                  Country_ID %in% country_ids,
                  !is.na(Risk_norm)) |>
    dplyr::group_by(Country_ID) |>
    dplyr::slice_max(Risk_norm, n = 1, with_ties = FALSE) |>
    dplyr::mutate(Risk_Position = "Highest")

  low <- data |>
    dplyr::filter(Sector_Name == sector_name,
                  Country_ID %in% country_ids,
                  !is.na(Risk_norm)) |>
    dplyr::group_by(Country_ID) |>
    dplyr::slice_min(Risk_norm, n = 1, with_ties = FALSE) |>
    dplyr::mutate(Risk_Position = "Lowest")

  dplyr::bind_rows(high, low) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      factor(Country_ID, levels = country_ids),
      dplyr::desc(Risk_Position == "Highest")
    )
}


#' Create radar charts (Figures 5 and 6)
#'
#' @param risk_data Tibble from build_risk_data()
#' @param output_dir Character path for saving PNGs
#' @return Character vector of saved file paths (invisibly)
plot_radar_charts <- function(risk_data, output_dir,
                              sectors = c("Total Manufacturing",
                                          "Manufacturing of Basic Metal Products"),
                              countries = c("DE", "EL")) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  radar_vars <- c(
    "Exposure",
    "Vuln_Energy", "Vuln_Labour",
    "Vuln_Technology", "Vuln_Institutions"
  )
  radar_labels <- c(
    "Exposure",
    "Energy", "Labour",
    "Technology", "Institutions"
  )

  region_cols <- rep(c("firebrick", "darkolivegreen4"), length(countries))

  saved <- character()

  for (idx in seq_along(sectors)) {
    s <- sectors[idx]
    sel <- .select_extremes(risk_data, s, countries)
    if (nrow(sel) == 0) next

    fig_num <- idx + 4L
    outfile <- file.path(output_dir, paste0("Figure_", fig_num, ".png"))
    n_panels <- nrow(sel)
    ncols <- min(n_panels, 2L)
    nrows <- ceiling(n_panels / ncols)

    grDevices::png(outfile, width = 1800, height = 1800, res = 150)
    graphics::par(mfrow = c(nrows, ncols))
    for (i in seq_len(n_panels)) {
      r <- sel[i, ]
      caption <- sprintf(
        "%s (%s)\n%s\n%s risk (Risk = %.2f)",
        r$NUTS_Name, r$Country_ID, s, r$Risk_Position, r$Risk_norm
      )
      .plot_one_radar(r, radar_vars, radar_labels, caption,
                      region_cols[((i - 1L) %% length(region_cols)) + 1L])
    }
    grDevices::dev.off()
    saved <- c(saved, outfile)
  }

  invisible(saved)
}


#' Headline Risk-index three-panel map (Total Manufacturing):
#' Exposure | Vulnerability | Risk
#'
#' @param risk_data Tibble from build_risk_data()
#' @param output_dir Character path for saving the PNG
#' @return Path of the saved file
plot_risk_maps <- function(risk_data, output_dir) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  layers <- .get_map_layers()

  C_rows <- risk_data |>
    dplyr::filter(Sector_ID == "C") |>
    dplyr::mutate(Exposure = ifelse(Exposure == 0, NA_real_, Exposure))
  mC <- layers$nuts2 |>
    dplyr::left_join(C_rows, by = "NUTS_ID") |>
    sf::st_transform(.crs_lambert)

  ttl <- function(t) list(
    ggplot2::ggtitle(t),
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      face = "bold"))
  )
  p1 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Exposure",
                    "Exposure", RColorBrewer::brewer.pal(7, "Purples")) +
    ttl("Exposure")
  p2 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Vulnerability",
                    "Vulnerability", RColorBrewer::brewer.pal(7, "Blues")) +
    ttl("Vulnerability")
  p3 <- .single_map(mC, layers$europe_bg, layers$eu_outline, "Risk_norm",
                    "Risk", RColorBrewer::brewer.pal(7, "Reds")) +
    ttl("Risk")

  combo <- (p1 | p2 | p3) + patchwork::plot_annotation(
    title = paste("Total Manufacturing - Risk index",
                  "(covered carbon: geocoded ETS + CBAM)"),
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold",
                                         hjust = 0.5))
  )

  outfile <- file.path(output_dir,
                       "Figure_risk_total_manufacturing.png")
  ggplot2::ggsave(outfile, combo, width = 14, height = 5, dpi = 600,
                  bg = "white")
  outfile
}
