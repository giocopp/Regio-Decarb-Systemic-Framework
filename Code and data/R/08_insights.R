# ── 08_insights.R ── Analytical outputs from risk_data ────────────────────────
#
# Builds tables and figures that exploit the existing risk_data more deeply:
#   build_top_bottom_table()    -- 11 sectors x (top-5 + bottom-5) NUTS-2
#   plot_quadrant()             -- Exposure x Vulnerability scatter w/ archetypes
#   plot_within_country_var()   -- boxplots of regional risk spread per country
#   plot_sector_cluster_maps()  -- employment concentration x risk score overlay

# ── Constants ────────────────────────────────────────────────────────────────
VULN_DIMS <- c("Vuln_Energy", "Vuln_Labour", "Vuln_Technology",
               "Vuln_Institutions")

VULN_PRETTY <- c(
  Vuln_Energy       = "Energy",
  Vuln_Labour       = "Labour",
  Vuln_Technology   = "Technology",
  Vuln_Institutions = "Institutions"
)


# ── 1. Top/Bottom region tables per sector ──────────────────────────────────

#' Build top-5 + bottom-5 regions per sector with score drivers
#'
#' For each of the 11 sectors, returns the 5 highest-Risk_norm and 5
#' lowest-Risk_norm NUTS-2 regions, with composite scores and a textual
#' breakdown of which sub-indicator drives the score.
#'
#' Driver logic:
#'   Top_Exposure_Driver = which covered-carbon leg dominates the cell's
#'     exposure_total: "ETS" (geocoded plant emissions) or "CBAM"
#'     (embodied import carbon)
#'   Top_Vulnerability_Driver = vulnerability dimension with highest score
#'   Risk_Type categorises whether Exposure or Vulnerability is the bigger
#'     contributor (Exposure > Vulnerability => "Exposure-driven", etc.)
#'
#' @param risk_data Tibble from build_risk_data()
#' @param k Integer, number of top and bottom regions per sector (default 5)
#' @return Tibble with one row per (sector, rank position)
build_top_bottom_table <- function(risk_data, k = 5L) {

  df <- risk_data |>
    dplyr::filter(!is.na(Risk_norm)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Top_Exposure_Driver = dplyr::if_else(exposure_ETS >= exposure_CBAM,
                                           "ETS", "CBAM"),
      Top_Vulnerability_Driver = {
        v <- dplyr::c_across(dplyr::all_of(VULN_DIMS))
        names_v <- VULN_DIMS
        if (all(is.na(v))) NA_character_
        else VULN_PRETTY[names_v[which.max(v)]]
      },
      Risk_Type = dplyr::case_when(
        is.na(Exposure) | is.na(Vulnerability)        ~ NA_character_,
        Exposure > Vulnerability + 0.1                ~ "Exposure-driven",
        Vulnerability > Exposure + 0.1                ~ "Vulnerability-driven",
        TRUE                                          ~ "Balanced"
      )
    ) |>
    dplyr::ungroup()

  top_rows <- df |>
    dplyr::group_by(Sector_ID, Sector_Name) |>
    dplyr::slice_max(Risk_norm, n = k, with_ties = FALSE) |>
    dplyr::mutate(Rank_Position = paste0("Top_", dplyr::row_number())) |>
    dplyr::ungroup()

  bot_rows <- df |>
    dplyr::group_by(Sector_ID, Sector_Name) |>
    dplyr::slice_min(Risk_norm, n = k, with_ties = FALSE) |>
    dplyr::mutate(Rank_Position = paste0("Bottom_", dplyr::row_number())) |>
    dplyr::ungroup()

  dplyr::bind_rows(top_rows, bot_rows) |>
    dplyr::transmute(
      Sector_ID, Sector_Name, Rank_Position,
      NUTS_ID, NUTS_Name, Country_ID,
      Risk_norm = round(Risk_norm, 3),
      Risk_Band,
      Exposure = round(Exposure, 3),
      Vulnerability = round(Vulnerability, 3),
      Risk_Type,
      Top_Exposure_Driver,
      Top_Vulnerability_Driver,
      exposure_ETS  = round(exposure_ETS),
      exposure_CBAM = round(exposure_CBAM),
      Scope1 = round(Scope1_Emissions, 2),
      Scope2 = round(Scope2_Emissions, 2),
      Scope3 = round(Scope3_Emissions, 2)
    ) |>
    dplyr::arrange(Sector_ID,
                   factor(Rank_Position,
                          levels = c(paste0("Top_", seq_len(k)),
                                     paste0("Bottom_", seq_len(k))))) |>
    tibble::as_tibble()
}


# ── 2. Exposure x Vulnerability quadrant plot ──────────────────────────────

#' Quadrant plot: Exposure (x) vs Vulnerability (y) with archetype labels
#'
#' For one or more sectors, plots each NUTS-2 region as a point, splits the
#' plane at the median Exposure and median Vulnerability, and labels the four
#' quadrants:
#'   Top-right  : high Exposure, high Vulnerability  ("clear losers")
#'   Top-left   : low Exposure,  high Vulnerability  ("vulnerable but cleaner")
#'   Bottom-right: high Exposure, low Vulnerability  ("transition leaders")
#'   Bottom-left: low Exposure, low Vulnerability    ("untouched")
#' Top-3 highest-risk regions per sector are labelled with their NUTS_ID.
#'
#' @return Path to saved PNG
plot_quadrant <- function(risk_data, output_dir = "Figures",
                          sectors = c("C", "C24", "C25+C28", "C29-C30",
                                      "C19-C20", "C23")) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # !is.na(Risk_norm) drops Zero-Risk cells (Exposure = 0), matching the
  # reporting convention in METHODOLOGY §12.
  df <- risk_data |>
    dplyr::filter(Sector_ID %in% sectors, !is.na(Risk_norm),
                  !is.na(Exposure), !is.na(Vulnerability)) |>
    dplyr::mutate(Sector_label = sector_name_map[Sector_ID])

  # Top-3 labelled per sector with NUTS_Name
  labels_df <- df |>
    dplyr::group_by(Sector_label) |>
    dplyr::slice_max(Risk_norm, n = 3, with_ties = FALSE) |>
    dplyr::ungroup()

  medians_df <- df |>
    dplyr::group_by(Sector_label) |>
    dplyr::summarise(med_exp = stats::median(Exposure, na.rm = TRUE),
                     med_vul = stats::median(Vulnerability, na.rm = TRUE),
                     .groups = "drop")

  p <- ggplot2::ggplot(df,
        ggplot2::aes(x = Exposure, y = Vulnerability, colour = Risk_norm)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::geom_vline(data = medians_df,
                        ggplot2::aes(xintercept = med_exp),
                        linetype = "dashed", colour = "grey50") +
    ggplot2::geom_hline(data = medians_df,
                        ggplot2::aes(yintercept = med_vul),
                        linetype = "dashed", colour = "grey50") +
    ggplot2::geom_text(data = labels_df,
                       ggplot2::aes(label = NUTS_Name),
                       size = 2.6, vjust = -0.6, colour = "black") +
    ggplot2::facet_wrap(~ Sector_label, ncol = 3,
                        labeller = ggplot2::label_wrap_gen(width = 38)) +
    ggplot2::scale_colour_distiller(palette = "Reds", direction = 1,
                                    name = "Risk") +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title    = "Exposure x Vulnerability: regional archetypes",
      subtitle = "Dashed lines = sector medians. Top-3 highest-risk regions labelled."
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 9),
                   plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "right")

  outfile <- file.path(output_dir, "Figure_8_quadrants.png")
  ggplot2::ggsave(outfile, p, width = 12, height = 8, dpi = 300, bg = "white")
  outfile
}


# ── 4. Within-country variance in Risk_norm ────────────────────────────────

#' Boxplots of regional Risk_norm within each country, faceted by sector
#'
#' Shows for each (country, sector) the spread of NUTS-2 Risk_norm values.
#' Countries ordered by within-country median Risk for the focal sector(s).
#'
#' @return Path to saved PNG
plot_within_country_var <- function(risk_data, output_dir = "Figures",
                                    sectors = c("C", "C24", "C25+C28", "C29-C30")) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  df <- risk_data |>
    dplyr::filter(Sector_ID %in% sectors, !is.na(Risk_norm)) |>
    dplyr::mutate(
      Sector_label  = sector_name_map[Sector_ID],
      Country_Name  = country_names[Country_ID]
    ) |>
    dplyr::group_by(Sector_label, Country_Name) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::ungroup()

  focal_sector <- sector_name_map[sectors[1]]
  focal <- df |>
    dplyr::filter(Sector_label == focal_sector) |>
    dplyr::group_by(Country_Name) |>
    dplyr::summarise(med = stats::median(Risk_norm, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::arrange(med)
  df$Country_Name <- factor(df$Country_Name, levels = focal$Country_Name)

  p <- ggplot2::ggplot(df,
        ggplot2::aes(x = Country_Name, y = Risk_norm, fill = Country_Name)) +
    ggplot2::geom_boxplot(outlier.size = 0.7, alpha = 0.75) +
    ggplot2::facet_wrap(~ Sector_label, ncol = 1, scales = "free_y",
                        labeller = ggplot2::label_wrap_gen(width = 80)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title    = "Within-country variation in regional risk",
      subtitle = "Boxplot per country; countries with at least 3 NUTS-2 regions in the sector. Countries ordered by median risk in the first sector.",
      x = NULL, y = "Composite risk score (0-1)"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = 9, angle = 35, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  outfile <- file.path(output_dir, "Figure_9_within_country_variance.png")
  ggplot2::ggsave(outfile, p, width = 12, height = 9, dpi = 300, bg = "white")
  outfile
}


# ── 5. Sector cluster maps: employment x risk ──────────────────────────────

#' Bivariate maps showing where sector employment is concentrated AND risk is high
#'
#' For each focal sector, computes the region's share of EU sectoral employment
#' (using pers_employed) and joins with NUTS-2 shapefile. Points sized by share,
#' coloured by Risk_norm. Identifies regions that are both 'cluster of the
#' sector' AND 'at risk'.
#'
#' @return Character vector of saved file paths
plot_sector_cluster_maps <- function(risk_data, output_dir = "Figures",
                                     sectors = c("C24", "C25+C28", "C29-C30",
                                                 "C19-C20", "C23")) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Use exactly the same NUTS-2 / EU outline / background layers and the LAEA
  # projection as the original Figure 3 maps (06_visualize.R). .get_map_layers
  # and .crs_lambert are defined there and are visible because tar_source()
  # loads all R/*.R files into the same environment.
  layers <- .get_map_layers()

  saved <- character()
  for (s in sectors) {

    sec_df <- risk_data |>
      dplyr::filter(Sector_ID == s, !is.na(Risk_norm), !is.na(pers_employed),
                    pers_employed > 0) |>
      dplyr::mutate(empl_share = pers_employed / sum(pers_employed))

    merged <- layers$nuts2 |>
      dplyr::left_join(sec_df, by = "NUTS_ID")

    centroids <- sf::st_centroid(merged |> dplyr::filter(!is.na(Risk_norm)))

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = layers$europe_bg,
                       fill = "grey90", colour = "grey80", linewidth = 0.15) +
      ggplot2::geom_sf(data = merged, fill = "grey95",
                       colour = "grey60", linewidth = 0.08) +
      ggplot2::geom_sf(data = layers$eu_outline,
                       fill = NA, colour = "grey15", linewidth = 0.25) +
      ggplot2::geom_sf(data = centroids,
                       ggplot2::aes(size = empl_share, colour = Risk_norm),
                       alpha = 0.85) +
      ggplot2::coord_sf(crs = .crs_lambert) +
      ggplot2::scale_size_continuous(
        range  = c(0.5, 7),
        labels = scales::percent_format(0.1),
        name   = "Share of EU\nsector employment"
      ) +
      ggplot2::scale_colour_distiller(
        palette = "Reds", direction = 1, limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1),
        name = "Risk"
      ) +
      ggplot2::labs(
        title    = paste0("Sector cluster and risk: ", sector_name_map[s]),
        subtitle = "Point size = region's share of EU sectoral employment.  Colour = composite risk."
      ) +
      ggplot2::theme_void(base_size = 10) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold", size = 11),
        plot.subtitle    = ggplot2::element_text(size = 9, colour = "grey25"),
        plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        legend.position  = "right",
        legend.title     = ggplot2::element_text(face = "bold", size = 8),
        legend.text      = ggplot2::element_text(size = 7)
      )

    slug <- gsub("[^A-Za-z0-9]+", "_", s)
    outfile <- file.path(output_dir,
                         paste0("Figure_10_cluster_risk_", slug, ".png"))
    ggplot2::ggsave(outfile, p, width = 9, height = 8, dpi = 300, bg = "white")
    saved <- c(saved, outfile)
  }

  invisible(saved)
}

