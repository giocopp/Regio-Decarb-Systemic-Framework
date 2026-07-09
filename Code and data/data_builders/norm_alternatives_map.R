# norm_alternatives_map.R — deck figure: Total-Manufacturing Exposure and
# Risk under min-max (headline) vs log. Rank was dropped from the slides
# (2026-07-09): it is the continuous version of the quintile-class map
# rendering and agrees with log at rho 0.98 — it stays only as a
# sensitivity-workbook row.
# CONTINUOUS fills on purpose: the figure exists to show how the score
# SPACING changes — the Exposure ordering is identical under all three
# (Spearman = 1.00); Risk reranks at rho ~ 0.78 (minmax vs log/rank) while
# log and rank agree at 0.98. Headline = tri_norm_mode (minmax); the choice
# is an open aggregation decision (METHODOLOGY §10.1 / §14).
# Run from "Code and data/":  Rscript data_builders/norm_alternatives_map.R

suppressMessages({library(dplyr); library(sf); library(ggplot2)
                  library(patchwork); library(targets)})
source("R/utils.R"); source("R/exposure.R"); source("R/04_normalize.R")
source("R/06_visualize.R")

OUT <- "Figures"
layers <- .get_map_layers()

vuln <- tar_read(vulnerability_pooled)
geo  <- tar_read(ets_geo)
cbam <- tar_read(cbam_leg)
drs  <- tar_read(data_reshaped)

pal_exp  <- RColorBrewer::brewer.pal(7, "Purples")
pal_risk <- RColorBrewer::brewer.pal(7, "Reds")

panels <- list()
for (nm in c("log", "minmax")) {
  d <- build_risk_data(vuln, geo, cbam, drs, norm = nm,
                       denom = "per_employee") |>
    filter(Sector_ID == "C") |>
    mutate(Exposure = if_else(Exposure == 0, NA_real_, Exposure))
  m <- layers$nuts2 |> left_join(d, by = "NUTS_ID") |>
    st_transform(.crs_lambert)
  lab <- c(log = "log (headline)", minmax = "min-max")[[nm]]
  panels[[paste0("E_", nm)]] <-
    .single_map(m, layers$europe_bg, layers$eu_outline,
                "Exposure", "Exposure", pal_exp) +
    ggtitle(paste0("Exposure — ", lab)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
  panels[[paste0("R_", nm)]] <-
    .single_map(m, layers$europe_bg, layers$eu_outline,
                "Risk_norm", "Risk", pal_risk) +
    ggtitle(paste0("Risk — ", lab)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
}

# third column: quintile classes of the headline scores (norm-invariant:
# identical under min-max, log or rank because the ordering is identical).
d0 <- build_risk_data(vuln, geo, cbam, drs, norm = "log",
                      denom = "per_employee") |>
  filter(Sector_ID == "C") |>
  mutate(Exposure = if_else(Exposure == 0, NA_real_, Exposure))
m0 <- layers$nuts2 |> left_join(d0, by = "NUTS_ID") |> st_transform(.crs_lambert)
panels$E_q <- .single_map(m0, layers$europe_bg, layers$eu_outline,
                          "Exposure", "Exposure", pal_exp, bins = TRUE) +
  ggtitle("Exposure — quintile classes (= binned rank)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
panels$R_q <- .single_map(m0, layers$europe_bg, layers$eu_outline,
                          "Risk_norm", "Risk", pal_risk, bins = TRUE) +
  ggtitle("Risk — quintile classes (= binned rank)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))

fig <- (panels$E_log | panels$E_minmax | panels$E_q) /
       (panels$R_log | panels$R_minmax | panels$R_q) +
  patchwork::plot_annotation(
    title = "Total Manufacturing — log (headline) vs min-max vs quintile classes",
    theme = theme(plot.title = element_text(size = 13, face = "bold",
                                            hjust = 0.5)))

f <- file.path(OUT, "Figure_norm_alternatives.png")
ggsave(f, fig, width = 14, height = 8.5, dpi = 300, bg = "white")
cat("wrote", f, "\n")
dc <- "../Review/Hazard_Exposure_Slides/norm_alternatives.png"
if (dir.exists(dirname(dc))) {
  file.copy(f, dc, overwrite = TRUE); cat("copied to", dc, "\n")
}
