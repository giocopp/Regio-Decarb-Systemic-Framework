# Total Manufacturing Exposure: log vs linear min-max, side by side, to inspect the
# normalization. Raw cost per region reconstructed from the prototype CSV (full phase-in, EUA=1).
suppressMessages({library(dplyr); library(sf); library(ggplot2); library(patchwork)})
source("R/06_visualize.R")
OUT <- "Figures"

d   <- read.csv("Final data/Risk_data_carbon_cost_PROTOTYPE.csv", stringsAsFactors = FALSE)
sub <- d |> filter(Sector_ID != "C", !is.na(Exposure)) |> mutate(raw = ets_emis_t + CBAM_emb_tCO2)
rng <- function(x) (x - min(x)) / (max(x) - min(x))
Craw <- sub |> group_by(NUTS_ID) |> summarise(raw = sum(raw), .groups = "drop") |>
  mutate(exp_linear = rng(raw), exp_log = rng(log1p(raw)))

layers <- .get_map_layers()
nuts2  <- layers$nuts2 |>
  mutate(NUTS_ID = ifelse(NUTS_ID %in% c("HR02","HR05","HR06"), "HR04", NUTS_ID)) |>
  group_by(NUTS_ID) |> summarise(geometry = st_union(geometry), .groups = "drop") |> st_make_valid()
m <- nuts2 |> left_join(Craw, by = "NUTS_ID") |> st_transform(.crs_lambert)

pal  <- RColorBrewer::brewer.pal(7, "Purples")
ttl  <- function(t) ggtitle(t) + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
plog <- .single_map(m, layers$europe_bg, layers$eu_outline, "exp_log",    "Exposure", pal) + ttl("log (current)")
plin <- .single_map(m, layers$europe_bg, layers$eu_outline, "exp_linear", "Exposure", pal) + ttl("linear min-max")
combo <- (plog | plin) + plot_annotation(
  title = "Total Manufacturing Exposure - normalization: log vs linear min-max",
  theme = theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5)))
f <- file.path(OUT, "Figure_exposure_norm_compare.png")
ggsave(f, combo, width = 11, height = 5, dpi = 300, bg = "white"); cat("wrote", f, "\n")
