library(dplyr)
library(fmsb)
library(scales)
library(readxl)

###

data <- readxl::read_xlsx("Code and data/Final data/Risk_data.xlsx")

output_root <- "Code and data/Figures"
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

# Radar‑chart variables
radar_vars   <- c("Exposure",
                  "Vuln_Energy", "Vuln_Labour",
                  "Vuln_Supply_Chain", "Vuln_Technology",
                  "Vuln_Finance", "Vuln_Institutions",
                  "Vuln_Diversification")

radar_labels <- c("Exposure",
                  "Energy", "Labour",
                  "Supply Chain", "Technology",
                  "Finance", "Institutions",
                  "Diversification")

# Helper: single radar plot
plot_radar <- function(row_data,
                       title_label,
                       color = "firebrick") {
  vals <- as.numeric(as.data.frame(row_data)[, radar_vars])
  
  chart_data <- rbind(rep(1, length(vals)),  # max for each axis
                      rep(0, length(vals)),  # min for each axis
                      vals)
  colnames(chart_data) <- radar_labels
  chart_data <- as.data.frame(chart_data)
  
  radarchart(chart_data,
             axistype    = 1,
             pcol        = color,
             pfcol       = alpha(color, .40),
             plwd        = 2,
             cglcol      = "grey70",
             cglty       = 1,
             cglwd       = .8,
             axislabcol  = "grey40",
             caxislabels = seq(0, 1, .2),
             vlcex       = 1.3,
             title       = NULL)
  
  title(main = title_label,
        cex.main = 1.4,
        font.main = 2)
}

# Region selection common to both figures

countries <- c(Germany = "DE", Greece = "EL")
sector_total <- "Total Manufacturing"
sector_polluting <- "Manufacturing of Basic Metal Products"

selected_regions <- data %>%
  filter(Sector_Name == sector_total,
         Country_ID  %in% countries,
         !is.na(Risk_norm)) %>%
  group_by(Country_ID) %>%
  slice_max(Risk_norm, n = 1, with_ties = FALSE) %>%
  mutate(Risk_Position = "Highest") %>%
  bind_rows(
    data %>%
      filter(Sector_Name == sector_total,
             Country_ID  %in% countries,
             !is.na(Risk_norm)) %>%
      group_by(Country_ID) %>%
      slice_min(Risk_norm, n = 1, with_ties = FALSE) %>%
      mutate(Risk_Position = "Lowest")
  ) %>%
  ungroup() %>%
  arrange(factor(Country_ID, levels = countries),
          desc(Risk_Position == "Highest"))

# Colour palette in the plotting order:
region_cols <- c("firebrick", "darkolivegreen4", "firebrick", "darkolivegreen4")

###

# Figure 5 – Total Manufacturing
png(file.path(output_root, "Figure_5.png"), width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
for (i in seq_len(nrow(selected_regions))) {
  r <- selected_regions[i, ]
  caption <- sprintf(
    "%s (%s)\n%s\n%s risk (Risk = %.2f)",
    r$NUTS_Name, r$Country_ID,
    "Total Manufacturing",
    r$Risk_Position, r$Risk_norm
  )
  plot_radar(r, caption, color = region_cols[i])
}
dev.off()

# Figure 6 – Basic Metal Products: highest/lowest for this sector
selected_regions_polluting <- data %>%
  filter(Sector_Name == sector_polluting,
         Country_ID  %in% countries,
         !is.na(Risk_norm)) %>%
  group_by(Country_ID) %>%
  slice_max(Risk_norm, n = 1, with_ties = FALSE) %>%
  mutate(Risk_Position = "Highest") %>%
  bind_rows(
    data %>%
      filter(Sector_Name == sector_polluting,
             Country_ID  %in% countries,
             !is.na(Risk_norm)) %>%
      group_by(Country_ID) %>%
      slice_min(Risk_norm, n = 1, with_ties = FALSE) %>%
      mutate(Risk_Position = "Lowest")
  ) %>%
  ungroup() %>%
  arrange(factor(Country_ID, levels = countries),
          desc(Risk_Position == "Highest"))

png(file.path(output_root, "Figure_6.png"), width = 1800, height = 1800, res = 150)
par(mfrow = c(2, 2))
for (i in seq_len(nrow(selected_regions_polluting))) {
  r <- selected_regions_polluting[i, ]
  caption <- sprintf(
    "%s (%s)\n%s\n%s risk (Risk = %.2f)",
    r$NUTS_Name, r$Country_ID,
    "Basic Metal Products",
    r$Risk_Position, r$Risk_norm
  )
  plot_radar(r, caption, color = region_cols[i])
}
dev.off()  