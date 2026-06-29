# cbam_typology_prototype.R — net-CBAM typology (cost vs protection side).
#
# The headline CBAM leg (R/exposure_cost.R::compute_cbam_leg) models CBAM as a
# COST on importers of covered goods. CBAM also PROTECTS EU producers of those
# goods by pricing imports at the EU carbon cost (carbon-leakage protection).
# This prototype classifies each EU country x covered good by whether CBAM, on
# balance, shields it, is neutral, or is a net cost — using FIGARO trade flows.
#
# Literature: protection is EU-market-only (Evans et al. 2020), excludes exports
# and downstream goods (Bellora & Fontagne 2023), and offsets only the
# competitiveness channel of leakage (~2/3; Mörsdorf 2021). So this is a
# DIRECTIONAL typology, not a euro quantification (that needs a GE/MRIO model).
#
# Run from "Code and data/":  Rscript prototypes/cbam_typology_prototype.R

suppressMessages({library(dplyr); library(tidyr); library(readr)})

dir <- "Initial data/Non sector data"
io_f <- list.files(dir, pattern = "^FIGARO_naio_10_fcp_ii4_\\d{4}\\.rds$",
                   full.names = TRUE)
io <- readRDS(io_f[length(io_f)]) |>
  mutate(across(c(ind_use, ind_ava, c_dest, c_orig), as.character))

eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
          "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")
nonEU <- setdiff(unique(io$c_orig), c(eu27, "DOM"))   # countries + RoW aggregates

# CBAM-covered origin industries (proxy used in compute_cbam_leg)
goods <- c("C20", "C23", "C24")

# Gross output, extra-EU exports, extra-EU imports per EU country x good (MEUR).
O <- io |> filter(ind_ava %in% goods, c_orig %in% eu27) |>
  group_by(Country = c_orig, good = ind_ava) |>
  summarise(O = sum(values, na.rm = TRUE), .groups = "drop")
X <- io |> filter(ind_ava %in% goods, c_orig %in% eu27, c_dest %in% nonEU) |>
  group_by(Country = c_orig, good = ind_ava) |>
  summarise(X = sum(values, na.rm = TRUE), .groups = "drop")
M <- io |> filter(ind_ava %in% goods, c_orig %in% nonEU, c_dest %in% eu27) |>
  group_by(Country = c_dest, good = ind_ava) |>
  summarise(M = sum(values, na.rm = TRUE), .groups = "drop")

tab <- O |> left_join(X, by = c("Country","good")) |>
  left_join(M, by = c("Country","good")) |>
  mutate(across(c(O, X, M), ~ replace_na(., 0)),
         export_share = ifelse(O > 0, X / O, NA),          # sold outside EU = unprotected
         import_pen   = ifelse((O - X + M) > 0, M / (O - X + M), NA))  # import competition CBAM neutralises

# Directional typology for all producer countries (O > 0); `major` flags the
# >= EUR 3bn producers used in the headline table (small ones are noisier).
cls <- tab |> filter(O > 0) |>
  mutate(major = O / 1e3 >= 3,
         type = case_when(
    M > O                ~ "Net importer (CBAM = cost)",
    export_share >= 0.30 ~ "Export-exposed (CBAM no help abroad)",
    import_pen   >= 0.15 ~ "Home-market, import-protected (CBAM shields)",
    TRUE                 ~ "Home-market, low-trade (CBAM ~neutral)"
  ))

# EU-level: share of covered-good output sold ON the EU market (= CBAM-protectable).
eu <- tab |> group_by(good) |>
  summarise(EU_market_share   = round(1 - sum(X) / sum(O), 2),
            import_competition = round(sum(M) / (sum(O) - sum(X) + sum(M)), 2),
            .groups = "drop")

cat("== EU covered-good output sold on the EU market (CBAM-protectable) ==\n")
print(as.data.frame(eu))
cat("\n== Typology counts (major producers, O>=EUR3bn) ==\n")
print(as.data.frame(cls |> filter(major) |> count(good, type) |>
                    pivot_wider(names_from = good, values_from = n, values_fill = 0)))

out <- cls |> arrange(good, desc(O)) |>
  transmute(Country, good, O_EURbn = round(O/1e3,1),
            export_share = round(export_share,2),
            import_pen = round(import_pen,2), major, type)
write_csv(out, "prototypes/cbam_typology_output.csv")
cat("\nWrote prototypes/cbam_typology_output.csv (", nrow(out), "rows)\n", sep = "")
