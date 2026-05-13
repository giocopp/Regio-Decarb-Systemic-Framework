# ── build_scope3_upstream.R ── Producer-side upstream Scope 3 via FIGARO MRIO ──
#
# Computes the cradle-to-gate upstream Scope 3 emissions for each
# (country, NACE industry) pair using the FIGARO industry-by-industry IO
# table and the env_ac_ghgfp emissions extension. Electricity (D35) is
# EXCLUDED from the upstream multiplier so it does not double-count with
# the separate Scope 2 indicator.
#
# Outputs a tibble in the same schema as create_scope2/create_scope3.
# ──────────────────────────────────────────────────────────────────────────

suppressMessages({
  library(dplyr); library(tidyr); library(tibble); library(readxl)
  library(writexl); library(restatapi); library(Matrix)
})

source("R/utils.R")  # for eu27, sector_aggregation, sector_name_map

# ── 1. Load cached FIGARO IO table (2022) ────────────────────────
cache_file <- "Initial data/Non sector data/FIGARO_naio_10_fcp_ii4_2022.rds"
stopifnot(file.exists(cache_file))
io <- readRDS(cache_file)
cat(sprintf("Loaded FIGARO IO: %d rows\n", nrow(io)))

# ── 2. Real industries (64) and value-added/final-demand codes ───
real_ind <- c("A01","A02","A03","B","C10-12","C13-15","C16","C17","C18",
              "C19","C20","C21","C22","C23","C24","C25","C26","C27","C28",
              "C29","C30","C31_32","C33","D35","E36","E37-39","F","G45",
              "G46","G47","H49","H50","H51","H52","H53","I","J58","J59_60",
              "J61","J62_63","K64","K65","K66","L","M69_70","M71","M72",
              "M73","M74_75","N77","N78","N79","N80-82","O84","P85","Q86",
              "Q87_88","R90-92","R93","S94","S95","S96","T","U")
stopifnot(length(real_ind) == 64)
fd_use <- c("P3_S13","P3_S14","P3_S15","P5M","P51G")  # final demand columns

# Countries: 49 individual + WRL_REST. DOM appears in c_orig only.
regions <- sort(setdiff(unique(io$c_orig), "DOM"))
stopifnot(length(regions) == 50)
cat(sprintf("Regions: %d   Industries: %d\n", length(regions), length(real_ind)))

# ── 3. Build dimension index: (region, industry) -> integer 1..N ─
N <- length(regions) * length(real_ind)
idx <- expand.grid(region = regions, ind = real_ind,
                   stringsAsFactors = FALSE) |>
  mutate(k = row_number())
cat(sprintf("MRIO dimension N = %d\n", N))

# Lookup function
mk_key <- function(region, ind) paste(region, ind, sep = "|")
idx$key <- mk_key(idx$region, idx$ind)
key_to_k <- setNames(idx$k, idx$key)

# ── 4. Construct intermediate flow matrix Z (sparse) ─────────────
# Z[supplier=(c_orig,ind_ava), user=(c_dest,ind_use)] = value
Z_rows <- io |>
  filter(c_orig != "DOM",
         c_orig %in% regions, c_dest %in% regions,
         ind_ava %in% real_ind, ind_use %in% real_ind,
         !is.na(values), values > 0) |>
  mutate(
    i = key_to_k[mk_key(c_orig, ind_ava)],
    j = key_to_k[mk_key(c_dest, ind_use)]
  )
cat(sprintf("Non-zero Z entries: %d (%.2f%% of %d^2)\n",
            nrow(Z_rows), 100 * nrow(Z_rows) / N^2, N))
Z <- Matrix::sparseMatrix(i = Z_rows$i, j = Z_rows$j,
                          x = Z_rows$values, dims = c(N, N))

# ── 5. Total output x by (c_orig, ind_ava) = row sum + final demand
# x_i = sum_j z_{ij} + final_demand_{i}
fd_rows <- io |>
  filter(c_orig != "DOM",
         c_orig %in% regions, c_dest %in% regions,
         ind_ava %in% real_ind, ind_use %in% fd_use,
         !is.na(values)) |>
  group_by(c_orig, ind_ava) |>
  summarise(fd = sum(values, na.rm = TRUE), .groups = "drop")

x_int <- Matrix::rowSums(Z)
fd_vec <- numeric(N)
fd_idx <- key_to_k[mk_key(fd_rows$c_orig, fd_rows$ind_ava)]
fd_vec[fd_idx] <- fd_rows$fd
x_tot <- x_int + fd_vec
cat(sprintf("Total output: sum=%.0f MEUR, zero-output cells=%d\n",
            sum(x_tot), sum(x_tot == 0)))

# ── 6. A matrix: A[i,j] = z_{ij} / x_j  (dense for solve()) ──────
# Avoid division by zero: where x_j == 0, A column is 0
x_inv <- ifelse(x_tot > 0, 1 / x_tot, 0)
A <- Z %*% Matrix::Diagonal(N, x_inv)
A <- as.matrix(A)  # densify for solve()
cat(sprintf("A matrix: %d x %d, %.1f MB\n", nrow(A), ncol(A),
            object.size(A) / 1024^2))

# ── 7. Leontief inverse L = (I - A)^-1 ─────────────────────────
cat("Computing L = (I - A)^-1 ...\n")
t0 <- Sys.time()
I_minus_A <- diag(N) - A
L <- solve(I_minus_A)
cat(sprintf("  done in %.1fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# ── 8. Emissions vector f: Scope 1 per MEUR output  ─────────────
# From env_ac_ghgfp: sum over c_dest gives production-side Scope 1
# of (c_orig, nace_r2) regardless of where final demand is.
# API URL limit forces chunked downloads.
ghg_cache <- "Initial data/Non sector data/FIGARO_env_ac_ghgfp_2022.rds"
if (!file.exists(ghg_cache)) {
  cat("Fetching env_ac_ghgfp in country chunks...\n")
  chunks <- split(regions, ceiling(seq_along(regions) / 10))
  ghg_list <- list()
  for (ci in seq_along(chunks)) {
    cat(sprintf("  chunk %d/%d: %s\n", ci, length(chunks),
                paste(chunks[[ci]], collapse=",")))
    d <- get_eurostat_data(
      id = "env_ac_ghgfp",
      filters = list(na_item = "TOTAL",
                     c_orig = chunks[[ci]],
                     nace_r2 = real_ind),
      date_filter = 2022, exact_match = TRUE, label = FALSE
    )
    if (!is.null(d) && nrow(d) > 0) {
      ghg_list[[ci]] <- as_tibble(d) |>
        mutate(values = as.numeric(values)) |>
        select(c_orig, c_dest, nace_r2, values)
    }
  }
  ghg_all <- bind_rows(ghg_list)
  saveRDS(ghg_all, ghg_cache, compress = "xz")
  cat(sprintf("  cached %d rows\n", nrow(ghg_all)))
}
ghg <- readRDS(ghg_cache) |>
  filter(c_orig %in% regions, nace_r2 %in% real_ind) |>
  group_by(c_orig, nace_r2) |>
  summarise(scope1_kt = sum(values, na.rm = TRUE), .groups = "drop")
cat(sprintf("Emissions: %d (region, industry) cells\n", nrow(ghg)))

# Convert kt -> tonnes; intensity = tCO2eq / MEUR output
emis_vec <- numeric(N)
ghg$k <- key_to_k[mk_key(as.character(ghg$c_orig), as.character(ghg$nace_r2))]
ghg <- ghg |> filter(!is.na(k))
emis_t <- numeric(N)
emis_t[ghg$k] <- ghg$scope1_kt * 1000
f <- ifelse(x_tot > 0, emis_t / x_tot, 0)   # tonnes CO2eq per MEUR
cat(sprintf("f range: [%.3f, %.0f] tCO2eq/MEUR\n",
            min(f[f > 0]), max(f)))

# ── 9. Zero out D35 (electricity) intensities to avoid double counting
#       with Scope 2 indicator
d35_keys <- idx$k[idx$ind == "D35"]
f_no_d35 <- f
f_no_d35[d35_keys] <- 0
cat(sprintf("Zeroed out D35 emissions in %d regions\n", length(d35_keys)))

# ── 10. Upstream emissions per MEUR output of each sector ──────
# Upstream Scope 3 intensity (excl. Scope 1 own and Scope 2 D35):
#   m_j = sum_{i != j, i not D35} f_no_d35[i] * L[i,j]  -  f_no_d35[j] * (L[j,j] - 1)
# Simpler equivalent: m = f_no_d35 %*% (L - I), but subtract own-sector contribution
m_total <- as.numeric(t(f_no_d35) %*% (L - diag(N)))   # all upstream incl. own indirect
# Subtract own-sector self-loop contribution (avoid double-counting Scope 1 chain)
m_own  <- f_no_d35 * (diag(L) - 1)
m_up   <- m_total - m_own
m_up   <- pmax(m_up, 0)
cat(sprintf("Upstream intensity range (tCO2eq/MEUR): [%.2f, %.1f]\n",
            min(m_up[m_up > 0]), max(m_up)))

# ── 11. Scope 3 emissions per (region, industry) ──────────────
scope3_country_ind <- idx |>
  mutate(m_up = m_up, x = x_tot, Scope3_tCO2 = m_up * x_tot) |>
  select(region, ind, Scope3_tCO2)
cat(sprintf("\nSample (DE manufacturing):\n"))
print(scope3_country_ind |>
        filter(region == "DE", grepl("^C", ind)) |>
        mutate(Scope3_Mt = round(Scope3_tCO2 / 1e6, 2)) |>
        arrange(desc(Scope3_Mt)))

# ── 12. Aggregate FIGARO industries to 11 NACE manufacturing groups
nace_map <- tribble(
  ~ind,        ~Sector_ID,
  "C10-12",    "C10-C12",
  "C13-15",    "C13-C15",
  "C16",       "C16-C18", "C17", "C16-C18", "C18", "C16-C18",
  "C19",       "C19-C20", "C20", "C19-C20",
  "C21",       "C21-C22", "C22", "C21-C22",
  "C23",       "C23",
  "C24",       "C24",
  "C25",       "C25+C28", "C28", "C25+C28",
  "C29",       "C29-C30", "C30", "C29-C30",
  "C26",       "C26-C27", "C27", "C26-C27",
  "C31_32",    "C31-C33", "C33", "C31-C33"
)

scope3_grouped <- scope3_country_ind |>
  inner_join(nace_map, by = "ind") |>
  group_by(Country_ID = region, Sector_ID) |>
  summarise(Scope3_tCO2 = sum(Scope3_tCO2, na.rm = TRUE), .groups = "drop")

# C aggregate = sum of all manufacturing groups
scope3_C <- scope3_grouped |>
  group_by(Country_ID) |>
  summarise(Sector_ID = "C", Scope3_tCO2 = sum(Scope3_tCO2, na.rm = TRUE),
            .groups = "drop")
scope3_grouped <- bind_rows(scope3_grouped, scope3_C)

# Keep only EU-27 destinations (we don't need WRL_REST in output)
scope3_grouped <- scope3_grouped |> filter(Country_ID %in% eu27)
cat(sprintf("\nCountry-sector rows: %d\n", nrow(scope3_grouped)))

# ── 13. Downscale to NUTS-2 via employment shares ──────────────
empl <- read_xlsx("Initial data/Sector data/EMPL_Region.xlsx") |>
  select(NUTS_ID, Sector_ID, Share = Value) |>
  mutate(Country_ID = substr(NUTS_ID, 1, 2))

empl_total <- empl |>
  group_by(NUTS_ID) |>
  summarise(Total_Share = sum(Share, na.rm = TRUE), .groups = "drop")

empl_c <- empl_total |>
  mutate(Country_ID = substr(NUTS_ID, 1, 2), Sector_ID = "C") |>
  group_by(Country_ID) |>
  mutate(Share = Total_Share / sum(Total_Share, na.rm = TRUE)) |>
  ungroup() |>
  select(NUTS_ID, Sector_ID, Share, Country_ID)

empl_weights <- bind_rows(
  empl |>
    group_by(Country_ID, Sector_ID) |>
    mutate(Share = Share / sum(Share, na.rm = TRUE)) |>
    ungroup(),
  empl_c
)

scope3_regional <- scope3_grouped |>
  left_join(empl_weights, by = c("Country_ID", "Sector_ID")) |>
  mutate(Value = Scope3_tCO2 * Share) |>
  filter(!is.na(NUTS_ID), !is.na(Value)) |>
  transmute(Country_ID, NUTS_ID, Sector_ID,
            Indicator = "Scope3_Upstream",
            Unit = "tCO2eq",
            Value = round(Value, 2)) |>
  as_tibble()

cat(sprintf("\nFinal output: %d rows\n", nrow(scope3_regional)))
cat("Schema:\n"); print(names(scope3_regional))
cat("Top 10 by Value:\n")
print(scope3_regional |> arrange(desc(Value)) |> head(10))

# ── 14. Save to xlsx ──────────────────────────────────────────
out_file <- "Initial data/Sector data/EXP-Scope3_Upstream.xlsx"
writexl::write_xlsx(scope3_regional, out_file)
cat(sprintf("\nSaved to %s\n", out_file))
