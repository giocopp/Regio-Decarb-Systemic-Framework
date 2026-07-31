# build_tech_ris.R — regenerate Initial data/Non sector data/TECH-RIS.xlsx
# from the official EC RIS 2025 database (Regional Innovation Index, year
# 2024, scale "relative to EU 2018").
#
# Source: https://ec.europa.eu/assets/rtd/eis/2025/downloads/RIS_web_download.xlsx
# (sheet "RIS"; Indicator code "RII"; Year 2024; column "Value relative to EU in 2018")
#
# Mapping to the pipeline's region universe:
#   - coarser RIS codes (NUTS-1/0: AT1, BE2, FRC, CY0, DE3, ...) are expanded
#     by prefix to their NUTS-2021 NUTS-2 regions (value replicated — the RIS
#     publishes those countries at that level);
#   - finer/native codes with no prefix match (HR02/05/06, NL35/NL36,
#     PT19/PT1A-PT1D) are emitted as-is: R/03_reshape.R recombines them onto
#     the 230-region grid (HR04, NL31/NL33, PT16-18).
#
# Run from "Code and data/":  Rscript data_builders/build_tech_ris.R

suppressMessages({library(dplyr); library(readxl); library(writexl)})

URL   <- "https://ec.europa.eu/assets/rtd/eis/2025/downloads/RIS_web_download.xlsx"
OUT   <- "Initial data/Non sector data/TECH-RIS.xlsx"
CACHE <- "Initial data/Non sector data/RIS_web_download_2025.xlsx"
YEAR  <- 2024

if (!file.exists(CACHE)) download.file(URL, CACHE, mode = "wb", quiet = TRUE)

eu27 <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
          "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK")

db <- read_xlsx(CACHE, sheet = "RIS")
names(db) <- make.names(names(db))
rii <- db |>
  filter(Indicator.code == "RII", Year == YEAR,
         substr(Region.code, 1, 2) %in% eu27) |>
  transmute(RIS_code = Region.code, value = Value.relative.to.EU.in.2018)
stopifnot(nrow(rii) > 200, !anyNA(rii$value))

# region universe the pipeline has always fed (pre-recombination codes kept)
targets <- read_xlsx(OUT) |> distinct(NUTS_ID) |> pull(NUTS_ID)

rows <- purrr::map_dfr(seq_len(nrow(rii)), function(i) {
  rc   <- rii$RIS_code[i]
  kids <- targets[startsWith(targets, rc)]
  ids  <- if (length(kids) > 0) kids else rc
  tibble(CNTR_CODE = substr(rc, 1, 2), NUTS_ID = ids, RIS_code = rc,
         Indicator_ID = 0, Indicator = "0 Summary Innovation Index",
         value = rii$value[i])
})

# RIS 2025 keeps pre-recode NL codes and post-2024 PT codes; map them onto the
# pipeline's NUTS-2021 grid here (renames 1:1; PT = the Sec.2 recombination,
# unweighted mean of an intensive index, same as the reshape agg rule).
special <- list(list(target = "NL31", from = "NL35"),
                list(target = "NL33", from = "NL36"),
                list(target = "PT16", from = c("PT19", "PT1D")),
                list(target = "PT17", from = c("PT1A", "PT1B")),
                list(target = "PT18", from = "PT1C"))
sp_rows <- purrr::map_dfr(special, function(s) {
  v <- rii$value[rii$RIS_code %in% s$from]
  stopifnot(length(v) == length(s$from))
  tibble(CNTR_CODE = substr(s$target, 1, 2), NUTS_ID = s$target,
         RIS_code = paste(s$from, collapse = "+"), Indicator_ID = 0,
         Indicator = "0 Summary Innovation Index", value = mean(v))
})
rows <- rows |>
  filter(!NUTS_ID %in% unlist(lapply(special, `[[`, "from"))) |>
  bind_rows(sp_rows)
stopifnot(!any(duplicated(rows$NUTS_ID)))
missing_grid <- setdiff(targets, rows$NUTS_ID)
if (length(missing_grid) > 0)
  warning("grid regions without a RIS value: ", paste(missing_grid, collapse = " "))

write_xlsx(rows, OUT)
cat("wrote", OUT, "-", nrow(rows), "rows;",
    "microstates:", paste(intersect(c("CY00","EE00","LU00","LV00","MT00"),
                                    rows$NUTS_ID), collapse = " "), "\n")
