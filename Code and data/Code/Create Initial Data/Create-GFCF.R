### Install Necessary Packages ###
remotes::install_github("eurostat/restatapi")

libs <- c(
  "restatapi",
  "tidyverse",
  "giscoR",
  "sf",
  "classInt",
  "mice",
  "visdat",
  "VIM"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libs == FALSE)) {
  install.packages(
    libs[!installed_libs],
    dependencies = TRUE
  )
}

invisible(
  lapply(
    libs, library,
    character.only = TRUE
  )
)

### GET DATA
### 
### Eurostat data

###
### GFCF
### 

indicator_df <- restatapi::get_eurostat_data(
  id = "nama_10r_2gfcf",
  filters = c("MIO_EUR", "C"), 
  date_filter = c(2021),
  exact_match = T,
  label = F,
  cflags = T,
  keep_flags = T,
)

indicator_df_f <- indicator_df |> 
  dplyr::rename(
    "NUTS_ID" = "geo",
    "GFCF" = "values",
    "Year" = "time"
  ) |> 
  dplyr::select(NUTS_ID, GFCF)

base_data <- readxl::read_excel("Code and data/Initial data/base_data_plus.xlsx") |> 
  select(1:3)

Eurostat_GFCF <- base_data |> 
  dplyr::left_join(indicator_df_f, by = c("NUTS_ID" = "NUTS_ID")) |> 
  dplyr::select(-NUTS_NAME) |> 
  dplyr::filter(nchar(NUTS_ID) != 2)

Sector_ID <- c("C", "C10-C12", "C13-C15", "C16-C18", "C19-C20", "C21-C22", "C23", 
               "C24", "C25+C28-C30", "C26-C27", "C31-C32", "C33")

Eurostat_GFCF <- Eurostat_GFCF|> 
  tidyr::crossing(Sector_ID) |> 
  select(NUTS_ID, Sector_ID, everything())

# Save each dataset to an Excel file
writexl::write_xlsx(Eurostat_GFCF, "Code and data/Initial data/Non sector data/FINANCE-GFCF.xlsx")


