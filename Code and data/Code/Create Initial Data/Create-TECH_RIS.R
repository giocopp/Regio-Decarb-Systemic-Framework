
### INSTALL PACKAGES

remotes::install_github(
  "eurostat/restatapi"
)

libs <- c(
  "tidyverse",
  "classInt"
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


###
### REGIONAL INNOVATION SCOREBOARD
### 
RIS <- readxl::read_excel("Code and data/Initial data/RIS_2023.xlsx") |>
  dplyr::filter(Year == 2023) |>
  mutate(Indicator_ID = str_extract(Indicator, "^[0-9.]+")) |>
  select(Indicator_ID, everything())

RIS <- RIS |>
  filter(Indicator_ID == "0")

base_data <- readxl::read_excel("Code and data/Initial dataa/base_data_plus.xlsx")

RIS <- base_data |>
  left_join(RIS, by = c("RIS_code" = "Region")) |>
  filter(nchar(NUTS_ID) != 2) |>
  select(CNTR_CODE, NUTS_ID, NUTS_NAME, Value)

RIS <- RIS |> 
  mutate(
    Country_CD = CNTR_CODE,
    Country_Name = NA_character_,
    NUTS_ID = NUTS_ID,
    NUTS_Name = NUTS_NAME,
    Sector_CD = NA_character_,
    Sector_ID = NA_character_,
    Sector_Name = NA_character_,
    Component = "Vulnerability",
    Dimension = "Technology",
    Variable = "Regional Innovation Score",
    Year = as.factor(2022),
    Source = "Regional Innovation Scoreboard (EC-RIS)",
    Value = as.numeric(Value),
    Unit = "Index",
    Value_Norm = NA_real_
  ) |> 
  select(Country_CD, Country_Name, NUTS_ID, NUTS_Name, Sector_CD, Sector_ID, Sector_Name, Component, Dimension, Variable, Year, Source, Value, Unit, Value_Norm)

writexl::write_xlsx(RIS, "Code and data/Initial data/Non sector data/TECH-RIS.xlsx")