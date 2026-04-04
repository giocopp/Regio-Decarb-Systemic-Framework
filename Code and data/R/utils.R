# ── utils.R ── Shared helpers for the TRI pipeline ────────────────

#' Min-max rescale to [0.01, 0.99], preserving true zeros
#'
#' @param x Numeric vector
#' @return Numeric vector scaled to [0.01, 0.99]; zeros stay at 0
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.50, length(x)))
  scaled <- 0.01 + 0.98 * (x - rng[1]) / diff(rng)
  scaled[x == 0] <- 0
  scaled
}

#' Impute NA values with country x sector median
#'
#' @param df Data frame with Country_ID and Sector_ID columns
#' @param col Character: column name to impute
#' @return Data frame with NAs filled
impute_with_median <- function(df, col) {
  df |>
    dplyr::group_by(Country_ID, Sector_ID) |>
    dplyr::mutate(
      .donor = median(.data[[col]], na.rm = TRUE),
      !!rlang::sym(col) := dplyr::if_else(
        is.na(.data[[col]]), .donor, .data[[col]]
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".donor")
}

#' EU-27 country codes (2-letter ISO)
eu27 <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
  "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
  "PL", "PT", "RO", "SK", "SI", "ES", "SE"
)

#' ISO 3-letter to 2-letter mapping (for QoG Environmental data)
iso3_to_iso2 <- tibble::tribble(
  ~iso3, ~iso2,
  "AUT", "AT", "BEL", "BE", "BGR", "BG", "HRV", "HR", "CYP", "CY",
  "CZE", "CZ", "DNK", "DK", "EST", "EE", "FIN", "FI", "FRA", "FR",
  "DEU", "DE", "GRC", "EL", "HUN", "HU", "IRL", "IE", "ITA", "IT",
  "LVA", "LV", "LTU", "LT", "LUX", "LU", "MLT", "MT", "NLD", "NL",
  "POL", "PL", "PRT", "PT", "ROU", "RO", "SVK", "SK", "SVN", "SI",
  "ESP", "ES", "SWE", "SE"
)

#' Sector name lookup
sector_name_map <- c(
  "C"           = "Total Manufacturing",
  "C10-C12"     = "Manufacturing of Food, Beverage and Tobacco Products",
  "C13-C15"     = "Manufacturing of Textiles, Leather and Wearing Products",
  "C16-C18"     = "Manufacturing of Wood, Paper and Printing Products",
  "C19-C20"     = "Manufacturing of Chemical and Petrolchemical",
  "C21-C22"     = "Manufacturing of Pharmaceutical and Plastic Products",
  "C23"         = "Manufacturing of Non Metallic Mineral Products",
  "C24"         = "Manufacturing of Basic Metal Products",
  "C26-C27"     = "Manufacturing of Electronic and Electrical Products",
  "C25+C28-C30" = "Manufacturing of Fabricated Metal Products, Machinery, Vehicles and Transport Equipment",
  "C31-C33"     = "Other Manufacturing and Repairing"
)

#' Sector aggregation mapping (detailed NACE -> 11 groups)
sector_aggregation <- tibble::tribble(
  ~nace_detail, ~Sector_ID,
  "C",    "C",
  "C10",  "C10-C12", "C11", "C10-C12", "C12", "C10-C12",
  "C13",  "C13-C15", "C14", "C13-C15", "C15", "C13-C15",
  "C16",  "C16-C18", "C17", "C16-C18", "C18", "C16-C18",
  "C19",  "C19-C20", "C20", "C19-C20",
  "C21",  "C21-C22", "C22", "C21-C22",
  "C23",  "C23",
  "C24",  "C24",
  "C25",  "C25+C28-C30", "C28", "C25+C28-C30", "C29", "C25+C28-C30", "C30", "C25+C28-C30",
  "C26",  "C26-C27", "C27", "C26-C27",
  "C31",  "C31-C33", "C32", "C31-C33", "C33", "C31-C33"
)

#' Overseas / excluded NUTS-2 regions
excluded_nuts <- c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5",
                    "ES63", "ES64", "PT20", "PT30", "FI20", "CY00")

#' Aggregation rules for NUTS recombination (sum vs mean)
agg_rules <- tibble::tribble(
  ~Indicator,                        ~agg_fun,
  "GHG_Emissions",                   "sum",
  "Scope2_Emissions",                "sum",
  "Energy_Consumption",              "sum",
  "Fossil_Share",                    "mean",
  "Renewables_Share",                "mean",
  "Capital_Stock_Based_Prod",        "mean",
  "Gross_Fixed_Capital_Formation",   "sum",
  "Highly_Skilled_Workers",          "mean",
  "Labour_Market_Slack",             "mean",
  "Unemployment_Rate",               "mean",
  "Wage_Per_h",                      "mean",
  "Export_ExtraEU",                  "sum",
  "Import_ExtraEU",                  "sum",
  "BERD",                            "mean",
  "Regional_Innovation",             "mean",
  "Policy_Pressure",                 "mean",
  "QoG_Index",                       "mean",
  "Climate_Mitigation_Laws",         "mean",
  "HHI_Employment",                  "mean",
  "RE_Potential",                    "mean",
  "Share_of_Employment",             "mean",
  "Intangible_Investments",          "sum",
  "Tangible_Investments",            "sum",
  "Accountability",                  "mean",
  "Corruption",                      "mean",
  "Impartiality",                    "mean"
)
