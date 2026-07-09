# 08_sensitivity_alt.R — EDGAR-based Scope 1 sensitivity for the
# heavy-emitter sectors (C19-C20, C23, C24).
#
# IPCC -> canonical NACE crosswalk:
#   REF_TRF + CHE -> C19-C20
#   NMM           -> C23
#   IRO           -> C24


#' Download one EDGAR sector x year NetCDF and unzip (cached).
#' Returns the path to the .nc file.
.download_edgar_sector <- function(sector, year,
                                   gas = "CO2",
                                   cache_dir = "Initial data/_cache") {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  nc_name  <- sprintf("EDGAR_2024_GHG_%s_%d_%s_emi.nc", gas, year, sector)
  nc_path  <- file.path(cache_dir, nc_name)
  if (file.exists(nc_path) && file.size(nc_path) > 1e6) return(nc_path)

  zip_name <- sprintf("EDGAR_2024_GHG_%s_%d_%s_emi_nc.zip", gas, year, sector)
  zip_url  <- sprintf(
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/EDGAR_2024_GHG/%s/%s/emi_nc/%s",
    gas, sector, zip_name
  )
  zip_path <- file.path(cache_dir, zip_name)
  curl::curl_download(zip_url, zip_path, quiet = TRUE)
  utils::unzip(zip_path, files = nc_name, exdir = cache_dir)
  unlink(zip_path)
  if (!file.exists(nc_path)) stop("EDGAR download failed: ", nc_path)
  nc_path
}


#' Sum one EDGAR raster (tonnes per cell) over each NUTS-2 polygon.
.edgar_to_nuts2 <- function(nc_path, nuts2_sf) {
  r <- terra::rast(nc_path)
  v <- terra::vect(nuts2_sf)
  ext <- terra::extract(r, v, fun = sum, na.rm = TRUE, ID = FALSE)
  tibble::tibble(NUTS_ID = nuts2_sf$NUTS_ID,
                 tonnes  = as.numeric(ext[[1]]))
}


#' EDGAR-based Scope 1 emissions for C19-C20, C23, C24.
#' Aggregates EDGAR 0.1° gridded CO2 to NUTS-2 polygons.
#'
#' @param year EDGAR year (default 2023, latest).
#' @return Tibble: Country_ID, NUTS_ID, Sector_ID, Scope1_kt_EDGAR.
compute_edgar_scope1 <- function(year = 2023L) {

  nuts2 <- giscoR::gisco_get_nuts(year = "2021", nuts_level = 2,
                                   resolution = "10") |>
    dplyr::filter(CNTR_CODE %in% eu27,
                  !NUTS_ID %in% excluded_nuts) |>
    sf::st_make_valid()

  message("EDGAR: downloading sector NetCDFs for ", year, " ...")
  iro_nc     <- .download_edgar_sector("IRO",     year)
  nmm_nc     <- .download_edgar_sector("NMM",     year)
  ref_trf_nc <- .download_edgar_sector("REF_TRF", year)
  che_nc     <- .download_edgar_sector("CHE",     year)

  message("EDGAR: aggregating cells to NUTS-2 polygons ...")
  iro     <- .edgar_to_nuts2(iro_nc,     nuts2)
  nmm     <- .edgar_to_nuts2(nmm_nc,     nuts2)
  ref_trf <- .edgar_to_nuts2(ref_trf_nc, nuts2)
  che     <- .edgar_to_nuts2(che_nc,     nuts2)

  c1920 <- ref_trf |>
    dplyr::full_join(che, by = "NUTS_ID", suffix = c("_ref", "_che")) |>
    dplyr::transmute(NUTS_ID,
                     tonnes = dplyr::coalesce(tonnes_ref, 0) +
                              dplyr::coalesce(tonnes_che, 0),
                     Sector_ID = "C19-C20")
  c23 <- nmm |> dplyr::mutate(Sector_ID = "C23")
  c24 <- iro |> dplyr::mutate(Sector_ID = "C24")

  out <- dplyr::bind_rows(c1920, c23, c24) |>
    dplyr::transmute(Country_ID = substr(NUTS_ID, 1, 2),
                     NUTS_ID,
                     Sector_ID,
                     Scope1_kt_EDGAR = round(tonnes / 1000, 4)) |>
    tibble::as_tibble()

  attr(out, "year_selected")  <- year
  attr(out, "source_dataset") <- "EDGAR 2024 GHG (CO2 grids)"
  out
}


#' EDGAR Scope-1 sensitivity test for the three heavy sectors.
#' For each of C19-C20, C23, C24, swap baseline Scope1_Emissions for the
#' EDGAR-derived ranking, recompute Exposure and Risk_norm within that
#' sector only, and report the Spearman rho vs the baseline ranking.
run_edgar_sensitivity <- function(risk_data, edgar_scope1) {

  rd <- risk_data |>
    dplyr::filter(!is.na(Risk_norm),
                  Sector_ID %in% c("C19-C20", "C23", "C24"))

  joined <- rd |>
    dplyr::left_join(edgar_scope1, by = c("Country_ID", "NUTS_ID", "Sector_ID"))

  purrr::map_dfr(unique(joined$Sector_ID), function(s) {

    s_data <- joined |> dplyr::filter(Sector_ID == s, !is.na(Scope1_kt_EDGAR))
    if (nrow(s_data) < 5) return(NULL)

    s_data <- s_data |>
      dplyr::mutate(
        Scope1_Emissions_alt = range01(Scope1_kt_EDGAR),
        Exposure_alt = rowMeans(dplyr::pick(
          Scope1_Emissions_alt, Scope2_Emissions, Scope3_Emissions
        ), na.rm = TRUE),
        Exposure_alt = range01(Exposure_alt),
        Risk_raw_alt = Exposure_alt^0.5 * Vulnerability^0.5,
        Risk_alt = range01(Risk_raw_alt)
      )

    rho <- cor(s_data$Risk_norm, s_data$Risk_alt,
               use = "pairwise.complete.obs", method = "spearman")

    # Single-sector test: pooled and within-sector rho coincide.
    tibble::tibble(test = paste0("EDGAR Scope1 [", s, "]"),
                   rho_pooled = round(rho, 4),
                   rho_within_sector = round(rho, 4))
  })
}
