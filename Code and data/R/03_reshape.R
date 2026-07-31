# 03_reshape.R — expand the harmonised tibble into a complete
# region x sector x indicator grid and apply NUTS-2013 -> NUTS-2021
# recombinations.


#' Expand `all_data_long` into a complete region x sector grid, handle
#' NUTS recombinations (HR, NL, PT), and collapse duplicates.
#'
#' @param all_data_long Tibble from `combine_all()`.
#' @param agg_rules     Tibble (Indicator, agg_fun in {"sum","mean"}) from
#'   `utils.R`. Sum for extensive quantities, mean for intensive ones.
#' @return Long tibble with one row per region x sector x indicator.
reshape_to_grid <- function(all_data_long, agg_rules) {

  # ── reference universes ──
  sectors <- all_data_long |>
    dplyr::filter(!is.na(Sector_ID)) |>
    dplyr::distinct(Sector_ID, Sector_Name)

  regions <- all_data_long |>
    dplyr::filter(!is.na(NUTS_ID)) |>
    dplyr::distinct(Country_ID, NUTS_ID, NUTS_Name)

  n_regions <- regions |>
    dplyr::count(Country_ID, name = "n_regions")

  # ── pathway 1: region x sector (already at finest grain) ──
  reg_sec <- all_data_long |>
    dplyr::filter(!is.na(NUTS_ID), !is.na(Sector_ID)) |>
    dplyr::mutate(Notes = "data originally at region-sector level")

  # ── pathway 2: sector-national -> replicate to every region ──
  sector_nat <- all_data_long |>
    dplyr::filter(is.na(NUTS_ID) & !is.na(Sector_ID)) |>
    dplyr::select(-NUTS_ID, -NUTS_Name) |>
    dplyr::left_join(n_regions, by = "Country_ID") |>
    dplyr::mutate(
      Value = Value / n_regions,
      Notes = "sector-national value averaged across regions"
    ) |>
    dplyr::select(-n_regions) |>
    dplyr::left_join(regions, by = "Country_ID")

  # ── pathway 3: region-only -> cross with every sector ──
  region_only <- all_data_long |>
    dplyr::filter(!is.na(NUTS_ID) & is.na(Sector_ID)) |>
    dplyr::select(-Sector_ID, -Sector_Name) |>
    tidyr::crossing(sectors) |>
    dplyr::mutate(Notes = "region-only value duplicated across sectors")

  # ── combine & collapse duplicates ──
  grid <- dplyr::bind_rows(reg_sec, sector_nat, region_only) |>
    dplyr::group_by(Country_ID, NUTS_ID, NUTS_Name,
             Sector_ID, Sector_Name,
             Component, Dimension,
             Indicator, Unit) |>
    dplyr::summarise(
      Value = mean(Value, na.rm = TRUE),
      Notes = Notes[which.max(!is.na(Notes))],
      .groups = "drop"
    )

  # ──────────────────────────────────────────────────────────────────────────

  #                     NUTS recombinations
  # ──────────────────────────────────────────────────────────────────────────

  # ── helper: aggregate rows using agg_rules ──
  .aggregate_nuts <- function(df, source_ids, target_id, target_name,
                              copy_from = NULL, copy_indicators = NULL) {

    new_rows <- df |>
      dplyr::filter(NUTS_ID %in% source_ids) |>
      dplyr::left_join(agg_rules, by = "Indicator") |>
      dplyr::group_by(Country_ID, Sector_ID, Sector_Name,
               Component, Dimension, Indicator, Unit, agg_fun) |>
      dplyr::summarise(
        Value = if (dplyr::first(agg_fun) == "sum") {
          if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE)
        } else {
          if (all(is.na(Value))) NA_real_ else mean(Value, na.rm = TRUE)
        },
        Notes = dplyr::first(Notes[!is.na(Notes)], default = NA_character_),
        .groups = "drop"
      ) |>
      dplyr::mutate(NUTS_ID   = target_id,
             NUTS_Name = target_name) |>
      dplyr::select(dplyr::all_of(names(df)))

    # Optionally copy specific indicators from a donor region
    if (!is.null(copy_from) && !is.null(copy_indicators)) {
      to_copy <- df |>
        dplyr::filter(NUTS_ID == copy_from,
               Indicator %in% copy_indicators) |>
        dplyr::mutate(NUTS_ID   = target_id,
               NUTS_Name = target_name) |>
        dplyr::select(dplyr::all_of(names(df)))

      new_rows <- dplyr::rows_update(
        new_rows, to_copy,
        by = c("Country_ID", "NUTS_ID", "Sector_ID", "Indicator", "Unit")
      )
    }

    # Drop all-NA aggregates before the caller upserts: an input that already
    # arrives on the recombined code (e.g. ENSPRESO RE_Potential carries HR04
    # directly) must not be clobbered by an NA aggregate built from absent
    # source regions. Truly missing cells still end up NA via grid completion,
    # so the "all-NA groups stay NA, not 0" rule (METHODOLOGY §15) is intact.
    new_rows |> dplyr::filter(!is.na(Value))
  }

  # ── Croatia: HR02 + HR05 + HR06 -> HR04 ──
  grid <- grid |>
    dplyr::mutate(NUTS_Name = dplyr::if_else(NUTS_ID == "HR04",
                               "Continentalna Hrvatska", NUTS_Name))

  hr04_new <- .aggregate_nuts(
    grid,
    source_ids      = c("HR02", "HR05", "HR06"),
    target_id       = "HR04",
    target_name     = "Continentalna Hrvatska",
    copy_from       = "HR03",
    copy_indicators = c("Unemployment_Rate", "Capital_Stock_Based_Prod")
  )

  grid <- dplyr::rows_upsert(grid, hr04_new,
                       by = c("Country_ID", "NUTS_ID",
                              "Sector_ID", "Indicator", "Unit")) |>
    dplyr::filter(Country_ID != "HR" | NUTS_ID %in% c("HR03", "HR04"))

  # ── Netherlands: NL35 -> NL31, NL36 -> NL33 ──
  nl_lookup <- tibble::tribble(
    ~old,   ~new,   ~new_name,
    "NL35", "NL31", "Utrecht",
    "NL36", "NL33", "Zuid-Holland"
  )

  grid <- grid |>
    dplyr::left_join(nl_lookup, by = c("NUTS_ID" = "old")) |>
    dplyr::mutate(
      NUTS_ID   = dplyr::coalesce(new, NUTS_ID),
      NUTS_Name = dplyr::coalesce(new_name, NUTS_Name)
    ) |>
    dplyr::select(-new, -new_name)

  # ── Portugal: aggregate & relabel ──
  pt_lookup <- tibble::tribble(
    ~old,   ~new,
    "PT19", "PT16",
    "PT1D", "PT16",
    "PT1A", "PT17",
    "PT1B", "PT17",
    "PT1C", "PT18"
  )

  pt_new <- grid |>
    dplyr::filter(NUTS_ID %in% c(pt_lookup$old, pt_lookup$new)) |>
    dplyr::left_join(pt_lookup, by = c("NUTS_ID" = "old")) |>
    dplyr::mutate(target = dplyr::coalesce(new, NUTS_ID)) |>
    dplyr::left_join(agg_rules, by = "Indicator") |>
    dplyr::group_by(Country_ID, target, Sector_ID, Sector_Name,
             Component, Dimension, Indicator, Unit, agg_fun) |>
    dplyr::summarise(
      Value     = if (dplyr::first(agg_fun) == "sum") {
                    if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE)
                  } else {
                    if (all(is.na(Value))) NA_real_ else mean(Value, na.rm = TRUE)
                  },
      Notes     = dplyr::first(Notes[!is.na(Notes)], default = NA_character_),
      NUTS_Name = dplyr::first(NUTS_Name[!is.na(NUTS_Name)], default = NA_character_),
      .groups   = "drop"
    ) |>
    dplyr::rename(NUTS_ID = target) |>
    dplyr::select(dplyr::all_of(names(grid)))

  obsolete_pt <- pt_lookup$old
  grid <- grid |>
    dplyr::filter(!NUTS_ID %in% obsolete_pt) |>
    dplyr::rows_upsert(pt_new,
                by = c("Country_ID", "NUTS_ID",
                       "Sector_ID", "Indicator", "Unit"))

  # ── drop any remaining obsolete codes ──
  grid <- grid |>
    dplyr::filter(!NUTS_ID %in% c("NL35", "NL36",
                            "PT19", "PT1A", "PT1B", "PT1C", "PT1D"))

  # ── backfill NUTS_Name within each NUTS_ID ──
  grid <- grid |>
    dplyr::group_by(NUTS_ID) |>
    dplyr::mutate(NUTS_Name = dplyr::coalesce(
      NUTS_Name,
      dplyr::first(NUTS_Name[!is.na(NUTS_Name)])
    )) |>
    dplyr::ungroup()

  tibble::as_tibble(grid)
}
