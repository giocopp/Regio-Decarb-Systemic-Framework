# ── 03_reshape.R ── Expand long data to a complete region x sector grid ───────
#
# Exported function:
#   reshape_to_grid()
#
# Packages loaded via tar_option_set() in _targets.R

# ── reshape_to_grid() ───────────────────────────────────────────────────────

#' Expand all_data_long into a complete region x sector grid, handle NUTS
#' recombinations, and collapse duplicates.
#'
#' Three pathways:
#'   1. Region x Sector  -- keep as-is.
#'   2. Sector-national   -- divide Value by n_regions, replicate to every region.
#'   3. Region-only       -- cross with all sectors.
#'
#' NUTS recombinations:
#'   Croatia    HR02 + HR05 + HR06 -> HR04
#'   Netherlands NL35 -> NL31, NL36 -> NL33
#'   Portugal   PT19 + PT1D -> PT16, PT1A + PT1B -> PT17, PT1C -> PT18
#'
#' @param all_data_long Tibble from combine_all().
#' @param agg_rules     Tibble with columns Indicator, agg_fun ("sum"/"mean")
#'                      from utils.R.
#' @return Long tibble with one row per region x sector x indicator.
reshape_to_grid <- function(all_data_long, agg_rules) {

  # ── reference universes ──
  sectors <- all_data_long |>
    filter(!is.na(Sector_ID)) |>
    distinct(Sector_ID, Sector_Name)

  regions <- all_data_long |>
    filter(!is.na(NUTS_ID)) |>
    distinct(Country_ID, NUTS_ID, NUTS_Name)

  n_regions <- regions |>
    count(Country_ID, name = "n_regions")

  # ── pathway 1: region x sector (already at finest grain) ──
  reg_sec <- all_data_long |>
    filter(!is.na(NUTS_ID), !is.na(Sector_ID)) |>
    mutate(Notes = "data originally at region-sector level")

  # ── pathway 2: sector-national -> replicate to every region ──
  sector_nat <- all_data_long |>
    filter(is.na(NUTS_ID) & !is.na(Sector_ID)) |>
    select(-NUTS_ID, -NUTS_Name) |>
    left_join(n_regions, by = "Country_ID") |>
    mutate(
      Value = Value / n_regions,
      Notes = "sector-national value averaged across regions"
    ) |>
    select(-n_regions) |>
    left_join(regions, by = "Country_ID")

  # ── pathway 3: region-only -> cross with every sector ──
  region_only <- all_data_long |>
    filter(!is.na(NUTS_ID) & is.na(Sector_ID)) |>
    select(-Sector_ID, -Sector_Name) |>
    crossing(sectors) |>
    mutate(Notes = "region-only value duplicated across sectors")

  # ── combine & collapse duplicates ──
  grid <- bind_rows(reg_sec, sector_nat, region_only) |>
    group_by(Country_ID, NUTS_ID, NUTS_Name,
             Sector_ID, Sector_Name,
             Component, Dimension,
             Indicator, Unit) |>
    summarise(
      Value = mean(Value, na.rm = TRUE),
      Notes = Notes[which.max(!is.na(Notes))],
      .groups = "drop"
    ) |>
    filter(!(Indicator == "Energy_Consumption" & Unit != "MWh"))

  # ──────────────────────────────────────────────────────────────────────────

  #                     NUTS recombinations
  # ──────────────────────────────────────────────────────────────────────────

  # ── helper: aggregate rows using agg_rules ──
  .aggregate_nuts <- function(df, source_ids, target_id, target_name,
                              copy_from = NULL, copy_indicators = NULL) {

    new_rows <- df |>
      filter(NUTS_ID %in% source_ids) |>
      left_join(agg_rules, by = "Indicator") |>
      group_by(Country_ID, Sector_ID, Sector_Name,
               Component, Dimension, Indicator, Unit, agg_fun) |>
      summarise(
        Value = if (first(agg_fun) == "sum") {
          if (all(is.na(Value))) NA_real_ else sum(Value, na.rm = TRUE)
        } else {
          if (all(is.na(Value))) NA_real_ else mean(Value, na.rm = TRUE)
        },
        Notes = first(Notes[!is.na(Notes)], default = NA_character_),
        .groups = "drop"
      ) |>
      mutate(NUTS_ID   = target_id,
             NUTS_Name = target_name) |>
      select(all_of(names(df)))

    # Optionally copy specific indicators from a donor region
    if (!is.null(copy_from) && !is.null(copy_indicators)) {
      to_copy <- df |>
        filter(NUTS_ID == copy_from,
               Indicator %in% copy_indicators) |>
        mutate(NUTS_ID   = target_id,
               NUTS_Name = target_name) |>
        select(all_of(names(df)))

      new_rows <- rows_update(
        new_rows, to_copy,
        by = c("Country_ID", "NUTS_ID", "Sector_ID", "Indicator", "Unit")
      )
    }

    new_rows
  }

  # ── Croatia: HR02 + HR05 + HR06 -> HR04 ──
  grid <- grid |>
    mutate(NUTS_Name = if_else(NUTS_ID == "HR04",
                               "Continentalna Hrvatska", NUTS_Name))

  hr04_new <- .aggregate_nuts(
    grid,
    source_ids      = c("HR02", "HR05", "HR06"),
    target_id       = "HR04",
    target_name     = "Continentalna Hrvatska",
    copy_from       = "HR03",
    copy_indicators = c("Unemployment_Rate", "Capital_Stock_Based_Prod")
  )

  grid <- rows_upsert(grid, hr04_new,
                       by = c("Country_ID", "NUTS_ID",
                              "Sector_ID", "Indicator", "Unit")) |>
    filter(Country_ID != "HR" | NUTS_ID %in% c("HR03", "HR04"))

  # ── Netherlands: NL35 -> NL31, NL36 -> NL33 ──
  nl_lookup <- tibble::tribble(
    ~old,   ~new,   ~new_name,
    "NL35", "NL31", "Utrecht",
    "NL36", "NL33", "Zuid-Holland"
  )

  grid <- grid |>
    left_join(nl_lookup, by = c("NUTS_ID" = "old")) |>
    mutate(
      NUTS_ID   = coalesce(new, NUTS_ID),
      NUTS_Name = coalesce(new_name, NUTS_Name)
    ) |>
    select(-new, -new_name)

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
    filter(NUTS_ID %in% c(pt_lookup$old, pt_lookup$new)) |>
    left_join(pt_lookup, by = c("NUTS_ID" = "old")) |>
    mutate(target = coalesce(new, NUTS_ID)) |>
    left_join(agg_rules, by = "Indicator") |>
    group_by(Country_ID, target, Sector_ID, Sector_Name,
             Component, Dimension, Indicator, Unit, agg_fun) |>
    summarise(
      Value     = if (first(agg_fun) == "sum")
                    sum(Value, na.rm = TRUE)
                  else
                    mean(Value, na.rm = TRUE),
      Notes     = first(Notes[!is.na(Notes)], default = NA_character_),
      NUTS_Name = first(NUTS_Name[!is.na(NUTS_Name)], default = NA_character_),
      .groups   = "drop"
    ) |>
    rename(NUTS_ID = target) |>
    select(all_of(names(grid)))

  obsolete_pt <- pt_lookup$old
  grid <- grid |>
    filter(!NUTS_ID %in% obsolete_pt) |>
    rows_upsert(pt_new,
                by = c("Country_ID", "NUTS_ID",
                       "Sector_ID", "Indicator", "Unit"))

  # ── drop any remaining obsolete codes ──
  grid <- grid |>
    filter(!NUTS_ID %in% c("NL35", "NL36",
                            "PT19", "PT1A", "PT1B", "PT1C", "PT1D"))

  # ── backfill NUTS_Name within each NUTS_ID ──
  grid <- grid |>
    group_by(NUTS_ID) |>
    mutate(NUTS_Name = coalesce(
      NUTS_Name,
      first(NUTS_Name[!is.na(NUTS_Name)])
    )) |>
    ungroup()

  tibble::as_tibble(grid)
}
