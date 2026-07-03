# _targets.R — TRI pipeline definition.
#
# Usage:
#   setwd("Code and data")
#   library(targets)
#   tar_make()            # run full pipeline
#   tar_visnetwork()      # view dependency graph
#   tar_read(risk_data)   # load a specific target

library(targets)
library(tarchetypes)

# Source all function files
tar_source("R/")

# Pipeline options
tar_option_set(
  packages = c("dplyr", "tidyr", "readxl", "writexl", "stringr",
               "janitor", "purrr", "ggplot2", "sf", "giscoR",
               "RColorBrewer", "patchwork", "fmsb", "scales",
               "restatapi", "readr", "Matrix", "terra", "curl")
)

# ── File paths (inputs) ──────────────────────────────────────────
path <- list(
  base_data    = "Initial data/base_data_plus.xlsx",
  empl_shares  = "Initial data/Sector data/EMPL_Region.xlsx",
  qog          = "Initial data/Non sector data/qog_eureg.csv",
  qog_ei       = "Initial data/Non sector data/qog_ei_eureg.csv",
  enspreso     = "Initial data/Non sector data/ENSPRESO_Integrated_Data/ENSPRESO_Integrated_NUTS2_Data.csv"
)

# Non-sector indicator files
nonsector_files <- c(
  "Initial data/Non sector data/FINANCE-Capital_Stock_Based_Prod.xlsx",
  "Initial data/Non sector data/FINANCE-GFCF.xlsx",
  "Initial data/Non sector data/LABOUR-Highly_Skilled_Workers.xlsx",
  "Initial data/Non sector data/LABOUR-Labour_Market_Slack.xlsx",
  "Initial data/Non sector data/LABOUR-Unemployment.xlsx",
  "Initial data/Non sector data/TECH-RIS.xlsx",
  "Initial data/Non sector data/DIVERS-RE_Potential.xlsx",
  "Initial data/Non sector data/INST-QoG.xlsx",
  "Initial data/Non sector data/INST-Climate_Laws.xlsx",
  "Initial data/Non sector data/FINANCE-Cohesion_Fund.xlsx"
)

# Sector indicator files
sector_files <- c(
  "Initial data/Sector data/TECH-BERD.xlsx",
  "Initial data/Sector data/SUPCH-Import.xlsx",
  "Initial data/Sector data/SUPCH-Export.xlsx",
  "Initial data/Sector data/EXP-Emissions-Correct.xlsx",
  "Initial data/Sector data/ENERGY-Shares_Eurostat_nat.xlsx",
  "Initial data/Sector data/ENERGY-Energy-Correct.xlsx",
  "Initial data/Sector data/EMPL_Region.xlsx",
  "Initial data/Sector data/EXP-Scope2_Emissions.xlsx",
  "Initial data/Sector data/EXP-Scope3_Emissions.xlsx",
  "Initial data/Sector data/EXP-Policy_Pressure.xlsx",
  "Initial data/Sector data/DIVERS-Sector_Concentration.xlsx"
)

# ══════════════════════════════════════════════════════════════════
# PIPELINE DEFINITION
# ══════════════════════════════════════════════════════════════════

list(

  # ── Phase 1: Create initial data ─────────────────────────────
  tar_target(
    empl_weights,
    create_employment_weights(path$base_data),
    # Uncomment to skip API call and use cached file:
    # cue = tar_cue(mode = "never")
    format = "rds"
  ),

  # Regenerate EMPL_Region.xlsx whenever sector aggregation changes
  tar_target(
    empl_region_file,
    write_empl_region_xlsx(empl_weights,
                           "Initial data/Sector data/EMPL_Region.xlsx"),
    format = "file"
  ),

  tar_target(
    sector_concentration_data,
    { force(empl_region_file); create_sector_concentration(path$empl_shares) }
  ),
  tar_target(
    sector_concentration_file,
    {
      writexl::write_xlsx(sector_concentration_data,
                          "Initial data/Sector data/DIVERS-Sector_Concentration.xlsx")
      "Initial data/Sector data/DIVERS-Sector_Concentration.xlsx"
    },
    format = "file"
  ),

  tar_target(
    policy_pressure,
    create_policy_pressure(path$base_data)
  ),
  tar_target(
    policy_pressure_file,
    {
      writexl::write_xlsx(policy_pressure, "Initial data/Sector data/EXP-Policy_Pressure.xlsx")
      "Initial data/Sector data/EXP-Policy_Pressure.xlsx"
    },
    format = "file"
  ),

  tar_target(
    scope2_data,
    { force(empl_region_file); create_scope2(path$base_data, empl_weights) }
  ),
  tar_target(
    scope2_file,
    {
      writexl::write_xlsx(scope2_data, "Initial data/Sector data/EXP-Scope2_Emissions.xlsx")
      "Initial data/Sector data/EXP-Scope2_Emissions.xlsx"
    },
    format = "file"
  ),

  tar_target(
    scope3_data,
    { force(empl_region_file); create_scope3(empl_weights) }
  ),
  tar_target(
    scope3_file,
    {
      writexl::write_xlsx(scope3_data, "Initial data/Sector data/EXP-Scope3_Emissions.xlsx")
      "Initial data/Sector data/EXP-Scope3_Emissions.xlsx"
    },
    format = "file"
  ),

  # EQI 2024 standalone release (path inlined, not in `path`, so that editing
  # it does not invalidate every path-dependent creator target; the legacy
  # path$qog entry [qog_eureg.csv] is unused and kept only for hash stability)
  tar_target(
    qog_data,
    create_qog("Initial data/Non sector data/qog_eqi_long_24.csv",
               path$base_data)
  ),
  tar_target(
    qog_file,
    {
      writexl::write_xlsx(qog_data, "Initial data/Non sector data/INST-QoG.xlsx")
      "Initial data/Non sector data/INST-QoG.xlsx"
    },
    format = "file"
  ),

  tar_target(
    climate_laws,
    create_climate_laws(path$qog_ei, path$base_data)
  ),
  tar_target(
    climate_laws_file,
    {
      writexl::write_xlsx(climate_laws, "Initial data/Non sector data/INST-Climate_Laws.xlsx")
      "Initial data/Non sector data/INST-Climate_Laws.xlsx"
    },
    format = "file"
  ),

  tar_target(
    re_potential,
    create_re_potential(path$enspreso)
  ),
  tar_target(
    re_potential_file,
    {
      writexl::write_xlsx(re_potential, "Initial data/Non sector data/DIVERS-RE_Potential.xlsx")
      "Initial data/Non sector data/DIVERS-RE_Potential.xlsx"
    },
    format = "file"
  ),

  tar_target(
    cohesion_fund,
    create_cohesion_fund(path$base_data)
  ),
  tar_target(
    cohesion_fund_file,
    {
      writexl::write_xlsx(cohesion_fund, "Initial data/Non sector data/FINANCE-Cohesion_Fund.xlsx")
      "Initial data/Non sector data/FINANCE-Cohesion_Fund.xlsx"
    },
    format = "file"
  ),

  # ── Wave A — NUTS-2 direct Eurostat indicators ───────────────
  tar_target(gfcf_data,       create_gfcf(path$base_data)),
  tar_target(gfcf_file,
             write_indicator_xlsx(gfcf_data,
                                  "Initial data/Non sector data/FINANCE-GFCF.xlsx"),
             format = "file"),

  tar_target(unemployment_data, create_unemployment(path$base_data)),
  tar_target(unemployment_file,
             write_indicator_xlsx(unemployment_data,
                                  "Initial data/Non sector data/LABOUR-Unemployment.xlsx"),
             format = "file"),

  tar_target(labour_slack_data, create_labour_slack(path$base_data)),
  tar_target(labour_slack_file,
             write_indicator_xlsx(labour_slack_data,
                                  "Initial data/Non sector data/LABOUR-Labour_Market_Slack.xlsx"),
             format = "file"),

  tar_target(highly_skilled_data, create_highly_skilled(path$base_data)),
  tar_target(highly_skilled_file,
             write_indicator_xlsx(highly_skilled_data,
                                  "Initial data/Non sector data/LABOUR-Highly_Skilled_Workers.xlsx"),
             format = "file"),

  # ── Wave B — Extensive sector indicators (empl-share downscale) ─
  tar_target(scope1_data,
             { force(empl_region_file); create_scope1(path$base_data, empl_weights) }),
  tar_target(scope1_file,
             write_indicator_xlsx(scope1_data,
                                  "Initial data/Sector data/EXP-Emissions-Correct.xlsx"),
             format = "file"),

  tar_target(energy_consumption_data,
             { force(empl_region_file); create_energy_consumption(path$base_data, empl_weights) }),
  tar_target(energy_consumption_file,
             write_indicator_xlsx(energy_consumption_data,
                                  "Initial data/Sector data/ENERGY-Energy-Correct.xlsx"),
             format = "file"),

  tar_target(trade_extra_eu,
             { force(empl_region_file); create_trade_extra_eu(path$base_data, empl_weights) }),
  tar_target(trade_import_file,
             write_indicator_xlsx(trade_extra_eu$import,
                                  "Initial data/Sector data/SUPCH-Import.xlsx"),
             format = "file"),
  tar_target(trade_export_file,
             write_indicator_xlsx(trade_extra_eu$export,
                                  "Initial data/Sector data/SUPCH-Export.xlsx"),
             format = "file"),

  tar_target(berd_data,
             { force(empl_region_file); create_regional_berd(empl_weights) }),
  tar_target(berd_file,
             write_indicator_xlsx(berd_data,
                                  "Initial data/Sector data/TECH-BERD.xlsx"),
             format = "file"),

  # ── Wave C — Intensive sector indicators (uniform replication) ─
  tar_target(energy_shares_data, create_energy_shares(path$base_data)),
  tar_target(energy_shares_file,
             write_indicator_xlsx(energy_shares_data,
                                  "Initial data/Sector data/ENERGY-Shares_Eurostat_nat.xlsx"),
             format = "file"),

  tar_target(capital_stock_prod_data, create_capital_stock_prod(path$base_data)),
  tar_target(capital_stock_prod_file,
             write_indicator_xlsx(capital_stock_prod_data,
                                  "Initial data/Non sector data/FINANCE-Capital_Stock_Based_Prod.xlsx"),
             format = "file"),

  # ── Coverage report (which year each indicator picked) ──────
  tar_target(
    coverage_report,
    {
      yr <- function(x) {
        y <- attr(x, "year_selected")
        if (is.null(y)) NA_integer_ else as.integer(y)
      }
      src <- function(x, fallback) {
        s <- attr(x, "source_dataset")
        if (is.null(s)) fallback else as.character(s)
      }
      rows <- tibble::tibble(
        Indicator = c("Employment_Weights","GFCF","Unemployment",
                      "Labour_Market_Slack","Highly_Skilled_Workers",
                      "Scope1_Emissions","Scope2_Emissions","Scope3_Emissions",
                      "Energy_Consumption","Energy_Shares","Trade_Extra_EU",
                      "BERD","Capital_Stock_Based_Prod"),
        Source_Dataset = c(src(empl_weights, "sbs_r_nuts2021"),
                           src(gfcf_data, "nama_10r_2gfcf"),
                           src(unemployment_data, "lfst_r_lfu3rt"),
                           src(labour_slack_data, "lfst_r_sla_ga"),
                           src(highly_skilled_data, "hrst_st_rcat"),
                           "env_ac_ainah_r2",
                           src(scope2_data, "nrg_bal_c"),
                           src(scope3_data, "naio_10_fcp_ii4 + env_ac_ghgfp"),
                           "nrg_bal_c",
                           "nrg_bal_c",
                           "ext_tec01",
                           "rd_e_berdindr2 + demo_r_d2jan",
                           "nama_10_cp_a21"),
        Year_Selected = c(yr(empl_weights), yr(gfcf_data), yr(unemployment_data),
                          yr(labour_slack_data), yr(highly_skilled_data),
                          yr(scope1_data), yr(scope2_data), yr(scope3_data),
                          yr(energy_consumption_data), yr(energy_shares_data),
                          yr(trade_extra_eu), yr(berd_data),
                          yr(capital_stock_prod_data))
      )
      writexl::write_xlsx(rows, "Final data/Coverage_Report.xlsx")
      rows
    }
  ),

  # ── Phase 2: Harmonize ───────────────────────────────────────
  # Phase 2 depends on Phase 1 file targets (force() registers the dependency)
  tar_target(
    non_sector_data,
    {
      force(qog_file)
      force(climate_laws_file); force(re_potential_file)
      force(cohesion_fund_file)
      force(gfcf_file); force(unemployment_file)
      force(labour_slack_file); force(highly_skilled_file)
      force(capital_stock_prod_file)
      harmonize_non_sector(
        file_paths = validate_files(nonsector_files),
        base_data_path = path$base_data
      )
    }
  ),

  tar_target(
    sector_data,
    {
      force(scope2_file); force(scope3_file); force(policy_pressure_file)
      force(scope1_file); force(energy_consumption_file); force(energy_shares_file)
      force(trade_import_file); force(trade_export_file); force(berd_file)
      force(sector_concentration_file)
      harmonize_sector(
        file_paths = validate_files(sector_files),
        non_sector_data = non_sector_data,
        empl_weights = empl_weights
      )
    }
  ),

  tar_target(
    all_data_long,
    combine_all(sector_data, non_sector_data)
  ),

  # ── Phase 3: Reshape ─────────────────────────────────────────
  tar_target(
    data_reshaped,
    reshape_to_grid(all_data_long, agg_rules)
  ),

  # ── Phase 4: Normalize ───────────────────────────────────────
  tar_target(
    data_normalized,
    normalize_indicators(data_reshaped, empl_weights)
  ),

  # ── Phase 5: Risk index (headline) ───────────────────────────
  # Exposure = covered carbon volume (geocoded ETS + CBAM legs).
  # See METHODOLOGY.md §10.1 and R/exposure.R.

  # "minmax" | "log" | "rank" (sensitivity_risk compares all three)
  tar_target(tri_norm_mode, "minmax"),

  # EUTL inputs built once by prototypes/ets_geocode.R; provenance in
  # Initial data/EUTL_euets_info/README.md
  tar_target(ets_nuts2_file,
             "Initial data/EUTL_euets_info/ets_nuts2_sector.csv",
             format = "file"),
  tar_target(ets_geo, read_ets_nuts2(ets_nuts2_file)),

  tar_target(
    figaro_cache,
    { force(scope3_file); figaro_cache_files() },
    format = "file"
  ),

  # HEADLINE CBAM component: employment-share downscaling weights for ALL
  # sectors (heavy_sectors = character(0)) — adopted 2026-07-03 after the
  # external trade validation (METHODOLOGY §14): employment locates the
  # importing users of covered inputs, matches the pipeline's canonical
  # downscaling rule (§5), fits observed regional imports at ρ = 0.91 and
  # respects the physical import ceilings the plant-emission hybrid breaches.
  tar_target(
    cbam_leg,
    {
      io  <- readRDS(figaro_cache[[1]])
      ghg <- readRDS(figaro_cache[[2]])
      compute_cbam_leg(io, ghg,
                       build_cbam_weights(empl_weights, ets_geo,
                                          heavy_sectors = character(0)))
    }
  ),

  # CBAM component under the full-embodied (cradle-to-gate) intensity —
  # robustness input for the sensitivity battery (REVISION.md §23). Same
  # employment-only weights as the headline so the comparison isolates the
  # intensity basis. Headline stays direct.
  tar_target(
    cbam_leg_embodied,
    {
      io  <- readRDS(figaro_cache[[1]])
      ghg <- readRDS(figaro_cache[[2]])
      compute_cbam_leg(io, ghg,
                       build_cbam_weights(empl_weights, ets_geo,
                                          heavy_sectors = character(0)),
                       intensity = "embodied")
    }
  ),

  # CBAM component under the former hybrid weights (plant-emission shares in
  # the four heavy sectors) — retained ONLY as the allocation-sensitivity
  # variant; rejected as headline by the external validation (ρ = 0.69,
  # ceiling breaches in Sardegna 3.4× / Puglia 1.4× — METHODOLOGY §14).
  tar_target(
    cbam_leg_hybrid,
    {
      io  <- readRDS(figaro_cache[[1]])
      ghg <- readRDS(figaro_cache[[2]])
      compute_cbam_leg(io, ghg, build_cbam_weights(empl_weights, ets_geo))
    }
  ),

  tar_target(
    vulnerability_pooled,
    build_vulnerability_pooled(data_reshaped, empl_weights,
                               norm = tri_norm_mode)
  ),

  tar_target(
    risk_data,
    build_risk_data(vulnerability_pooled, ets_geo, cbam_leg,
                    data_reshaped, norm = tri_norm_mode)
  ),

  tar_target(
    save_risk_xlsx,
    {
      writexl::write_xlsx(risk_data, "Final data/Risk_data.xlsx")
      write.csv(risk_data, "Final data/Risk_data.csv",
                row.names = FALSE)
      "Final data/Risk_data.xlsx"
    },
    format = "file"
  ),

  # ── Phase 6: Raw-emissions index (comparison only) ───────────
  tar_target(
    risk_data_raw_emissions,
    aggregate_risk(data_normalized$wide)
  ),

  tar_target(
    save_risk_raw_emissions_xlsx,
    {
      writexl::write_xlsx(risk_data_raw_emissions,
                          "Final data/Risk_data_raw_emissions.xlsx")
      write.csv(risk_data_raw_emissions,
                "Final data/Risk_data_raw_emissions.csv", row.names = FALSE)
      "Final data/Risk_data_raw_emissions.xlsx"
    },
    format = "file"
  ),

  # ── Phase 7: Save raw data ───────────────────────────────────
  tar_target(
    save_raw_xlsx,
    {
      writexl::write_xlsx(data_reshaped, "Final data/Raw_data_not_normalized.xlsx")
      write.csv(data_reshaped, "Final data/Raw_data_not_normalized.csv", row.names = FALSE)
      "Final data/Raw_data_not_normalized.xlsx"
    },
    format = "file"
  ),

  # ── Phase 8: Figures ─────────────────────────────────────────
  tar_target(
    figure_maps,
    plot_tri_maps(risk_data, output_dir = "Figures"),
    format = "file"
  ),

  tar_target(
    figure_radars,
    plot_radar_charts(risk_data, output_dir = "Figures"),
    format = "file"
  ),

  tar_target(
    figure_risk_maps,
    plot_risk_maps(risk_data, output_dir = "Figures"),
    format = "file"
  ),

  # ── Phase 9: Sensitivity analysis ────────────────────────────
  # EDGAR-based Scope 1 for C19-C20, C23, C24 — sensitivity input only.
  tar_target(
    edgar_scope1,
    compute_edgar_scope1(year = 2023L)
  ),

  tar_target(
    sensitivity_results,
    run_sensitivity(risk_data_raw_emissions, edgar_scope1 = edgar_scope1)
  ),

  tar_target(
    sensitivity_risk,
    run_risk_sensitivity(risk_data, vulnerability_pooled, ets_geo,
                         cbam_leg, risk_data_raw_emissions,
                         norm = tri_norm_mode,
                         cbam_leg_embodied = cbam_leg_embodied,
                         cbam_leg_hybrid = cbam_leg_hybrid)
  ),

  tar_target(
    save_sensitivity,
    {
      writexl::write_xlsx(
        list(baseline_battery = sensitivity_results,
             risk_tri         = sensitivity_risk),
        "Final data/Sensitivity_Analysis.xlsx"
      )
      "Final data/Sensitivity_Analysis.xlsx"
    },
    format = "file"
  ),

  # ── Phase 10: Insights (tables + extra figures) ───────────────
  tar_target(
    top_bottom_table,
    build_top_bottom_table(risk_data, k = 5)
  ),

  tar_target(
    save_top_bottom_xlsx,
    {
      writexl::write_xlsx(top_bottom_table,
                          "Final data/Top_Bottom_Regions_per_Sector.xlsx")
      "Final data/Top_Bottom_Regions_per_Sector.xlsx"
    },
    format = "file"
  ),

  tar_target(
    figure_quadrants,
    plot_quadrant(risk_data, output_dir = "Figures",
                  sectors = c("C", "C24", "C25+C28", "C29-C30", "C19-C20", "C23")),
    format = "file"
  ),

  tar_target(
    figure_within_country,
    plot_within_country_var(risk_data, output_dir = "Figures",
                            sectors = c("C", "C24", "C25+C28", "C29-C30")),
    format = "file"
  ),

  tar_target(
    figure_cluster_maps,
    plot_sector_cluster_maps(risk_data, output_dir = "Figures",
                             sectors = c("C24", "C25+C28", "C29-C30",
                                         "C19-C20", "C23")),
    format = "file"
  )
)
