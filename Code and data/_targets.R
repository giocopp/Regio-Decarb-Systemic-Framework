# ── _targets.R ── Transition Risk Index pipeline ─────────────────
#
# Reproduces the full TRI analysis from raw Eurostat inputs
# to final risk scores, figures, and sensitivity analysis.
#
# Usage:
#   setwd("Code and data")
#   library(targets)
#   tar_make()            # run full pipeline
#   tar_visnetwork()      # view dependency graph
#   tar_read(risk_data)   # load a specific target
#
# Reference:
#   "A Systemic Framework for Assessing the Risk of Decarbonization
#    to Regional Manufacturing Activities in the European Union"
# ──────────────────────────────────────────────────────────────────

library(targets)
library(tarchetypes)

# Source all function files
tar_source("R/")

# Pipeline options
tar_option_set(
  packages = c("dplyr", "tidyr", "readxl", "writexl", "stringr",
               "janitor", "purrr", "ggplot2", "sf", "giscoR",
               "RColorBrewer", "patchwork", "fmsb", "scales",
               "restatapi", "readr", "Matrix")
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
  "Initial data/Non sector data/DIVERS-HHI.xlsx",
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
  "Initial data/Sector data/EXP-Policy_Pressure.xlsx"
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
    hhi_data,
    { force(empl_region_file); create_hhi(path$empl_shares) }
  ),
  tar_target(
    hhi_file,
    {
      writexl::write_xlsx(hhi_data, "Initial data/Non sector data/DIVERS-HHI.xlsx")
      "Initial data/Non sector data/DIVERS-HHI.xlsx"
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
    { force(empl_region_file); create_scope2(path$base_data, path$empl_shares) }
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
    { force(empl_region_file); create_scope3(path$empl_shares) }
  ),
  tar_target(
    scope3_file,
    {
      writexl::write_xlsx(scope3_data, "Initial data/Sector data/EXP-Scope3_Emissions.xlsx")
      "Initial data/Sector data/EXP-Scope3_Emissions.xlsx"
    },
    format = "file"
  ),

  tar_target(
    qog_data,
    create_qog(path$qog)
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

  # ── Phase 2: Harmonize ───────────────────────────────────────
  # Phase 2 depends on Phase 1 file targets (force() registers the dependency)
  tar_target(
    non_sector_data,
    {
      force(hhi_file); force(qog_file)
      force(climate_laws_file); force(re_potential_file)
      force(cohesion_fund_file)
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

  # ── Phase 5: Aggregate risk ──────────────────────────────────
  tar_target(
    risk_data,
    aggregate_risk(data_normalized$wide)
  ),

  # ── Phase 6: Save final data ─────────────────────────────────
  tar_target(
    save_raw_xlsx,
    {
      writexl::write_xlsx(data_reshaped, "Final data/Raw_data_not_normalized.xlsx")
      write.csv(data_reshaped, "Final data/Raw_data_not_normalized.csv", row.names = FALSE)
      "Final data/Raw_data_not_normalized.xlsx"
    },
    format = "file"
  ),

  tar_target(
    save_risk_xlsx,
    {
      writexl::write_xlsx(risk_data, "Final data/Risk_data.xlsx")
      write.csv(risk_data, "Final data/Risk_data.csv", row.names = FALSE)
      "Final data/Risk_data.xlsx"
    },
    format = "file"
  ),

  # ── Phase 7: Figures ─────────────────────────────────────────
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

  # ── Phase 8: Sensitivity analysis ────────────────────────────
  tar_target(
    sensitivity_results,
    run_sensitivity(risk_data)
  ),

  tar_target(
    save_sensitivity,
    {
      writexl::write_xlsx(sensitivity_results, "Final data/Sensitivity_Analysis.xlsx")
      "Final data/Sensitivity_Analysis.xlsx"
    },
    format = "file"
  ),

  # ── Phase 9: Insights (tables + extra figures) ────────────────
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
    figure_decomposition,
    plot_risk_decomposition(risk_data, output_dir = "Figures",
                            sectors = c("C", "C24", "C25+C28", "C29-C30",
                                        "C19-C20", "C23")),
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
