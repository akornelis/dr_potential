# report.R
# Runs the full DR potential pipeline and generates report figures.
# Run from the project root: Rscript report.R

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

source("R/load_data.R")
source("R/technical_potential.R")
source("R/achievable_potential.R")
source("R/costs.R")
source("R/aggregation.R")
source("R/reporting.R")

num_years <- 10

# ── Run pipeline ──────────────────────────────────────────────────────────────

cat("Loading inputs...\n")
inputs <- load_all_inputs("data")

cat("Calculating technical potential...\n")
technical_potential <- calculate_technical_potential(inputs, num_years = num_years)

cat("Calculating enrollment...\n")
enrollment <- calculate_enrollment(inputs, num_years = num_years)

cat("Calculating achievable potential...\n")
achievable_potential <- calculate_achievable_potential(inputs, enrollment, num_years = num_years)

cat("Calculating costs...\n")
program_costs <- calculate_costs(inputs, enrollment)

cat("Computing summary statistics...\n")
summary_stats   <- get_summary_stats(achievable_potential)
seasonal_stats  <- get_seasonal_stats(achievable_potential, inputs$global$season_lookup)

# ── Export CSVs ───────────────────────────────────────────────────────────────

cat("\nExporting CSVs...\n")
export_results(technical_potential,  "01_technical_potential_hourly.csv")
export_results(achievable_potential, "02_achievable_potential_hourly.csv")
export_results(program_costs,        "03_program_costs_annual.csv")
export_results(summary_stats,        "04_summary_statistics.csv")
export_results(seasonal_stats,       "05_seasonal_stats.csv")

# ── Generate figures ──────────────────────────────────────────────────────────

generate_report(
  achievable      = achievable_potential,
  costs           = program_costs,
  summary_stats   = summary_stats,
  season_lookup   = inputs$global$season_lookup,
  output_dir      = "outputs/figures",
  milestone_years = c(5, 10)
)

cat("\n✓ Report complete.\n")
