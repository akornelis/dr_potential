# example_analysis.R
# Example workflow: Load data, calculate potentials, generate outputs
# Run from the project root: Rscript example_analysis.R

# Source all R functions
source("R/load_data.R")
source("R/technical_potential.R")
source("R/achievable_potential.R")
source("R/costs.R")
source("R/aggregation.R")

# ===== LOAD ALL INPUTS =====
cat("Loading inputs...\n")
inputs <- load_all_inputs("data")

cat("✓ Inputs loaded successfully:\n")
cat("  Customers:", nrow(inputs$customers), "rows\n")
cat("  Product impacts:", nrow(inputs$product_impact), "rows\n")
cat("  End-use shapes:", length(unique(inputs$end_use_shapes$end_use)), "shapes × 8760 hours\n")
cat("  Participation rates:", nrow(inputs$participation), "rows\n")
cat("  Costs:", nrow(inputs$costs), "rows\n")
cat("  Eligibility matrix:", nrow(inputs$eligibility), "rows\n\n")

num_years <- 10

# ===== CALCULATE ENROLLMENT (shared) =====
cat("Calculating enrollment...\n")
enrollment <- calculate_enrollment(inputs, num_years = num_years)
cat("✓ Enrollment calculated\n")
cat("  Dimensions:", nrow(enrollment), "rows\n\n")

# ===== CALCULATE TECHNICAL POTENTIAL =====
cat("Calculating technical potential...\n")
technical_potential <- calculate_technical_potential(inputs, num_years = num_years)
cat("✓ Technical potential calculated\n")
cat("  Dimensions:", nrow(technical_potential), "rows\n")
cat("  Date range: Year", min(technical_potential$year), "-", max(technical_potential$year), "\n\n")

# Sample output
cat("Sample technical potential (first 10 rows):\n")
print(head(technical_potential, 10))

# ===== CALCULATE ACHIEVABLE POTENTIAL =====
cat("\nCalculating achievable potential...\n")
achievable_potential <- calculate_achievable_potential(inputs, enrollment, num_years = num_years)
cat("✓ Achievable potential calculated\n")
cat("  Dimensions:", nrow(achievable_potential), "rows\n\n")

# Sample output
cat("Sample achievable potential (first 10 rows):\n")
print(head(achievable_potential, 10))

# ===== CALCULATE COSTS =====
cat("\nCalculating program costs...\n")
program_costs <- calculate_costs(inputs, enrollment)
cat("✓ Costs calculated\n")
cat("  Dimensions:", nrow(program_costs), "rows\n\n")

# Sample output
cat("Sample costs (first 10 rows):\n")
print(head(program_costs, 10))

# ===== AGGREGATIONS AND SUMMARIES =====
cat("\n===== RESULTS SUMMARIES =====\n\n")

# Annual totals by product
cat("Annual Technical Potential by Product (MWh, summed across all segments and hours):\n")
tech_by_product <- aggregate(
  technical_potential$technical_mw,
  by = list(year = technical_potential$year, product = technical_potential$product),
  FUN = function(x) sum(x)  # MW-hours
)
names(tech_by_product)[3] <- "total_mwh"
print(tech_by_product)

cat("\n\nAnnual Achievable Potential by Product (MWh):\n")
achv_by_product <- aggregate(
  achievable_potential$achievable_mw,
  by = list(year = achievable_potential$year, product = achievable_potential$product),
  FUN = function(x) sum(x)
)
names(achv_by_product)[3] <- "total_mwh"
print(achv_by_product)

# Summary statistics
cat("\n\nSummary Statistics - Achievable Potential:\n")
summary_stats <- get_summary_stats(achievable_potential)
print(summary_stats)

# Seasonal statistics
cat("\n\nSeasonal Peak Statistics:\n")
seasonal_stats <- get_seasonal_stats(achievable_potential, inputs$global$season_lookup)
print(seasonal_stats)

# Peak day profile (example: day 1, year 10)
cat("\n\nPeak Day Profile - Year 10 (Day 1):\n")
peak_day <- get_peak_day(achievable_potential, peak_day = 1, year = 10)
print(peak_day)

# ===== EXPORT RESULTS =====
cat("\n===== EXPORTING RESULTS =====\n\n")

export_results(technical_potential, "01_technical_potential_hourly.csv")
export_results(achievable_potential, "02_achievable_potential_hourly.csv")
export_results(program_costs, "03_program_costs_annual.csv")
export_results(tech_by_product, "04_technical_potential_annual_by_product.csv")
export_results(achv_by_product, "05_achievable_potential_annual_by_product.csv")
export_results(summary_stats, "06_summary_statistics.csv")
export_results(seasonal_stats, "07_seasonal_stats.csv")

cat("\n✓ All results exported to outputs/ directory!\n")
