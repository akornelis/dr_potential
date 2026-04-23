#!/usr/bin/env Rscript
# Generate input template CSV files for DR model

output_dir <- "data/"

# Ensure directory exists
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ===== 1. CUSTOMER COUNTS (year-varying: ~1% annual growth) =====
num_years <- 10
base_counts <- c(single_family = 70000, multi_family = 60000, manufactured_home = 55000,
                 industrial = 3500)
growth_rates <- c(single_family = 0.010, multi_family = 0.010, manufactured_home = 0.010,
                  industrial = 0.005)
customer_counts_df <- do.call(rbind, lapply(1:num_years, function(yr) {
  data.frame(
    year          = yr,
    segment       = names(base_counts),
    customer_count = round(base_counts * (1 + growth_rates) ^ (yr - 1))
  )
}))
customer_counts_wide <- reshape(customer_counts_df,
                                idvar = "segment", timevar = "year", direction = "wide")
names(customer_counts_wide) <- c("segment", paste0("year_", 1:num_years))
write.csv(customer_counts_wide,
          file.path(output_dir, "customer_counts.csv"),
          row.names = FALSE)

# ===== 2. PRODUCT IMPACT =====
# Bottom-up: kw_per_customer (fixed kW reduction, no load shape)
# Top-down:  pct_of_load (% of baseline whole-home/facility load; sector specifies which baseline)
res_products <- c("byo_thermostat", "water_heater_dlc", "peak_time_rebate")
res_segments <- c("single_family", "multi_family", "manufactured_home")

res_impact_df <- expand.grid(product = res_products, segment = res_segments,
                              stringsAsFactors = FALSE)
res_impact_df$impact_value <- ifelse(
  res_impact_df$product == "byo_thermostat",   1.2,
  ifelse(res_impact_df$product == "water_heater_dlc", 0.3,
  0.06)  # PTR: 6% of baseline load
)
res_impact_df$impact_unit <- ifelse(
  res_impact_df$product == "peak_time_rebate", "pct_of_load", "kw_per_customer"
)
res_impact_df$sector <- ifelse(
  res_impact_df$product == "peak_time_rebate", "residential", NA_character_
)

ind_impact_df <- data.frame(
  product      = "industrial_curtailment",
  segment      = "industrial",
  impact_value = 0.30,
  impact_unit  = "pct_of_load",
  sector       = "industrial",
  stringsAsFactors = FALSE
)

product_impact_df <- rbind(res_impact_df, ind_impact_df)

write.csv(product_impact_df,
          file.path(output_dir, "product_impact.csv"),
          row.names = FALSE)

# ===== 2b. ANNUAL SALES (for top-down baseline derivation, year-varying) =====
# Base-year kWh per customer by end-use; annual growth rates reflect efficiency trends
kwh_per_customer <- list(
  single_family     = c(hvac=8000, water_heating=4000, lighting=1200, appliances=3000, other=1800),
  multi_family      = c(hvac=5000, water_heating=3000, lighting=800,  appliances=2000, other=1200),
  manufactured_home = c(hvac=7000, water_heating=3500, lighting=1000, appliances=2500, other=1500)
)
# Annual growth rates by end-use (negative = efficiency improvement)
eu_growth_rates <- c(hvac=0.005, water_heating=0.002, lighting=-0.02, appliances=0.003, other=0.005)

res_sales_df <- do.call(rbind, lapply(1:num_years, function(yr) {
  do.call(rbind, lapply(names(kwh_per_customer), function(seg) {
    do.call(rbind, lapply(names(kwh_per_customer[[seg]]), function(eu) {
      kwh_base     <- kwh_per_customer[[seg]][[eu]]
      kwh_yr       <- kwh_base * (1 + eu_growth_rates[[eu]]) ^ (yr - 1)
      customers_yr <- round(base_counts[[seg]] * (1 + growth_rates[[seg]]) ^ (yr - 1))
      data.frame(
        year       = yr,
        sector     = "residential",
        segment    = seg,
        end_use    = eu,
        annual_kwh = kwh_yr * customers_yr,
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

# Industrial annual sales: base-year kWh per facility by end-use
ind_kwh_per_customer <- c(ind_process = 120000, ind_hvac = 30000, ind_lighting = 10000)
ind_eu_growth_rates  <- c(ind_process = 0.002,  ind_hvac = 0.003, ind_lighting = -0.015)

ind_sales_df <- do.call(rbind, lapply(1:num_years, function(yr) {
  do.call(rbind, lapply(names(ind_kwh_per_customer), function(eu) {
    kwh_base     <- ind_kwh_per_customer[[eu]]
    kwh_yr       <- kwh_base * (1 + ind_eu_growth_rates[[eu]]) ^ (yr - 1)
    customers_yr <- round(base_counts[["industrial"]] * (1 + growth_rates[["industrial"]]) ^ (yr - 1))
    data.frame(
      year       = yr,
      sector     = "industrial",
      segment    = "industrial",
      end_use    = eu,
      annual_kwh = kwh_yr * customers_yr,
      stringsAsFactors = FALSE
    )
  }))
}))

annual_sales_df <- rbind(res_sales_df, ind_sales_df)
annual_sales_wide <- reshape(annual_sales_df,
                              idvar = c("sector", "segment", "end_use"),
                              timevar = "year", direction = "wide")
names(annual_sales_wide) <- c("sector", "segment", "end_use", paste0("year_", 1:num_years))
write.csv(annual_sales_wide,
          file.path(output_dir, "annual_sales.csv"),
          row.names = FALSE)

# ===== 3. PARTICIPATION & ADOPTION RATES =====
res_participation_df <- expand.grid(product = res_products, segment = res_segments)
res_participation_df$max_adoption_rate <- c(
  0.30, 0.28, 0.25,    # byo_thermostat
  0.25, 0.23, 0.20,    # water_heater_dlc
  0.20, 0.18, 0.15     # peak_time_rebate
)
res_participation_df$annual_ramp_rate  <- 0.15
res_participation_df$event_optout_rate <- 0.08
res_participation_df$first_year_adoption <- 0.03

ind_participation_df <- data.frame(
  product              = "industrial_curtailment",
  segment              = "industrial",
  max_adoption_rate    = 0.60,
  annual_ramp_rate     = 0.20,
  event_optout_rate    = 0.05,
  first_year_adoption  = 0.10
)

participation_adoption_df <- rbind(res_participation_df, ind_participation_df)

write.csv(participation_adoption_df,
          file.path(output_dir, "participation_adoption.csv"),
          row.names = FALSE)

# ===== 4. COSTS =====
res_costs_df <- data.frame(
  product                               = res_products,
  program_setup_cost                    = c(500000, 400000, 300000),
  hardware_cost_per_new_participant     = c(300,    250,    0),
  marketing_cost_per_new_participant    = c(50,     50,     75),
  onetime_incentive_per_new_participant = c(100,    75,     50),
  annual_incentive_per_participant      = c(25,     20,     15),
  om_admin_cost_annual_fixed            = c(50000,  40000,  30000),
  incentive_unit                        = "per_participant",
  annual_incentive_per_kw_pledged       = 0
)

ind_costs_df <- data.frame(
  product                               = "industrial_curtailment",
  program_setup_cost                    = 200000,
  hardware_cost_per_new_participant     = 0,
  marketing_cost_per_new_participant    = 100,
  onetime_incentive_per_new_participant = 0,
  annual_incentive_per_participant      = 0,
  om_admin_cost_annual_fixed            = 25000,
  incentive_unit                        = "per_kw_pledged",
  annual_incentive_per_kw_pledged       = 15
)

costs_df <- rbind(res_costs_df, ind_costs_df)

write.csv(costs_df,
          file.path(output_dir, "costs.csv"),
          row.names = FALSE)

# ===== 5. ELIGIBILITY MATRIX (year-varying: gradual increase as smart devices penetrate) =====
base_eligibility <- c(
  0.95, 0.80, 0.70,    # byo_thermostat
  0.90, 0.75, 0.65,    # water_heater_dlc
  1.00, 1.00, 1.00     # peak_time_rebate
)
annual_eligibility_growth <- c(
  0.005, 0.005, 0.005,  # byo_thermostat: +0.5%/yr, capped at 1.0
  0.004, 0.004, 0.004,  # water_heater_dlc: +0.4%/yr, capped at 1.0
  0.000, 0.000, 0.000   # peak_time_rebate: already 100%
)
res_elig_df <- expand.grid(product = res_products, segment = res_segments)
res_elig_df$base_share    <- base_eligibility
res_elig_df$annual_growth <- annual_eligibility_growth

res_eligibility_df <- do.call(rbind, lapply(1:num_years, function(yr) {
  df <- res_elig_df
  df$year              <- yr
  df$eligibility_share <- pmin(df$base_share + df$annual_growth * (yr - 1), 1.0)
  df[, c("year", "product", "segment", "eligibility_share")]
}))

# Industrial: all facilities eligible from year 1
ind_eligibility_df <- data.frame(
  year             = 1:num_years,
  product          = "industrial_curtailment",
  segment          = "industrial",
  eligibility_share = 1.0
)

eligibility_matrix_df <- rbind(res_eligibility_df, ind_eligibility_df)
eligibility_wide <- reshape(eligibility_matrix_df,
                             idvar = c("product", "segment"),
                             timevar = "year", direction = "wide")
names(eligibility_wide) <- c("product", "segment", paste0("year_", 1:num_years))
write.csv(eligibility_wide,
          file.path(output_dir, "eligibility_matrix.csv"),
          row.names = FALSE)

# ===== 6. GLOBAL INPUTS =====
global_inputs_df <- data.frame(
  parameter = c("line_loss_rate", "discount_rate"),
  value     = c(0.07,             0.05)
)
write.csv(global_inputs_df,
          file.path(output_dir, "global_inputs.csv"),
          row.names = FALSE)

# ===== 7. PEAK PERIOD DEFINITIONS =====
# Columns: season, month_start, month_end, peak_start_hour, peak_end_hour (24-hr, inclusive)
# Multiple rows per season = multiple daily windows.
# month_start > month_end indicates a year-wrapping season (e.g., Dec-Feb).
peak_periods_df <- data.frame(
  season           = c("winter", "winter", "summer", "shoulder", "shoulder"),
  month_start      = c(12,       12,        6,         3,          3),
  month_end        = c(2,         2,        8,         5,          5),
  peak_start_hour  = c(8,        17,       16,         8,         17),
  peak_end_hour    = c(11,       20,       19,         10,        19)
)
write.csv(peak_periods_df,
          file.path(output_dir, "peak_periods.csv"),
          row.names = FALSE)

# ===== VERIFICATION =====
cat("✓ All input template CSV files created successfully!\n\n")

for (file in c("customer_counts.csv", "product_impact.csv", "annual_sales.csv",
               "participation_adoption.csv", "costs.csv",
               "eligibility_matrix.csv", "global_inputs.csv", "peak_periods.csv")) {
  cat("---", file, "---\n")
  df <- read.csv(file.path(output_dir, file))
  print(df)
  cat("\n")
}

cat("✓ Files saved to:", output_dir, "\n")