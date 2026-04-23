# R/costs.R
# Calculate program costs
# Depends on R/achievable_potential.R (calculate_enrollment).
# Use report.R as the entry point — it sources both files and passes enrollment explicitly.

#' Calculate Program Costs
#'
#' Returns total incentive and O&M costs by product and year.
#' enrollment must be pre-filtered to the desired planning horizon
#' (calculate_enrollment() applies the num_years filter internally).
#'
#' @param inputs     List from load_all_inputs()
#' @param enrollment data frame from calculate_enrollment()
#' @return data frame with costs by year and product
#' @export
calculate_costs <- function(inputs, enrollment) {

  # Attach sector and impact metadata for kW-pledged calculation
  enrolled_by_year <- merge(
    enrollment,
    inputs$product_impact[, c("product", "segment", "sector", "impact_unit", "impact_value")],
    by = c("product", "segment"),
    all.x = TRUE
  )

  # Average baseline MW per customer during peak hours (for pct_of_load products)
  bl <- inputs$baseline_load
  stopifnot(all(bl$hour >= 1L & bl$hour <= 8760L))
  peak_bl <- bl[inputs$global$peak_hours[bl$hour], ]
  avg_peak_baseline <- aggregate(
    peak_bl$baseline_mw_per_customer,
    by = list(year = peak_bl$year, sector = peak_bl$sector, segment = peak_bl$segment),
    FUN = mean
  )
  names(avg_peak_baseline)[4] <- "avg_peak_mw_per_customer"

  enrolled_by_year <- merge(
    enrolled_by_year,
    avg_peak_baseline,
    by = c("year", "sector", "segment"),
    all.x = TRUE
  )

  # kW pledged = enrolled × impact_pct × avg peak baseline kW/customer (0 for bottom-up)
  enrolled_by_year$enrolled_kw_pledged <- ifelse(
    !is.na(enrolled_by_year$impact_unit) & enrolled_by_year$impact_unit == "pct_of_load",
    enrolled_by_year$enrolled_customers *
      enrolled_by_year$impact_value *
      enrolled_by_year$avg_peak_mw_per_customer * 1000,
    0
  )

  # Aggregate enrolled customers and kW pledged to product level (sum across segments)
  enrolled_by_product <- aggregate(
    cbind(enrolled_customers  = enrolled_by_year$enrolled_customers,
          enrolled_kw_pledged = enrolled_by_year$enrolled_kw_pledged),
    by = list(year = enrolled_by_year$year, product = enrolled_by_year$product),
    FUN = sum
  )

  # Compute new participants per product per year (floored at 0)
  enrolled_by_product <- enrolled_by_product[order(enrolled_by_product$product,
                                                    enrolled_by_product$year), ]
  enrolled_by_product$new_participants <- ave(
    enrolled_by_product$enrolled_customers,
    enrolled_by_product$product,
    FUN = function(x) pmax(0, c(x[1], diff(x)))
  )

  # Merge with cost inputs
  enrolled_by_product <- merge(enrolled_by_product, inputs$costs, by = "product")

  # Calculate each cost component
  enrolled_by_product$program_setup_cost_total <-
    ifelse(enrolled_by_product$year == 1, enrolled_by_product$program_setup_cost, 0)

  enrolled_by_product$hardware_cost_total <-
    enrolled_by_product$new_participants * enrolled_by_product$hardware_cost_per_new_participant

  enrolled_by_product$marketing_cost_total <-
    enrolled_by_product$new_participants * enrolled_by_product$marketing_cost_per_new_participant

  enrolled_by_product$onetime_incentive_total <-
    enrolled_by_product$new_participants * enrolled_by_product$onetime_incentive_per_new_participant

  enrolled_by_product$annual_incentive_total <- ifelse(
    enrolled_by_product$incentive_unit == "per_kw_pledged",
    enrolled_by_product$enrolled_kw_pledged * enrolled_by_product$annual_incentive_per_kw_pledged,
    enrolled_by_product$enrolled_customers  * enrolled_by_product$annual_incentive_per_participant
  )

  enrolled_by_product$om_admin_cost_total <- enrolled_by_product$om_admin_cost_annual_fixed

  enrolled_by_product$total_cost <-
    enrolled_by_product$program_setup_cost_total +
    enrolled_by_product$hardware_cost_total      +
    enrolled_by_product$marketing_cost_total     +
    enrolled_by_product$onetime_incentive_total  +
    enrolled_by_product$annual_incentive_total   +
    enrolled_by_product$om_admin_cost_total

  # Discount each year's costs back to year 1 (year 1 = present value)
  discount_rate <- inputs$global$discount_rate
  enrolled_by_product$discount_factor       <- 1 / (1 + discount_rate) ^ (enrolled_by_product$year - 1)
  enrolled_by_product$discounted_total_cost <- enrolled_by_product$total_cost *
                                               enrolled_by_product$discount_factor

  result <- enrolled_by_product[, c("year", "product", "enrolled_customers", "enrolled_kw_pledged",
                                    "new_participants",
                                    "program_setup_cost_total", "hardware_cost_total",
                                    "marketing_cost_total", "onetime_incentive_total",
                                    "annual_incentive_total", "om_admin_cost_total",
                                    "total_cost", "discount_factor", "discounted_total_cost")]
  return(result)
}
