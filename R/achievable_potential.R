# R/achievable_potential.R
# Calculate achievable potential for DR products

#' Calculate Adoption Trajectory
#'
#' @param first_year_adoption Initial adoption rate (0-1)
#' @param annual_ramp_rate Annual increase as a fraction of max_adoption_rate (0-1)
#' @param max_adoption_rate Maximum adoption rate (0-1)
#' @param num_years Number of years
#' @return vector of adoption rates by year
calculate_adoption_trajectory <- function(first_year_adoption, annual_ramp_rate,
                                          max_adoption_rate, num_years) {
  adoption    <- rep(0, num_years)
  adoption[1] <- first_year_adoption

  for (year in 2:num_years) {
    adoption[year] <- min(adoption[year - 1] + annual_ramp_rate * max_adoption_rate,
                          max_adoption_rate)
  }

  return(adoption)
}

#' Calculate Enrollment by Year, Product, and Segment
#'
#' Shared helper called by calculate_achievable_potential() and calculate_costs().
#' Returns enrolled customers (eligible × adoption × (1 - opt-out)) for every
#' product × segment × year combination.
#'
#' @param inputs List from load_all_inputs()
#' @param num_years Number of years
#' @return data frame with year, product, segment, eligible_count,
#'   adoption_rate, event_optout_rate, enrolled_customers
#' @export
calculate_enrollment <- function(inputs, num_years = 10) {
  has_geo <- "geography" %in% names(inputs$customers)

  elig_key <- if (has_geo && "geography" %in% names(inputs$eligibility)) {
    c("year", "geography", "segment")
  } else {
    c("year", "segment")
  }
  base_df <- merge(inputs$customers, inputs$eligibility, by = elig_key)
  base_df <- base_df[base_df$year <= num_years, ]
  base_df$eligible_count <- base_df$customer_count * base_df$eligibility_share
  base_df <- merge(base_df,
                   inputs$product_impact[, c("product", "segment")],
                   by = c("product", "segment"))

  unique_combos <- unique(base_df[, c("product", "segment")])

  # Pre-index participation by product×segment key to avoid an O(N×P) scan per combo.
  part_key  <- paste(inputs$participation$product, inputs$participation$segment)
  part_list <- split(inputs$participation, part_key)

  adoption_lookup <- do.call(rbind, lapply(seq_len(nrow(unique_combos)), function(i) {
    key      <- paste(unique_combos$product[i], unique_combos$segment[i])
    part_row <- part_list[[key]]
    data.frame(
      product           = unique_combos$product[i],
      segment           = unique_combos$segment[i],
      year              = seq_len(num_years),
      adoption_rate     = calculate_adoption_trajectory(
        first_year_adoption = part_row$first_year_adoption,
        annual_ramp_rate    = part_row$annual_ramp_rate,
        max_adoption_rate   = part_row$max_adoption_rate,
        num_years           = num_years
      ),
      event_optout_rate = part_row$event_optout_rate,
      stringsAsFactors  = FALSE
    )
  }))

  geo_cols  <- if (has_geo) "geography" else character(0)
  base_cols <- c("year", geo_cols, "product", "segment", "eligible_count")

  result <- merge(base_df[, base_cols],
                  adoption_lookup,
                  by = c("product", "segment", "year"))
  result$enrolled_customers <- result$eligible_count *
                                result$adoption_rate *
                                (1 - result$event_optout_rate)

  out_cols <- c("year", geo_cols, "product", "segment",
                "eligible_count", "adoption_rate", "event_optout_rate", "enrolled_customers")
  result[, out_cols]
}

#' Calculate Achievable Potential
#'
#' Two tracks determined by impact_unit in product_impact.csv:
#'   kw_per_customer — bottom-up: flat kW reduction per enrolled customer (no load shape)
#'   pct_of_load     — top-down:  impact_value × hourly baseline load per customer
#'
#' @param inputs List from load_all_inputs()
#' @param num_years Number of years to model (default 10)
#' @return data frame with achievable_mw by year, product, segment, hour
#' @export
calculate_achievable_potential <- function(inputs, enrollment, num_years = 10) {
  has_geo  <- "geography" %in% names(enrollment)
  geo_cols <- if (has_geo) "geography" else character(0)

  pi    <- inputs$product_impact
  bu_df <- pi[pi$impact_unit == "kw_per_customer", c("product", "segment", "impact_value")]
  td_df <- pi[pi$impact_unit == "pct_of_load",     c("product", "segment", "impact_value", "sector")]

  enroll_key  <- c("year", geo_cols, "product", "segment")
  enroll_cols <- c(enroll_key, "enrolled_customers")
  out_cols    <- c("year", geo_cols, "product", "segment", "hour", "achievable_mw")

  results <- list()

  # ── Bottom-up: flat kW per enrolled customer, no load shape ──────────────────
  if (nrow(bu_df) > 0) {
    bu_expanded <- merge(
      data.frame(hour = 1:8760),
      bu_df,
      by = character(0)
    )
    bu_expanded <- merge(bu_expanded, enrollment[, enroll_cols], by = c("product", "segment"))
    bu_expanded$achievable_mw <- bu_expanded$enrolled_customers *
                                   bu_expanded$impact_value / 1000
    results$bottom_up <- bu_expanded[, out_cols]
  }

  # ── Top-down: impact_value (%) × baseline load per customer ──────────────────
  if (nrow(td_df) > 0) {
    td_expanded <- merge(
      data.frame(hour = 1:8760),
      td_df,
      by = character(0)
    )
    td_expanded <- merge(td_expanded, enrollment[, enroll_cols], by = c("product", "segment"))
    td_expanded <- merge(
      td_expanded,
      inputs$baseline_load[, c("year", "sector", "segment", "hour", "baseline_mw_per_customer")],
      by = c("year", "sector", "segment", "hour")
    )
    td_expanded$achievable_mw <- td_expanded$enrolled_customers *
                                   td_expanded$impact_value *
                                   td_expanded$baseline_mw_per_customer
    results$top_down <- td_expanded[, out_cols]
  }

  result <- do.call(rbind, results)

  # Convert customer-side MW to generation-equivalent MW using line loss rate
  line_loss_factor <- 1 / (1 - inputs$global$line_loss_rate)
  result$achievable_mw <- result$achievable_mw * line_loss_factor

  return(result)
}
