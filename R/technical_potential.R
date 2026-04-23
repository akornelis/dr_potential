# R/technical_potential.R
# Calculate technical potential for DR products

#' Calculate Technical Potential
#'
#' Two tracks determined by impact_unit in product_impact.csv:
#'   kw_per_customer — bottom-up: flat kW reduction per enrolled customer (no load shape)
#'   pct_of_load     — top-down:  impact_value × hourly baseline load per customer
#'
#' @param inputs List from load_all_inputs()
#' @param num_years Number of years to model (default 10)
#' @return data frame with technical_mw by year, product, segment, hour
#' @export
calculate_technical_potential <- function(inputs, num_years = 10) {
  has_geo  <- "geography" %in% names(inputs$customers)
  geo_cols <- if (has_geo) "geography" else character(0)

  elig_key <- if (has_geo && "geography" %in% names(inputs$eligibility)) {
    c("year", "geography", "segment")
  } else {
    c("year", "segment")
  }
  eligible_customers <- merge(inputs$customers, inputs$eligibility, by = elig_key)
  eligible_customers <- eligible_customers[eligible_customers$year <= num_years, ]
  eligible_customers$eligible_count <- eligible_customers$customer_count *
                                        eligible_customers$eligibility_share

  technical_df <- merge(eligible_customers, inputs$product_impact, by = c("product", "segment"))

  bu_df <- technical_df[technical_df$impact_unit == "kw_per_customer", ]
  td_df <- technical_df[technical_df$impact_unit == "pct_of_load", ]

  results <- list()

  # ── Bottom-up: flat kW per eligible customer, no load shape ──────────────────
  if (nrow(bu_df) > 0) {
    bu_cols <- c("year", geo_cols, "product", "segment", "eligible_count", "impact_value")
    bu_expanded <- merge(
      data.frame(hour = 1:8760),
      bu_df[, bu_cols],
      by = character(0)
    )
    bu_expanded$technical_mw <- bu_expanded$eligible_count * bu_expanded$impact_value / 1000
    out_cols <- c("year", geo_cols, "product", "segment", "hour", "technical_mw")
    results$bottom_up <- bu_expanded[, out_cols]
  }

  # ── Top-down: impact_value (%) × baseline load per customer ──────────────────
  if (nrow(td_df) > 0) {
    td_cols <- c("year", geo_cols, "product", "segment", "eligible_count", "impact_value", "sector")
    td_expanded <- merge(
      data.frame(hour = 1:8760),
      td_df[, td_cols],
      by = character(0)
    )
    td_expanded <- merge(
      td_expanded,
      inputs$baseline_load[, c("year", "sector", "segment", "hour", "baseline_mw_per_customer")],
      by = c("year", "sector", "segment", "hour")
    )
    td_expanded$technical_mw <- td_expanded$eligible_count *
                                  td_expanded$impact_value *
                                  td_expanded$baseline_mw_per_customer
    out_cols <- c("year", geo_cols, "product", "segment", "hour", "technical_mw")
    results$top_down <- td_expanded[, out_cols]
  }

  result <- do.call(rbind, results)

  # Convert customer-side MW to generation-equivalent MW using line loss rate
  line_loss_factor <- 1 / (1 - inputs$global$line_loss_rate)
  result$technical_mw <- result$technical_mw * line_loss_factor

  return(result)
}
