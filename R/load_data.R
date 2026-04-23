# R/load_data.R
# Data loading and validation functions for DR model

# Convert a wide year-column CSV back to long format.
# id_cols  — character vector of non-year column names
# value_col — name to give the value column in the result
wide_to_long <- function(df, id_cols, value_col) {
  year_cols <- grep("^year_", names(df), value = TRUE)
  years     <- as.integer(sub("^year_", "", year_cols))
  do.call(rbind, lapply(seq_along(year_cols), function(i) {
    out           <- df[, id_cols, drop = FALSE]
    out$year      <- years[i]
    out[[value_col]] <- df[[year_cols[i]]]
    out
  }))
}

#' Load and Validate Customer Counts
#' @param file_path Path to customer_counts.csv
#' @return data frame with year, segment, and customer_count columns
#' @export
load_customer_counts <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  if (!"segment" %in% names(df))
    stop("Missing 'segment' column in customer_counts.csv")
  if (!any(grepl("^year_", names(df))))
    stop("No year columns (year_N) found in customer_counts.csv")

  id_cols <- if ("geography" %in% names(df)) c("geography", "segment") else "segment"
  df <- wide_to_long(df, id_cols = id_cols, value_col = "customer_count")

  if (any(df$customer_count < 0))
    stop("Customer counts cannot be negative")
  if (any(is.na(df$customer_count)))
    stop("Missing values in customer_count")

  return(df)
}

#' Load and Validate Product Impact Assumptions
#' @param file_path Path to product_impact.csv
#' @return data frame with product, segment, impact_value, impact_unit, and optional sector
#' @export
load_product_impact <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_cols <- c("product", "segment", "impact_value", "impact_unit")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in product_impact.csv")
  }

  if (any(df$impact_value < 0)) {
    stop("Impact values cannot be negative")
  }
  if (any(is.na(df$impact_value))) {
    stop("Missing values in impact_value")
  }

  valid_units <- c("kw_per_customer", "pct_of_load")
  if (!all(df$impact_unit %in% valid_units)) {
    stop("Invalid impact_unit. Must be 'kw_per_customer' or 'pct_of_load'")
  }

  # sector column required for pct_of_load products
  if ("sector" %in% names(df)) {
    td_rows <- df$impact_unit == "pct_of_load"
    if (any(is.na(df$sector[td_rows]))) {
      stop("sector must be specified for all pct_of_load products in product_impact.csv")
    }
  } else if (any(df$impact_unit == "pct_of_load")) {
    stop("sector column required in product_impact.csv when impact_unit = 'pct_of_load'")
  }

  return(df)
}

#' Load and Validate Annual Sales
#' @param file_path Path to annual_sales.csv
#' @return data frame with sector, segment, end_use, annual_kwh
#' @export
load_annual_sales <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  id_cols <- c("sector", "segment", "end_use")
  if (!all(id_cols %in% names(df)))
    stop("Missing required id columns in annual_sales.csv: ",
         paste(setdiff(id_cols, names(df)), collapse = ", "))
  if (!any(grepl("^year_", names(df))))
    stop("No year columns (year_N) found in annual_sales.csv")

  df <- wide_to_long(df, id_cols = id_cols, value_col = "annual_kwh")

  if (any(df$annual_kwh < 0)) stop("annual_kwh cannot be negative")
  if (any(is.na(df$annual_kwh))) stop("Missing values in annual_kwh")

  return(df)
}

#' Load and Validate Combined End-Use Load Shapes
#'
#' Reads end_use_shapes.csv: wide format with columns end_use, hour, year_1...year_N.
#' Returns a long data frame: end_use, year, hour, energy_fraction.
#' Shapes may vary by year; each end_use × year combination must sum to 1.0.
#'
#' @param file_path Path to end_use_shapes.csv
#' @return data frame with end_use, year, hour, energy_fraction
#' @export
load_end_use_shapes <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_id <- c("end_use", "hour")
  if (!all(required_id %in% names(df)))
    stop("Missing required columns in end_use_shapes.csv: ",
         paste(setdiff(required_id, names(df)), collapse = ", "))
  if (!any(grepl("^year_", names(df))))
    stop("No year columns (year_N) found in end_use_shapes.csv")

  df <- wide_to_long(df, id_cols = c("end_use", "hour"), value_col = "energy_fraction")

  if (any(df$energy_fraction < 0))
    stop("energy_fraction values cannot be negative in end_use_shapes.csv")

  counts <- aggregate(energy_fraction ~ end_use + year, df, length)
  bad    <- counts[counts$energy_fraction != 8760, ]
  if (nrow(bad) > 0)
    stop("Each end_use × year must have exactly 8760 rows in end_use_shapes.csv: ",
         paste(paste(bad$end_use, bad$year, sep = "/"), collapse = ", "))

  sums <- aggregate(energy_fraction ~ end_use + year, df, sum)
  bad  <- sums[abs(sums$energy_fraction - 1.0) > 0.001, ]
  if (nrow(bad) > 0)
    stop("energy_fraction must sum to 1.0 per end_use × year in end_use_shapes.csv: ",
         paste(paste(bad$end_use, bad$year, sep = "/"), collapse = ", "))

  df
}

#' Derive Hourly Baseline Load Per Customer
#'
#' Converts annual energy sales by end-use into an 8760 hourly MW profile
#' per customer for each sector × segment combination.
#'
#' @param annual_sales  data frame from load_annual_sales()
#' @param end_use_shapes data frame from load_end_use_shapes() (end_use, year, hour, energy_fraction)
#' @param customers     data frame from load_customer_counts() (year-varying)
#' @return data frame with sector, segment, hour, baseline_mw_per_customer
#' @export
derive_baseline_load <- function(annual_sales, end_use_shapes, customers) {
  cust_by_seg <- if ("geography" %in% names(customers)) {
    aggregate(customer_count ~ year + segment, customers, sum)
  } else {
    customers[, c("year", "segment", "customer_count")]
  }

  sales_shapes <- merge(annual_sales, end_use_shapes, by = c("year", "end_use"))
  sales_shapes <- merge(sales_shapes, cust_by_seg, by = c("year", "segment"))

  # hourly MW per customer = (annual_kwh / customers) × energy_fraction / 1000
  sales_shapes$hour_mw <- (sales_shapes$annual_kwh / sales_shapes$customer_count) *
                           sales_shapes$energy_fraction / 1000

  # Sum across end-uses → whole-home baseline per year
  baseline <- aggregate(
    sales_shapes$hour_mw,
    by = list(year    = sales_shapes$year,
              sector  = sales_shapes$sector,
              segment = sales_shapes$segment,
              hour    = sales_shapes$hour),
    FUN = sum
  )
  names(baseline)[5] <- "baseline_mw_per_customer"
  return(baseline)
}

#' Load and Validate Participation/Adoption Rates
#' @param file_path Path to participation_adoption.csv
#' @return data frame with product, segment, and rate columns
#' @export
load_participation_adoption <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_cols <- c("product", "segment", "max_adoption_rate",
                     "annual_ramp_rate", "event_optout_rate", "first_year_adoption")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in participation_adoption.csv")
  }

  # Validate rates (must be between 0 and 1)
  rate_cols <- c("max_adoption_rate", "annual_ramp_rate", "event_optout_rate", "first_year_adoption")
  for (col in rate_cols) {
    if (any(df[[col]] < 0) || any(df[[col]] > 1)) {
      stop(paste(col, "must be between 0 and 1"))
    }
    if (any(is.na(df[[col]]))) {
      stop(paste("Missing values in", col))
    }
  }

  return(df)
}

#' Load and Validate Costs
#' @param file_path Path to costs.csv
#' @return data frame with product, segment, and cost columns
#' @export
load_costs <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_cols <- c("product",
                     "program_setup_cost",
                     "hardware_cost_per_new_participant",
                     "marketing_cost_per_new_participant",
                     "onetime_incentive_per_new_participant",
                     "annual_incentive_per_participant",
                     "om_admin_cost_annual_fixed",
                     "incentive_unit",
                     "annual_incentive_per_kw_pledged")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in costs.csv: ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }

  cost_cols <- setdiff(required_cols, c("product", "incentive_unit"))
  for (col in cost_cols) {
    if (any(df[[col]] < 0))    stop(paste(col, "cannot be negative"))
    if (any(is.na(df[[col]]))) stop(paste("Missing values in", col))
  }

  valid_units <- c("per_participant", "per_kw_pledged")
  if (!all(df$incentive_unit %in% valid_units)) {
    stop("incentive_unit must be 'per_participant' or 'per_kw_pledged'")
  }

  return(df)
}

#' Load and Validate Eligibility Matrix
#' @param file_path Path to eligibility_matrix.csv
#' @return data frame with year, product, segment, eligibility_share
#' @export
load_eligibility_matrix <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_id <- c("product", "segment")
  if (!all(required_id %in% names(df)))
    stop("Missing required id columns in eligibility_matrix.csv: ",
         paste(setdiff(required_id, names(df)), collapse = ", "))
  if (!any(grepl("^year_", names(df))))
    stop("No year columns (year_N) found in eligibility_matrix.csv")

  id_cols <- intersect(c("geography", "product", "segment"), names(df))
  df <- wide_to_long(df, id_cols = id_cols, value_col = "eligibility_share")

  if (any(df$eligibility_share < 0) || any(df$eligibility_share > 1))
    stop("Eligibility shares must be between 0 and 1")
  if (any(is.na(df$eligibility_share)))
    stop("Missing values in eligibility_share")

  return(df)
}

#' Load and Validate Global Inputs
#' @param file_path Path to global_inputs.csv
#' @return named list with scalar parameters (line_loss_rate, discount_rate)
#' @export
load_global_inputs <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_cols <- c("parameter", "value")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in global_inputs.csv: ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }

  required_params <- c("line_loss_rate", "discount_rate")
  missing <- setdiff(required_params, df$parameter)
  if (length(missing) > 0) {
    stop("Missing required parameters in global_inputs.csv: ",
         paste(missing, collapse = ", "))
  }

  params <- setNames(as.list(df$value), df$parameter)

  if (params$line_loss_rate < 0 || params$line_loss_rate >= 1)
    stop("line_loss_rate must be between 0 and 1 (exclusive)")
  if (params$discount_rate < 0)
    stop("discount_rate cannot be negative")

  params
}

#' Load and Validate Peak Period Definitions
#' @param file_path Path to peak_periods.csv
#' @return data frame with season, month_start, month_end, peak_start_hour, peak_end_hour
#' @export
load_peak_periods <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  required_cols <- c("season", "month_start", "month_end", "peak_start_hour", "peak_end_hour")
  if (!all(required_cols %in% names(df))) {
    stop("Missing required columns in peak_periods.csv: ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }
  if (any(df$month_start < 1) || any(df$month_start > 12) ||
      any(df$month_end   < 1) || any(df$month_end   > 12)) {
    stop("month_start and month_end must be between 1 and 12")
  }
  if (any(df$peak_start_hour < 1) || any(df$peak_start_hour > 24) ||
      any(df$peak_end_hour   < 1) || any(df$peak_end_hour   > 24)) {
    stop("peak_start_hour and peak_end_hour must be between 1 and 24")
  }
  if (any(df$peak_start_hour > df$peak_end_hour)) {
    stop("peak_start_hour must be <= peak_end_hour (overnight windows not supported)")
  }

  df
}

#' Derive an 8760-row data frame mapping each hour to its season (NA = off-peak)
#'
#' @param peak_periods data frame from load_peak_periods()
#' @return data frame with columns hour (1–8760) and season (character, NA for off-peak hours)
#' @export
derive_season_lookup <- function(peak_periods) {
  hours         <- 1:8760
  hour_of_day   <- ((hours - 1L) %% 24L) + 1L
  day_of_year   <- ((hours - 1L) %/% 24L) + 1L
  month_starts  <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366)
  month_of_year <- findInterval(day_of_year, month_starts)

  season <- rep(NA_character_, 8760)
  for (i in seq_len(nrow(peak_periods))) {
    p <- peak_periods[i, ]
    if (p$month_start <= p$month_end) {
      in_season <- month_of_year >= p$month_start & month_of_year <= p$month_end
    } else {
      # Year-wrapping season (e.g., month_start=12, month_end=2 → Dec–Feb)
      in_season <- month_of_year >= p$month_start | month_of_year <= p$month_end
    }
    in_window <- hour_of_day >= p$peak_start_hour & hour_of_day <= p$peak_end_hour
    season[in_season & in_window] <- as.character(p$season)
  }

  data.frame(hour = hours, season = season, stringsAsFactors = FALSE)
}

#' Derive an 8760-length logical vector flagging peak hours
#'
#' @param peak_periods data frame from load_peak_periods()
#' @return logical vector of length 8760; TRUE = peak hour
#' @export
derive_peak_hours <- function(peak_periods) {
  !is.na(derive_season_lookup(peak_periods)$season)
}

validate_inputs <- function(inputs) {
  pi_combos   <- unique(inputs$product_impact[, c("product", "segment")])
  part_combos <- unique(inputs$participation[, c("product", "segment")])
  elig_combos <- unique(inputs$eligibility[, c("product", "segment")])

  n_pi   <- nrow(pi_combos)
  n_part <- nrow(merge(pi_combos, part_combos, by = c("product", "segment")))
  if (n_part < n_pi)
    stop("Some product×segment combos in product_impact have no participation row")

  n_elig <- nrow(merge(pi_combos, elig_combos, by = c("product", "segment")))
  if (n_elig < n_pi)
    stop("Some product×segment combos in product_impact have no eligibility row")

  miss_segs <- setdiff(unique(inputs$customers$segment), unique(inputs$eligibility$segment))
  if (length(miss_segs) > 0)
    stop("Segments in customer_counts not in eligibility_matrix: ",
         paste(miss_segs, collapse = ", "))

  missing_shapes <- setdiff(unique(inputs$annual_sales$end_use), unique(inputs$end_use_shapes$end_use))
  if (length(missing_shapes) > 0)
    stop("End uses in annual_sales have no load shape: ",
         paste(missing_shapes, collapse = ", "))

  td_sectors     <- unique(inputs$product_impact$sector[
                             inputs$product_impact$impact_unit == "pct_of_load"])
  missing_sectors <- setdiff(td_sectors, unique(inputs$annual_sales$sector))
  if (length(missing_sectors) > 0)
    stop("Sectors for pct_of_load products not in annual_sales: ",
         paste(missing_sectors, collapse = ", "))

  invisible(TRUE)
}

#' Load all inputs and return a list
#' @param data_dir Path to data directory containing all CSV files
#' @return list with all loaded and validated data (including derived baseline_load)
#' @export
load_all_inputs <- function(data_dir) {
  end_use_shapes <- load_end_use_shapes(file.path(data_dir, "end_use_shapes.csv"))
  customers      <- load_customer_counts(file.path(data_dir, "customer_counts.csv"))
  annual_sales   <- load_annual_sales(file.path(data_dir, "annual_sales.csv"))
  params         <- load_global_inputs(file.path(data_dir, "global_inputs.csv"))
  peak_periods   <- load_peak_periods(file.path(data_dir, "peak_periods.csv"))
  season_lookup  <- derive_season_lookup(peak_periods)

  inputs <- list(
    customers      = customers,
    product_impact = load_product_impact(file.path(data_dir, "product_impact.csv")),
    end_use_shapes = end_use_shapes,
    participation  = load_participation_adoption(file.path(data_dir, "participation_adoption.csv")),
    costs          = load_costs(file.path(data_dir, "costs.csv")),
    eligibility    = load_eligibility_matrix(file.path(data_dir, "eligibility_matrix.csv")),
    annual_sales   = annual_sales,   # retained for inspection/export; used only by derive_baseline_load
    baseline_load  = derive_baseline_load(annual_sales, end_use_shapes, customers),
    global         = c(params, list(
      peak_periods  = peak_periods,
      peak_hours    = !is.na(season_lookup$season),
      season_lookup = season_lookup
    ))
  )

  validate_inputs(inputs)
  inputs
}