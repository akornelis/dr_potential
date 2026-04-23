# R/aggregation.R
# Aggregation and summary functions

#' Aggregate Potential by Product
#'
#' @param potential_df Data frame from calculate_technical_potential or calculate_achievable_potential
#' @param by_hour If TRUE, returns by hour. If FALSE, returns annual totals
#' @return Aggregated data frame
#' @export
aggregate_by_product <- function(potential_df, by_hour = TRUE) {
  if (by_hour) {
    result <- aggregate(
      potential_df[, grep("_mw$", names(potential_df))],
      by = list(year = potential_df$year,
                product = potential_df$product,
                hour = potential_df$hour),
      FUN = sum
    )
  } else {
    result <- aggregate(
      potential_df[, grep("_mw$", names(potential_df))],
      by = list(year = potential_df$year,
                product = potential_df$product),
      FUN = sum
    )
  }
  return(result)
}

#' Aggregate Potential by Segment
#'
#' @param potential_df Data frame from calculate_technical_potential or calculate_achievable_potential
#' @param by_hour If TRUE, returns by hour. If FALSE, returns annual totals
#' @return Aggregated data frame
#' @export
aggregate_by_segment <- function(potential_df, by_hour = TRUE) {
  if (by_hour) {
    result <- aggregate(
      potential_df[, grep("_mw$", names(potential_df))],
      by = list(year = potential_df$year,
                segment = potential_df$segment,
                hour = potential_df$hour),
      FUN = sum
    )
  } else {
    result <- aggregate(
      potential_df[, grep("_mw$", names(potential_df))],
      by = list(year = potential_df$year,
                segment = potential_df$segment),
      FUN = sum
    )
  }
  return(result)
}

#' Get Peak Day Summary
#'
#' Returns MW for a typical peak day
#'
#' @param potential_df Data frame with hourly potential
#' @param peak_day Day of year (1-365). Default 1 (winter peak)
#' @return Data frame with hour of day (1-24) and MW
#' @export
get_peak_day <- function(potential_df, peak_day = 1, year = 1) {
  # Calculate hour numbers for the peak day
  start_hour <- (peak_day - 1) * 24 + 1
  end_hour <- start_hour + 23

  peak_subset <- potential_df[
    potential_df$year == year &
    potential_df$hour >= start_hour &
    potential_df$hour <= end_hour, ]

  result <- aggregate(
    peak_subset[, grep("_mw$", names(peak_subset))],
    by = list(hour_of_day = (peak_subset$hour - start_hour + 1)),
    FUN = sum
  )

  return(result)
}

# Shared helper: aggregate MW values by a grouping, return total_gwh / peak_mw / avg_mw.
# include_min adds a min_mw column (used by get_summary_stats for annual output).
.agg_stats <- function(vals, by, include_min = FALSE) {
  n      <- length(by)
  result <- data.frame(
    aggregate(vals, by = by, FUN = sum),
    peak_mw = aggregate(vals, by = by, FUN = max)[[n + 1]],
    avg_mw  = aggregate(vals, by = by, FUN = mean)[[n + 1]]
  )
  if (include_min) result$min_mw <- aggregate(vals, by = by, FUN = min)[[n + 1]]
  names(result)[n + 1] <- "total_gwh"
  result$total_gwh <- result$total_gwh / 1000
  result
}

#' Generate Summary Statistics
#'
#' @param potential_df Data frame from calculate_*_potential
#' @return Data frame with annual summaries (total_gwh, peak_mw, avg_mw, min_mw)
#' @export
get_summary_stats <- function(potential_df) {
  mw_col   <- grep("_mw$", names(potential_df))[1]
  vals     <- potential_df[[mw_col]]
  grp_cols <- c("year", "product", "segment")
  if ("geography" %in% names(potential_df)) grp_cols <- c("geography", grp_cols)
  by <- lapply(grp_cols, function(c) potential_df[[c]])
  names(by) <- grp_cols
  .agg_stats(vals, by, include_min = TRUE)
}

#' Generate Seasonal Summary Statistics
#'
#' Summarises peak-hour achievable potential by season, product, and segment.
#'
#' @param potential_df   data frame from calculate_achievable_potential()
#' @param season_lookup  data frame from derive_season_lookup() (hour, season)
#' @return data frame with year, product, segment, season, total_gwh, peak_mw, avg_mw
#' @export
get_seasonal_stats <- function(potential_df, season_lookup) {
  df <- merge(potential_df,
              season_lookup[!is.na(season_lookup$season), ],
              by = "hour")
  mw_col   <- grep("_mw$", names(df))[1]
  vals     <- df[[mw_col]]
  grp_cols <- c("year", "product", "segment", "season")
  if ("geography" %in% names(df)) grp_cols <- c("geography", grp_cols)
  by <- lapply(grp_cols, function(c) df[[c]])
  names(by) <- grp_cols
  .agg_stats(vals, by)
}

#' Export to CSV
#'
#' @param df Data frame to export
#' @param filename Output filename
#' @param output_dir Directory to save to
#' @export
export_results <- function(df, filename, output_dir = "outputs/") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(df, file.path(output_dir, filename), row.names = FALSE)
  cat("✓ Exported:", file.path(output_dir, filename), "\n")
}