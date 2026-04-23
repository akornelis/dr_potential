# R/reporting.R
# Reporting functions: figures for milestone years and cost curve

library(ggplot2)

# ── Label and color helpers ───────────────────────────────────────────────────

.snake_to_title <- function(x) {
  gsub("(^|\\s)(\\w)", "\\1\\U\\2",
       gsub("_", " ", x), perl = TRUE)
}

# Preferred colors for known programs — new programs get auto-assigned colors.
.preferred_colors <- c(
  "BYO Thermostat"        = "#d73027",
  "Water Heater DLC"      = "#4dac26",
  "Peak Time Rebate"      = "#7b2d8b",
  "Industrial Curtailment"= "#e08214"
)

# Preferred segment colors — new segments get auto-assigned.
.preferred_seg_colors <- c(
  "Single Family"     = "#2166ac",
  "Multi-Family"      = "#67a9cf",
  "Manufactured Home" = "#d1e5f0",
  "Industrial"        = "#f4a582"
)

.fallback_palette <- c(
  "#1f78b4","#33a02c","#e31a1c","#ff7f00",
  "#6a3d9a","#b15928","#a6cee3","#b2df8a",
  "#fb9a99","#fdbf6f","#cab2d6","#ffff99"
)

assign_palette <- function(labels, preferred = .preferred_colors) {
  labels  <- unique(labels)
  in_pref <- labels %in% names(preferred)
  result  <- setNames(rep(NA_character_, length(labels)), labels)
  result[labels[in_pref]]  <- preferred[labels[in_pref]]
  unknown <- labels[!in_pref]
  if (length(unknown) > 0) {
    avail <- setdiff(.fallback_palette, preferred[labels[in_pref]])
    result[unknown] <- head(avail, length(unknown))
  }
  result
}

.theme_dr <- function() {
  theme_minimal(base_size = 12) +
    theme(
      strip.text       = element_text(face = "bold"),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

.save_fig <- function(p, filename, output_dir, width = 9, height = 6) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(output_dir, filename)
  ggsave(path, plot = p, width = width, height = height, dpi = 150)
  cat("  Saved:", path, "\n")
  invisible(p)
}

# ── Data preparation ──────────────────────────────────────────────────────────

#' Prepare milestone data
#'
#' Joins peak MW from achievable potential with annual costs for given years.
#'
#' @param achievable  data frame from calculate_achievable_potential()
#' @param costs       data frame from calculate_costs()
#' @param summary_stats data frame from get_summary_stats()
#' @param milestone_years integer vector of planning years to highlight
#' @return list with by_segment and by_product data frames
prepare_milestone_data <- function(achievable, costs, summary_stats,
                                   milestone_years = c(5, 10)) {
  # Segment-level peak MW — used by the bar chart
  peak <- summary_stats[summary_stats$year %in% milestone_years,
                        c("year", "product", "segment", "peak_mw")]
  peak$product_label <- .snake_to_title(peak$product)
  peak$segment_label <- .snake_to_title(peak$segment)
  peak$year_label    <- paste("Year", peak$year)

  # Product-level peak MW + costs — used by the cost curve
  peak_by_product <- aggregate(peak_mw ~ year + product, peak, sum)
  cost_sub <- costs[costs$year %in% milestone_years,
                    c("year", "product", "total_cost")]
  cost_curve_data <- merge(peak_by_product, cost_sub, by = c("year", "product"))
  cost_curve_data$cost_per_kw   <- cost_curve_data$total_cost / (cost_curve_data$peak_mw * 1000)
  cost_curve_data$product_label <- .snake_to_title(cost_curve_data$product)
  cost_curve_data$year_label    <- paste("Year", cost_curve_data$year)

  list(by_segment = peak, by_product = cost_curve_data)
}

# ── Figure 1: Achievable potential at milestone years ─────────────────────────

#' Bar chart of peak MW by product and segment at milestone years
#'
#' @param milestone_data  output of prepare_milestone_data()
#' @param output_dir      directory to save PNG
#' @param facet_geography logical; if TRUE and data contains geography, use facet_grid
#' @return ggplot object
plot_milestone_potential <- function(milestone_data, output_dir,
                                     facet_geography = FALSE) {
  d <- milestone_data$by_segment
  seg_labels  <- unique(d$segment_label)
  seg_palette <- assign_palette(seg_labels, .preferred_seg_colors)
  d$segment_label <- factor(d$segment_label, levels = names(seg_palette))

  p <- ggplot(d, aes(x = product_label, y = peak_mw, fill = segment_label)) +
    geom_col(position = "stack", width = 0.65) +
    scale_fill_manual(values = seg_palette, name = "Segment") +
    labs(
      title    = "Achievable Peak Demand Response Potential",
      subtitle = "At milestone years",
      x        = NULL,
      y        = "Peak MW"
    ) +
    .theme_dr() +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

  if (facet_geography && "geography" %in% names(d)) {
    p <- p + facet_grid(geography ~ year_label)
  } else {
    p <- p + facet_wrap(~ year_label)
  }

  .save_fig(p, "01_milestone_potential.png", output_dir)
}

# ── Figure 2: Annual potential trajectory ────────────────────────────────────

#' Line chart of peak MW by product over all modeled years
#'
#' @param achievable   data frame from calculate_achievable_potential()
#' @param num_years    planning horizon
#' @param output_dir   directory to save PNG
#' @return ggplot object
plot_potential_trajectory <- function(achievable, num_years = 10, output_dir) {
  prod_hour <- aggregate(
    achievable$achievable_mw,
    by = list(year = achievable$year, product = achievable$product, hour = achievable$hour),
    FUN = sum
  )
  names(prod_hour)[4] <- "mw"

  traj <- aggregate(prod_hour$mw,
                    by = list(year = prod_hour$year, product = prod_hour$product),
                    FUN = max)
  names(traj)[3] <- "peak_mw"
  traj$product_label <- .snake_to_title(traj$product)

  prod_labels  <- unique(traj$product_label)
  prod_palette <- assign_palette(prod_labels)
  milestone_years <- c(5, 10)[c(5, 10) <= num_years]

  p <- ggplot(traj, aes(x = year, y = peak_mw, color = product_label)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = milestone_years, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    scale_x_continuous(breaks = seq_len(num_years)) +
    scale_color_manual(values = prod_palette, name = "Program") +
    labs(
      title    = "Achievable Peak MW Trajectory by Program",
      subtitle = "Dashed lines mark milestone years",
      x        = "Planning Year",
      y        = "Peak MW"
    ) +
    .theme_dr()

  .save_fig(p, "02_potential_trajectory.png", output_dir)
}

# ── Figure 3: Cost curve (supply curve) ──────────────────────────────────────

#' Supply-curve style cost curve at milestone years
#'
#' Blocks are sorted by $/kW ascending; X shows cumulative achievable MW.
#'
#' @param milestone_data  output of prepare_milestone_data()
#' @param output_dir      directory to save PNG
#' @return ggplot object
plot_cost_curve <- function(milestone_data, output_dir) {
  d <- milestone_data$by_product
  curve_data <- do.call(rbind, lapply(unique(d$year_label), function(yr) {
    sub  <- d[d$year_label == yr, ]
    sub  <- sub[order(sub$cost_per_kw), ]
    sub$mw_end   <- cumsum(sub$peak_mw)
    sub$mw_start <- c(0, head(sub$mw_end, -1))
    sub$year_label <- yr
    sub
  }))

  prod_labels  <- unique(curve_data$product_label)
  prod_palette <- assign_palette(prod_labels)
  curve_data$product_label <- factor(curve_data$product_label, levels = prod_labels)

  p <- ggplot(curve_data) +
    geom_rect(aes(xmin = mw_start, xmax = mw_end,
                  ymin = 0,        ymax = cost_per_kw,
                  fill = product_label),
              color = "white", linewidth = 0.3) +
    facet_wrap(~ year_label, scales = "free_x") +
    scale_fill_manual(values = prod_palette, name = "Program") +
    labs(
      title    = "Demand Response Cost Curve",
      subtitle = "Annual program cost ($/kW) vs. cumulative achievable peak MW",
      x        = "Cumulative Peak MW",
      y        = "Cost ($/kW)"
    ) +
    .theme_dr()

  .save_fig(p, "03_cost_curve.png", output_dir)
}

# ── Figure 4: Seasonal potential ─────────────────────────────────────────────

#' Bar chart of peak MW by product and season at milestone years
#'
#' @param achievable      data frame from calculate_achievable_potential()
#' @param season_lookup   data frame from derive_season_lookup() (hour, season)
#' @param output_dir      directory to save PNG
#' @param milestone_years integer vector of years to show
#' @return ggplot object
plot_seasonal_potential <- function(achievable, season_lookup, output_dir,
                                    milestone_years = c(5, 10)) {
  seasonal <- get_seasonal_stats(achievable, season_lookup)
  seasonal <- seasonal[seasonal$year %in% milestone_years, ]
  seasonal$product_label <- .snake_to_title(seasonal$product)
  seasonal$year_label    <- paste("Year", seasonal$year)

  prod_labels  <- unique(seasonal$product_label)
  prod_palette <- assign_palette(prod_labels)

  p <- ggplot(seasonal, aes(x = season, y = peak_mw, fill = product_label)) +
    geom_col(position = "dodge", width = 0.7) +
    facet_wrap(~ year_label) +
    scale_fill_manual(values = prod_palette, name = "Program") +
    labs(
      title    = "Peak DR Potential by Season",
      subtitle = "Peak MW during seasonal peak windows at milestone years",
      x        = "Season",
      y        = "Peak MW"
    ) +
    .theme_dr()

  .save_fig(p, "04_seasonal_potential.png", output_dir)
}

# ── Master reporting function ─────────────────────────────────────────────────

#' Generate all report figures
#'
#' @param achievable       output of calculate_achievable_potential()
#' @param costs            output of calculate_costs()
#' @param summary_stats    output of get_summary_stats()
#' @param season_lookup    output of derive_season_lookup() (stored in inputs$global$season_lookup)
#' @param output_dir       directory to write PNGs (created if needed)
#' @param milestone_years  planning years to highlight
#' @param facet_geography  if TRUE and geography column present, facet Figure 1 by geography
generate_report <- function(achievable, costs, summary_stats,
                             season_lookup    = NULL,
                             output_dir       = "outputs/figures",
                             milestone_years  = c(5, 10),
                             facet_geography  = FALSE) {
  cat("\nGenerating report figures...\n")

  num_years      <- max(achievable$year)
  milestone_data <- prepare_milestone_data(achievable, costs, summary_stats,
                                           milestone_years)

  plot_milestone_potential(milestone_data, output_dir, facet_geography)
  plot_potential_trajectory(achievable, num_years, output_dir)
  plot_cost_curve(milestone_data, output_dir)

  if (!is.null(season_lookup)) {
    plot_seasonal_potential(achievable, season_lookup, output_dir, milestone_years)
  }

  cat("Done. Figures written to:", output_dir, "\n")
}
