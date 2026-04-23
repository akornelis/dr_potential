#!/usr/bin/env Rscript
# Generate end_use_shapes.csv — single combined table, wide format by year.
# Run from the project root: Rscript generate_load_shapes.R

output_dir <- "data/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

num_years   <- 10
hours       <- 1:8760
day_of_year <- ((hours - 1) %/% 24) + 1
hour_of_day <- ((hours - 1) %% 24) + 1

make_energy_shape <- function(seasonal_vals, diurnal_vals) {
  raw <- seasonal_vals * diurnal_vals
  round(raw / sum(raw), 10)
}

# ── EU-HVAC ───────────────────────────────────────────────────────────────────
eu_hvac_seasonal <- ifelse((day_of_year <= 90) | (day_of_year >= 305), 0.90,
                    ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.85, 0.40))
eu_hvac_diurnal  <- ifelse((hour_of_day >= 6)  & (hour_of_day <= 9),  0.95,
                    ifelse((hour_of_day >= 17) & (hour_of_day <= 21), 0.90,
                    ifelse((hour_of_day >= 10) & (hour_of_day <= 16), 0.30,
                    ifelse( hour_of_day > 21,  0.90 - (hour_of_day - 21) * 0.22, 0.25))))

# ── EU-WATER HEATING ──────────────────────────────────────────────────────────
eu_wh_seasonal  <- ifelse((day_of_year <= 90) | (day_of_year >= 305), 0.85,
                   ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.70, 0.75))
eu_wh_diurnal   <- ifelse((hour_of_day >= 5) & (hour_of_day <= 7),  0.95,
                   ifelse((hour_of_day >= 17) & (hour_of_day <= 20), 0.85,
                   ifelse((hour_of_day >= 8)  & (hour_of_day <= 16), 0.50,
                   ifelse( hour_of_day > 20,   0.85 - (hour_of_day - 20) * 0.18, 0.40))))

# ── EU-LIGHTING ───────────────────────────────────────────────────────────────
eu_lighting_seasonal <- ifelse((day_of_year <= 90) | (day_of_year >= 305), 0.95,
                        ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.60, 0.75))
eu_lighting_diurnal  <- ifelse((hour_of_day >= 18) & (hour_of_day <= 22), 0.95,
                        ifelse((hour_of_day >= 6)  & (hour_of_day <= 8),  0.60,
                        ifelse((hour_of_day >= 9)  & (hour_of_day <= 17), 0.25,
                        ifelse( hour_of_day > 22,   0.95 - (hour_of_day - 22) * 0.30, 0.20))))

# ── EU-APPLIANCES ─────────────────────────────────────────────────────────────
eu_appliances_seasonal <- ifelse((day_of_year <= 90) | (day_of_year >= 305), 0.85,
                          ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.80, 0.75))
eu_appliances_diurnal  <- ifelse((hour_of_day >= 7)  & (hour_of_day <= 9),  0.90,
                          ifelse((hour_of_day >= 17) & (hour_of_day <= 21), 0.95,
                          ifelse((hour_of_day >= 10) & (hour_of_day <= 16), 0.55,
                          ifelse( hour_of_day > 21,   0.95 - (hour_of_day - 21) * 0.25, 0.35))))

# ── EU-OTHER ──────────────────────────────────────────────────────────────────
eu_other_seasonal <- ifelse((day_of_year <= 90) | (day_of_year >= 305), 0.80,
                     ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.78, 0.72))
eu_other_diurnal  <- ifelse((hour_of_day >= 8) & (hour_of_day <= 22), 0.70,
                     ifelse((hour_of_day >= 6) & (hour_of_day <  8),  0.50, 0.35))

# ── EU-IND-PROCESS ────────────────────────────────────────────────────────────
eu_ind_process_seasonal <- ifelse((day_of_year >= 152) & (day_of_year <= 244), 1.05, 1.00)
eu_ind_process_diurnal  <- ifelse((hour_of_day >= 6) & (hour_of_day <= 22), 1.10, 0.85)

# ── EU-IND-HVAC ───────────────────────────────────────────────────────────────
eu_ind_hvac_seasonal <- ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.90,
                        ifelse((day_of_year <= 90) | (day_of_year >= 305), 0.85, 0.50))
eu_ind_hvac_diurnal  <- ifelse((hour_of_day >= 8)  & (hour_of_day <= 17), 0.95,
                        ifelse((hour_of_day >= 6)  & (hour_of_day <  8),  0.60,
                        ifelse((hour_of_day > 17)  & (hour_of_day <= 20), 0.70, 0.20)))

# ── EU-IND-LIGHTING ───────────────────────────────────────────────────────────
eu_ind_lighting_seasonal <- ifelse((day_of_year <= 90) | (day_of_year >= 305), 1.05,
                             ifelse((day_of_year >= 152) & (day_of_year <= 244), 0.90, 1.00))
eu_ind_lighting_diurnal  <- ifelse((hour_of_day >= 7) & (hour_of_day <= 18), 0.95,
                             ifelse((hour_of_day >= 5) & (hour_of_day <  7),  0.40,
                             ifelse((hour_of_day > 18) & (hour_of_day <= 21), 0.50, 0.10)))

# ── Assemble shapes ───────────────────────────────────────────────────────────
eu_shapes <- list(
  hvac          = make_energy_shape(eu_hvac_seasonal,           eu_hvac_diurnal),
  water_heating = make_energy_shape(eu_wh_seasonal,             eu_wh_diurnal),
  lighting      = make_energy_shape(eu_lighting_seasonal,       eu_lighting_diurnal),
  appliances    = make_energy_shape(eu_appliances_seasonal,     eu_appliances_diurnal),
  other         = make_energy_shape(eu_other_seasonal,          eu_other_diurnal),
  ind_process   = make_energy_shape(eu_ind_process_seasonal,    eu_ind_process_diurnal),
  ind_hvac      = make_energy_shape(eu_ind_hvac_seasonal,       eu_ind_hvac_diurnal),
  ind_lighting  = make_energy_shape(eu_ind_lighting_seasonal,   eu_ind_lighting_diurnal)
)

# ── Write combined wide-format table ──────────────────────────────────────────
# Rows: end_use × hour.  Columns: end_use, hour, year_1 … year_N.
# All years share the same shape in this example; add real forecasted shapes
# by replacing the year_N columns for any end_use in the output CSV.

year_col_names <- paste0("year_", seq_len(num_years))

combined <- do.call(rbind, lapply(names(eu_shapes), function(nm) {
  ef <- eu_shapes[[nm]]
  row <- data.frame(
    end_use = nm,
    hour    = hours,
    stringsAsFactors = FALSE
  )
  # Replicate the same shape across all years
  for (col in year_col_names) row[[col]] <- ef
  row
}))

out_path <- file.path(output_dir, "end_use_shapes.csv")
write.csv(combined, out_path, row.names = FALSE)

# ── Verification ──────────────────────────────────────────────────────────────
cat("✓ End-use shapes written to:", out_path, "\n")
cat("  End uses:", length(eu_shapes), "\n")
cat("  Hours per end use:", nrow(eu_shapes[[1]]), "\n")
cat("  Year columns:", num_years, "\n\n")

for (nm in names(eu_shapes)) {
  s <- eu_shapes[[nm]]
  cat(sprintf("  %-15s sum=%.6f  min=%.8f  max=%.8f\n",
              nm, sum(s), min(s), max(s)))
}

cat("\n✓ All load shapes generated!\n")
