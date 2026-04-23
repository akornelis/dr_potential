# DR Potential Assessment Model

An R model for estimating the technical and achievable demand response (DR) potential of utility programs, with program cost tracking, seasonal analysis, and optional geographic segmentation.

## Overview

The model calculates two potential types:

- **Technical potential** — maximum possible DR if all eligible customers enrolled
- **Achievable potential** — realistic DR accounting for adoption ramp-up, market saturation, and event opt-outs

Two impact tracks are supported:

- **Bottom-up** (`kw_per_customer`) — flat kW reduction per enrolled customer, independent of load shape
- **Top-down** (`pct_of_load`) — percentage reduction of the customer's hourly baseline load profile

Results are produced by year, product, segment, and hour across a configurable planning horizon.

---

## Quickstart

```r
# 1. Generate example input data
Rscript generate_inputs.R       # creates data/*.csv
Rscript generate_load_shapes.R  # creates data/end_use_shapes.csv

# 2. Run the full pipeline and produce report figures
Rscript report.R

# 3. (Optional) Run the annotated example workflow
Rscript example_analysis.R
```

All outputs are written to `outputs/`. Figures go to `outputs/figures/`.

---

## Repository structure

```
R/
  load_data.R              # input loaders, validation, baseline load derivation
  technical_potential.R    # 100%-enrollment potential
  achievable_potential.R   # adoption-adjusted potential + enrollment helper
  costs.R                  # incentive and O&M costs
  aggregation.R            # summary statistics and seasonal aggregations
  reporting.R              # ggplot2 report figures

report.R                   # main pipeline entry point
example_analysis.R         # annotated walkthrough with printed output
generate_inputs.R          # generates example input CSVs in data/
generate_load_shapes.R     # generates data/end_use_shapes.csv

data/templates/            # input templates and column reference (committed)
data/                      # working input data — not committed, user-supplied
outputs/                   # model outputs — not committed
```

---

## Input files

Place input CSVs in `data/` before running the model. See [`data/templates/README.md`](data/templates/README.md) for a full column reference for each file.

| file | description |
|---|---|
| `global_inputs.csv` | Line loss rate, discount rate |
| `peak_periods.csv` | Season definitions (months + daily hour windows) |
| `customer_counts.csv` | Customers by segment and year |
| `product_impact.csv` | Per-customer kW impact or % of load by product and segment |
| `annual_sales.csv` | Annual kWh by sector, segment, and end-use (drives top-down baseline) |
| `end_use_shapes.csv` | Hourly energy fraction by end-use and year (8760 rows × year columns) |
| `participation_adoption.csv` | Adoption ramp parameters by product and segment |
| `costs.csv` | Program costs by product |
| `eligibility_matrix.csv` | Eligible customer share by product, segment, and year |

Year-varying files use a wide format: one `year_N` column per planning year. Add or remove year columns to change the planning horizon; all year-varying files must cover the same number of years.

### Optional: geographic segmentation

Add a `geography` column to `customer_counts.csv` and `eligibility_matrix.csv` to model multiple service territories. When present, geography flows through enrollment, potential, and summary statistics automatically. See the templates for the column ordering.

---

## Pipeline

```
load_all_inputs()
    │
    ├─ calculate_technical_potential()   → technical_mw by year/product/segment/hour
    │
    ├─ calculate_enrollment()            → enrolled_customers by year/product/segment
    │      │
    │      ├─ calculate_achievable_potential()  → achievable_mw by year/product/segment/hour
    │      │
    │      └─ calculate_costs()                 → cost components by year/product
    │
    └─ get_summary_stats() / get_seasonal_stats()
           │
           └─ generate_report()          → PNG figures in outputs/figures/
```

Enrollment is computed once and passed to both `calculate_achievable_potential()` and `calculate_costs()`.

---

## Outputs

### CSVs (`outputs/`)

| file | description |
|---|---|
| `01_technical_potential_hourly.csv` | Technical potential: MW by year/product/segment/hour |
| `02_achievable_potential_hourly.csv` | Achievable potential: MW by year/product/segment/hour |
| `03_program_costs_annual.csv` | Cost components and discounted total by year/product |
| `04_summary_statistics.csv` | Annual peak MW, avg MW, total GWh by product/segment |
| `05_seasonal_stats.csv` | Peak MW and GWh by season, product, and segment |

### Figures (`outputs/figures/`)

| file | description |
|---|---|
| `01_milestone_potential.png` | Stacked bar: peak MW by segment at milestone years |
| `02_potential_trajectory.png` | Line chart: peak MW trajectory by program over the planning horizon |
| `03_cost_curve.png` | Supply curve: $/kW vs. cumulative peak MW at milestone years |
| `04_seasonal_potential.png` | Bar chart: peak MW by season and program at milestone years |

---

## Extending the model

**New end-use** — Add a `eu_*` block to `generate_load_shapes.R`, add a row to `annual_sales.csv`, and run `Rscript generate_load_shapes.R`. No other code changes needed.

**New product or segment** — Add rows to `product_impact.csv`, `participation_adoption.csv`, `costs.csv`, and `eligibility_matrix.csv`. Labels appear automatically in all figures.

**Longer planning horizon** — Add `year_N` columns to all wide-format input files and update `num_years` in `report.R`.
