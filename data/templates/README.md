# Input Templates

Copy these files to `data/` and populate them with your utility's data before running the model.
Run `Rscript generate_inputs.R` to regenerate the example data at any time.

---

## Year columns

Files with forecasts use a **wide year format**: one column per planning year named `year_1`, `year_2`, …, `year_N`.
The model reads however many year columns are present. All year-varying files must cover the same number of years.

---

## File reference

### `global_inputs.csv`
Scalar parameters applied across the entire model.

| column | description |
|---|---|
| `parameter` | Parameter name (must include `line_loss_rate` and `discount_rate`) |
| `value` | Numeric value |

- `line_loss_rate` — fraction of customer-side energy lost in transmission/distribution (0–1, exclusive of 1)
- `discount_rate` — annual discount rate for present-value cost calculations (≥ 0)

---

### `peak_periods.csv`
Defines the daily hour windows and calendar months that constitute each season's peak period.
Multiple rows per season are allowed (e.g., morning and evening windows).

| column | description |
|---|---|
| `season` | Season label (e.g., `summer`, `winter`) |
| `month_start` | First month of the season (1–12) |
| `month_end` | Last month of the season (1–12). If `month_start > month_end`, the season wraps across the new year (e.g., Dec–Feb). |
| `peak_start_hour` | First peak hour of the day, 24-hour clock (1–24, inclusive) |
| `peak_end_hour` | Last peak hour of the day, 24-hour clock (1–24, inclusive) |

---

### `customer_counts.csv`
Number of customers by segment and year. Optionally include a `geography` column (before `segment`) to model multiple service territories.

| column | description |
|---|---|
| `geography` *(optional)* | Territory name (e.g., `east`, `west`) |
| `segment` | Customer segment name |
| `year_1` … `year_N` | Customer count for each planning year |

Segment names must match those used in `product_impact.csv`, `annual_sales.csv`, and `eligibility_matrix.csv`.

---

### `product_impact.csv`
Per-customer impact assumption for each product × segment combination.

| column | description |
|---|---|
| `product` | Program name |
| `segment` | Customer segment |
| `impact_value` | Magnitude of impact (see `impact_unit`) |
| `impact_unit` | `kw_per_customer` — flat kW reduction per enrolled customer; `pct_of_load` — fraction of hourly baseline load reduced |
| `sector` | Required when `impact_unit = pct_of_load`; must match a sector in `annual_sales.csv`. Leave blank for `kw_per_customer` rows. |

Every product × segment combination here must also appear in `participation_adoption.csv` and `eligibility_matrix.csv`.

---

### `annual_sales.csv`
Annual electricity sales (kWh) by sector, segment, and end-use, used to derive the hourly baseline load profile. Values represent total kWh across all customers in that sector/segment/end-use cell.

| column | description |
|---|---|
| `sector` | Broad sector (e.g., `residential`, `industrial`) — must match `sector` values in `product_impact.csv` |
| `segment` | Customer segment |
| `end_use` | End-use name (e.g., `hvac`, `water_heating`) — must match an end-use in `end_use_shapes.csv` |
| `year_1` … `year_N` | Total annual kWh for that sector/segment/end-use in each planning year |

---

### `end_use_shapes.csv`
Hourly energy distribution fractions for each end-use. Each end-use × year combination must have exactly **8760 rows** (one per hour) and the `energy_fraction` values must **sum to 1.0**.

Run `Rscript generate_load_shapes.R` to regenerate this file from parametric seasonal/diurnal assumptions.

| column | description |
|---|---|
| `end_use` | End-use name — must match values in `annual_sales.csv` |
| `hour` | Hour of year (1–8760) |
| `year_1` … `year_N` | Fraction of annual energy consumed in this hour (all 8760 values per year must sum to 1.0) |

---

### `participation_adoption.csv`
Adoption trajectory parameters for each product × segment combination.

| column | description |
|---|---|
| `product` | Program name |
| `segment` | Customer segment |
| `max_adoption_rate` | Maximum fraction of eligible customers who ever enroll (0–1) |
| `annual_ramp_rate` | Annual increment as a fraction of `max_adoption_rate` (0–1); enrollment grows by `annual_ramp_rate × max_adoption_rate` each year |
| `event_optout_rate` | Fraction of enrolled customers who opt out of any given event (0–1) |
| `first_year_adoption` | Adoption rate in year 1 (0–1) |

---

### `costs.csv`
Program cost assumptions by product. Costs are product-level (not segment-level).

| column | description |
|---|---|
| `product` | Program name |
| `program_setup_cost` | One-time setup cost incurred in year 1 ($) |
| `hardware_cost_per_new_participant` | Hardware/device cost per newly enrolled customer ($/customer) |
| `marketing_cost_per_new_participant` | Marketing cost per newly enrolled customer ($/customer) |
| `onetime_incentive_per_new_participant` | Enrollment incentive per newly enrolled customer ($/customer) |
| `annual_incentive_per_participant` | Annual incentive paid per enrolled customer ($/customer/year) — used when `incentive_unit = per_participant` |
| `om_admin_cost_annual_fixed` | Fixed annual O&M and administration cost ($/year) |
| `incentive_unit` | `per_participant` — annual incentive scales with enrolled customers; `per_kw_pledged` — annual incentive scales with kW pledged |
| `annual_incentive_per_kw_pledged` | Annual incentive rate per kW pledged ($/kW/year) — used when `incentive_unit = per_kw_pledged` |

Set unused incentive columns to `0` (e.g., set `annual_incentive_per_kw_pledged = 0` for `per_participant` products).

---

### `eligibility_matrix.csv`
Share of customers in each segment who are eligible to enroll in each product, by year. Optionally include a `geography` column to vary eligibility by territory.

| column | description |
|---|---|
| `geography` *(optional)* | Territory name — include only if `customer_counts.csv` has a `geography` column |
| `product` | Program name |
| `segment` | Customer segment |
| `year_1` … `year_N` | Eligible share of customers (0–1) for each planning year |
