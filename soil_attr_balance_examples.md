# soil_attr_balance examples

This file provides practical examples for using `soil_attr_balance()`.

The examples assume that the function is available from GitHub and that the input soil database has already been cleaned and harmonized.

## 1. Load the function

```r
source("https://raw.githubusercontent.com/moquedace/funcs/main/soil_attr_balance.R")
```

If the function file is stored locally:

```r
source("./soil_attr_balance.R")
```

## 2. Basic input structure

The function expects:

1. A soil database as a data frame or tibble.
2. A vector of attributes to evaluate.
3. Optional columns for sampling units.
4. Optional depth columns.
5. Optional weights, valid ranges, and selection settings.

Example objects used below:

```r
soil_data <- k_bind

soil_attributes <- c(
  "c_gkg",
  "sand_gkg",
  "silt_gkg",
  "clay_gkg",
  "ca_co3_gkg",
  "ca_mmolkg",
  "p_mgkg",
  "p_h_h2o",
  "mg_mmolkg",
  "k_mmolkg",
  "na_mmolkg",
  "al_mmolkg",
  "h_al_mmolkg",
  "cec_ph7_mmolkg"
)
```

## 3. General exploratory diagnosis

Use this when you want to evaluate all combinations and understand the overall completeness structure of the database.

```r
res_general <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  target_attrs = "c_gkg",
  min_size = 2,
  max_size = length(soil_attributes),
  top_n = 20,
  parallel = TRUE
)
```

Inspect the main outputs:

```r
res_general$input_summary

res_general$attr_summary

res_general$row_attr_summary

res_general$best_combo_by_size

res_general$target_combo_summary

res_general$decision_notes
```

Print the best combinations without truncating long attribute names:

```r
res_general$best_combo_by_size %>%
  dplyr::select(
    n_attributes,
    attributes,
    n_rows_complete,
    pct_rows_complete,
    weighted_score,
    representativeness_score
  ) %>%
  print(n = Inf, width = Inf)
```

## 4. Carbon-focused diagnosis

Use this when every evaluated combination must include soil organic carbon.

```r
res_carbon <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  min_size = 2,
  max_size = length(soil_attributes),
  min_pct = 0.10,
  top_n = 20,
  parallel = TRUE
)
```

Inspect selection candidates:

```r
res_carbon$selection_candidates %>%
  dplyr::select(
    n_attributes,
    attributes,
    n_rows_complete,
    pct_rows_complete,
    n_coord_id_complete,
    pct_coord_id_complete,
    weighted_score,
    representativeness_score
  ) %>%
  dplyr::slice_head(n = 20) %>%
  print(n = 20, width = Inf)
```

Inspect bottleneck attributes:

```r
res_carbon$attribute_bottleneck_summary %>%
  dplyr::select(
    attribute,
    n_non_na,
    pct_non_na,
    bottleneck_effect,
    bottleneck_intensity,
    bottleneck_class
  ) %>%
  print(n = Inf, width = Inf)
```

## 5. Define attribute weights

Use weights when some attributes are more important than others for the intended application.

Example:

```r
attribute_weights_soil <- c(
  c_gkg = 6,
  sand_gkg = 5,
  silt_gkg = 5,
  clay_gkg = 5,
  p_h_h2o = 4,
  cec_ph7_mmolkg = 4,
  ca_mmolkg = 3,
  mg_mmolkg = 3,
  k_mmolkg = 3,
  al_mmolkg = 3,
  h_al_mmolkg = 3,
  p_mgkg = 2,
  na_mmolkg = 1,
  ca_co3_gkg = 1
)
```

Run weighted analysis:

```r
res_weighted <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_soil,
  min_pct = 0.10,
  ranking_metric = "weighted_score",
  selection_priority = "metric_first",
  top_n = 20,
  parallel = TRUE
)
```

Inspect weighted candidates:

```r
res_weighted$selection_candidates %>%
  dplyr::select(
    n_attributes,
    attributes,
    n_rows_complete,
    pct_rows_complete,
    attribute_weight_sum,
    weighted_score,
    representativeness_score
  ) %>%
  dplyr::slice_head(n = 20) %>%
  print(n = 20, width = Inf)
```

## 6. Define valid ranges

Use valid ranges to diagnose invalid values or to treat them as unavailable during the combination analysis.

Example:

```r
valid_ranges_soil <- list(
  c_gkg = c(0, 600),
  sand_gkg = c(0, 1000),
  silt_gkg = c(0, 1000),
  clay_gkg = c(0, 1000),
  ca_co3_gkg = c(0, 1000),
  ca_mmolkg = c(0, 2000),
  p_mgkg = c(0, 10000),
  p_h_h2o = c(2, 12),
  mg_mmolkg = c(0, 2000),
  k_mmolkg = c(0, 2000),
  na_mmolkg = c(0, 2000),
  al_mmolkg = c(0, 2000),
  h_al_mmolkg = c(0, 2000),
  cec_ph7_mmolkg = c(0, 3000)
)
```

## 7. Valid ranges as diagnostics only

Here, valid ranges are reported in `attr_summary`, but combinations are still evaluated using only non-missing values.

```r
res_ranges_diagnostic <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  valid_ranges = valid_ranges_soil,
  use_valid_ranges_for_combinations = FALSE,
  min_pct = 0.10,
  top_n = 20,
  parallel = TRUE
)
```

Inspect invalid values:

```r
res_ranges_diagnostic$attr_summary %>%
  dplyr::select(
    attribute,
    n_non_na,
    pct_non_na,
    n_valid,
    pct_valid,
    n_invalid,
    pct_invalid
  ) %>%
  dplyr::arrange(
    dplyr::desc(n_invalid),
    attribute
  ) %>%
  print(n = Inf, width = Inf)
```

## 8. Strict valid-range analysis

Here, a value is considered available only if it is not missing and lies within the valid range.

```r
res_ranges_strict <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_soil,
  valid_ranges = valid_ranges_soil,
  use_valid_ranges_for_combinations = TRUE,
  min_pct = 0.10,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  top_n = 20,
  parallel = TRUE
)
```

Inspect strict candidates:

```r
res_ranges_strict$selection_candidates %>%
  dplyr::select(
    n_attributes,
    attributes,
    n_rows_complete,
    pct_rows_complete,
    n_coord_id_complete,
    pct_coord_id_complete,
    weighted_score,
    representativeness_score
  ) %>%
  dplyr::slice_head(n = 20) %>%
  print(n = 20, width = Inf)
```

## 9. Remove rare or limiting attributes

After inspecting `attribute_bottleneck_summary`, you may decide to remove rare attributes from the general analysis.

Example:

```r
soil_attributes_core <- setdiff(
  soil_attributes,
  c("ca_co3_gkg", "na_mmolkg")
)
```

Keep only weights and ranges for the selected attributes:

```r
attribute_weights_core <- attribute_weights_soil[
  names(attribute_weights_soil) %in% soil_attributes_core
]

valid_ranges_core <- valid_ranges_soil[
  names(valid_ranges_soil) %in% soil_attributes_core
]
```

Run the core analysis:

```r
res_core <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_core,
  valid_ranges = valid_ranges_core,
  use_valid_ranges_for_combinations = TRUE,
  min_pct = 0.10,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  return_selected = TRUE,
  parallel = TRUE
)
```

Inspect selected dataset:

```r
res_core$selected_summary

cat(res_core$selected_summary$selected_attributes)
```

## 10. Richness-first selection

Use `selection_priority = "richness_first"` when the goal is to retain the largest possible number of attributes after applying a minimum completeness threshold.

```r
res_richness <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_core,
  valid_ranges = valid_ranges_core,
  use_valid_ranges_for_combinations = TRUE,
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  return_selected = TRUE,
  parallel = TRUE
)
```

Inspect:

```r
res_richness$selected_summary

cat(res_richness$selected_summary$selected_attributes)
```

## 11. Metric-first selection

Use `selection_priority = "metric_first"` when `ranking_metric` should guide the selection before number of attributes.

This is useful when `attribute_weights` are meaningful and `ranking_metric = "weighted_score"`.

```r
res_metric <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_core,
  valid_ranges = valid_ranges_core,
  use_valid_ranges_for_combinations = TRUE,
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "metric_first",
  return_selected = TRUE,
  parallel = TRUE
)
```

Inspect:

```r
res_metric$selected_summary

cat(res_metric$selected_summary$selected_attributes)
```

## 12. Rows-first selection

Use `selection_priority = "rows_first"` when the goal is to maximize sample size.

Important: use `min_size` with `rows_first`, otherwise the function may select very small combinations with many rows.

```r
res_rows_10 <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_core,
  valid_ranges = valid_ranges_core,
  use_valid_ranges_for_combinations = TRUE,
  min_size = 10,
  max_size = length(soil_attributes_core),
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "rows_first",
  return_selected = TRUE,
  parallel = TRUE
)
```

Inspect:

```r
res_rows_10$selected_summary

cat(res_rows_10$selected_summary$selected_attributes)
```

## 13. Compare selection priorities

Compare the selected datasets from different strategies.

```r
selection_comparison <- dplyr::bind_rows(
  richness_first = res_richness$selected_summary,
  metric_first = res_metric$selected_summary,
  rows_first_min_10 = res_rows_10$selected_summary,
  .id = "selection_priority"
) %>%
  dplyr::select(
    selection_priority,
    selected_attributes,
    n_attributes,
    n_rows_selected,
    pct_rows_selected,
    n_coord_id_selected,
    pct_coord_id_selected,
    n_depth_classes_selected
  )

selection_comparison %>%
  print(width = Inf)
```

## 14. Select a specific custom combination

Use `selected_attrs` when you already know which attribute combination you want.

```r
selected_attrs_custom <- c(
  "c_gkg",
  "sand_gkg",
  "silt_gkg",
  "clay_gkg",
  "p_h_h2o",
  "cec_ph7_mmolkg"
)

res_custom <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  selected_attrs = selected_attrs_custom,
  return_selected = TRUE,
  parallel = TRUE
)
```

Inspect:

```r
res_custom$selected_summary

selected_data_custom <- res_custom$selected_data
```

## 15. Export selected dataset

`soil_attr_balance()` does not write files automatically.

Export should be done outside the function.

Example:

```r
readr::write_csv2(
  res_custom$selected_data,
  "soil_selected_dataset.csv"
)

readr::write_csv2(
  res_custom$selected_summary,
  "soil_selected_summary.csv"
)
```

## 16. Save diagnostic outputs

Example:

```r
readr::write_csv2(
  res_core$attr_summary,
  "soil_attr_summary.csv"
)

readr::write_csv2(
  res_core$row_attr_summary,
  "soil_row_attr_summary.csv"
)

readr::write_csv2(
  res_core$selection_candidates,
  "soil_selection_candidates.csv"
)

readr::write_csv2(
  res_core$attribute_bottleneck_summary,
  "soil_attribute_bottleneck_summary.csv"
)

readr::write_csv2(
  res_core$missingness_correlation_long,
  "soil_missingness_correlation_long.csv"
)

readr::write_csv2(
  res_core$decision_notes,
  "soil_decision_notes.csv"
)
```

## 17. Recommended practical workflow

```r
source("https://raw.githubusercontent.com/moquedace/funcs/main/soil_attr_balance.R")

soil_attributes <- c(
  "c_gkg",
  "sand_gkg",
  "silt_gkg",
  "clay_gkg",
  "ca_co3_gkg",
  "ca_mmolkg",
  "p_mgkg",
  "p_h_h2o",
  "mg_mmolkg",
  "k_mmolkg",
  "na_mmolkg",
  "al_mmolkg",
  "h_al_mmolkg",
  "cec_ph7_mmolkg"
)

res_initial <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  target_attrs = "c_gkg",
  parallel = TRUE
)

res_initial$attr_summary
res_initial$attribute_bottleneck_summary

soil_attributes_core <- setdiff(
  soil_attributes,
  c("ca_co3_gkg", "na_mmolkg")
)

res_final <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_soil[names(attribute_weights_soil) %in% soil_attributes_core],
  valid_ranges = valid_ranges_soil[names(valid_ranges_soil) %in% soil_attributes_core],
  use_valid_ranges_for_combinations = TRUE,
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  return_selected = TRUE,
  parallel = TRUE
)

res_final$selected_summary
res_final$decision_notes

soil_selected <- res_final$selected_data
```

## 18. Notes

Use `richness_first` when the goal is to maximize analytical richness.

Use `metric_first` when the chosen `ranking_metric` should dominate selection.

Use `rows_first` when sample size is the priority, but pair it with `min_size`.

Use `required_attrs` when a variable must appear in all combinations.

Use `target_attrs` when you want a specific summary for an important attribute, but do not want to force all combinations to contain it.

Use `valid_ranges` first as a diagnostic, then decide whether strict range filtering is appropriate.

Use `return_selected = TRUE` only when you are ready to generate a filtered dataset.
