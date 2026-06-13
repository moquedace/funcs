# soil_attr_balance

`soil_attr_balance()` is a generic R function designed to evaluate completeness and attribute-combination trade-offs in soil databases.

It helps identify which combinations of soil attributes preserve an acceptable number of complete observations while retaining as much analytical information as possible.

The function is especially useful for soil databases compiled from multiple sources, where different attributes may have very different levels of analytical coverage.

## 1. Purpose

Soil databases often contain many attributes, but not all attributes are available for the same samples, layers, profiles, or coordinates.

For example, a database may contain many observations with bulk density, texture, or pH, but fewer observations with carbon, phosphorus, exchangeable bases, or more specific chemical attributes.

This creates a practical problem when building datasets for:

1. Digital soil mapping.
2. Pedotransfer functions.
3. Soil carbon modeling.
4. Spectroscopic modeling.
5. Harmonized soil databases.
6. Exploratory multivariate analysis.
7. Model training datasets.

The central question answered by `soil_attr_balance()` is:

```text
Which combinations of soil attributes preserve enough complete observations while retaining a useful set of analytical variables?
```

## 2. What the function does

`soil_attr_balance()` evaluates combinations of user-defined attributes and returns several diagnostic tables.

It can compute:

1. Individual completeness for each attribute.
2. Row-level attribute availability.
3. Complete-row counts for all attribute combinations.
4. Best combinations by number of attributes.
5. Top combinations by number of attributes.
6. Selection candidates after applying minimum row or percentage thresholds.
7. Pareto-style trade-offs between number of attributes and complete rows.
8. Marginal loss when increasing the number of attributes.
9. Bottleneck attributes.
10. Missingness correlation among attributes.
11. Depth completeness diagnostics.
12. Sampling-unit completeness diagnostics.
13. Target-attribute summaries.
14. Optional selected dataset based on a chosen or automatically selected combination.
15. Optional diagnostic graphs as `ggplot` objects.
16. Short decision notes summarizing the analysis.

## 3. What the function does not do

The function assumes that the input database has already been cleaned and harmonized.

It does not:

1. Read files.
2. Write files.
3. Convert units.
4. Convert organic matter to organic carbon.
5. Derive new soil variables.
6. Remove duplicate records.
7. Harmonize analytical methods.
8. Perform spatial filtering.
9. Apply project-specific pedological rules.
10. Decide which attributes are scientifically valid for a specific modeling objective.

All project-specific preprocessing must be done before calling the function.

For example, if organic matter should be converted to carbon, this conversion must be performed before passing the data to `soil_attr_balance()`.

## 4. Basic usage

```r
source("https://raw.githubusercontent.com/moquedace/funcs/main/utils/soil_attr_balance.R")

res <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  return_selected = TRUE
)
```

## 5. Function interface

```r
soil_attr_balance(
  data,
  attrs,
  unit_cols = NULL,
  depth_cols = NULL,
  depth_class_col = NULL,
  depth_breaks = c(0, 5, 15, 30, 60, 100, 200),
  depth_method = "midpoint",
  attribute_weights = NULL,
  valid_ranges = NULL,
  use_valid_ranges_for_combinations = FALSE,
  min_size = 2,
  max_size = length(attrs),
  required_attrs = NULL,
  target_attrs = NULL,
  target_mode = "all",
  min_rows = 1,
  min_pct = 0,
  top_n = 20,
  ranking_metric = "n_rows_complete",
  selection_priority = "richness_first",
  compute_marginal_loss = TRUE,
  compute_bottleneck = TRUE,
  compute_missingness_correlation = TRUE,
  compute_representativeness = TRUE,
  return_selected = FALSE,
  selected_attrs = NULL,
  graphs = FALSE,
  graph_top_n = 30,
  parallel = TRUE,
  n_workers = NULL,
  chunk_size = 500,
  max_combinations = 500000,
  clean_names = FALSE,
  load_packages = TRUE
)
```

## 6. Arguments

### 6.1 data

A data frame or tibble containing the soil database.

The database should already be harmonized before use. This means that column names, units, analytical methods, spatial filters, and derived variables should already be prepared according to the objective of the analysis.

Example:

```r
data = k_bind
```

### 6.2 attrs

Character vector with the soil attributes that should be evaluated in the combination analysis.

Only these attributes are used to build combinations. Other columns in `data` are kept in `selected_data` if `return_selected = TRUE`, but they are not used to define completeness.

Example:

```r
attrs <- c(
  "c_gkg",
  "sand_gkg",
  "silt_gkg",
  "clay_gkg",
  "p_h_h2o",
  "ca_mmolkg",
  "mg_mmolkg",
  "k_mmolkg",
  "al_mmolkg",
  "h_al_mmolkg",
  "cec_ph7_mmolkg"
)
```

### 6.3 unit_cols

Optional character vector with one or more columns representing sampling units, profiles, sites, coordinates, samples, or other grouping identifiers.

If supplied, the function calculates how many unique units remain complete for each attribute combination.

If `NULL`, the function only computes row-level completeness.

Examples:

```r
unit_cols = "coord_id"
```

```r
unit_cols = c("profile_id", "sample_id")
```

```r
unit_cols = NULL
```

### 6.4 depth_cols

Optional character vector with two columns representing upper and lower depth.

Expected format:

```r
depth_cols = c("upper_depth_cm", "lower_depth_cm")
```

These columns should contain numeric depth values, usually in centimeters.

If `depth_cols` is supplied and `depth_class_col` is `NULL`, the function creates depth classes internally using `depth_breaks` and `depth_method`.

If `NULL`, depth diagnostics are skipped unless `depth_class_col` is supplied.

### 6.5 depth_class_col

Optional character string with the name of a pre-existing depth class column.

Use this when the database already has standardized depth intervals.

Example:

```r
depth_class_col = "depth_interval"
```

Possible values in this column may include:

```text
0_5
5_15
15_30
30_60
60_100
100_200
```

If both `depth_class_col` and `depth_cols` are supplied, `depth_class_col` has priority.

### 6.6 depth_breaks

Numeric vector defining the depth intervals used when `depth_cols` is supplied.

Default:

```r
depth_breaks = c(0, 5, 15, 30, 60, 100, 200)
```

This creates the following classes:

```text
0_5
5_15
15_30
30_60
60_100
100_200
```

If `depth_method = "midpoint"`, layers are classified according to their midpoint.

If a layer midpoint is deeper than the maximum depth break, it is classified as:

```text
deeper_than_200
```

when using the default breaks.

### 6.7 depth_method

Method used to assign depth classes when `depth_cols` is supplied.

Possible values:

```r
depth_method = "midpoint"
depth_method = "exact"
```

`"midpoint"` uses the layer midpoint:

```r
depth_mid_cm = (upper_depth_cm + lower_depth_cm) / 2
```

This is recommended for heterogeneous databases where layers do not exactly match standard intervals.

`"exact"` only classifies layers that exactly match the intervals defined in `depth_breaks`. Layers that do not exactly match are classified as `"other"`.

Default:

```r
depth_method = "midpoint"
```

### 6.8 attribute_weights

Optional named numeric vector with weights assigned to attributes.

The weights are used to compute `weighted_score`:

```r
weighted_score = sum(attribute weights in the combination) * log1p(n_rows_complete)
```

If `attribute_weights = NULL`, all attributes receive weight 1.

This means the function runs normally even without weights.

Example:

```r
attribute_weights <- c(
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
  p_mgkg = 2
)
```

Any attribute not explicitly listed in `attribute_weights` receives weight 1.

### 6.9 valid_ranges

Optional named list defining valid numerical ranges for attributes.

Each list element must contain a numeric vector of length 2.

Example:

```r
valid_ranges <- list(
  c_gkg = c(0, 600),
  sand_gkg = c(0, 1000),
  silt_gkg = c(0, 1000),
  clay_gkg = c(0, 1000),
  p_h_h2o = c(2, 12),
  ca_mmolkg = c(0, 2000),
  mg_mmolkg = c(0, 2000),
  k_mmolkg = c(0, 2000),
  al_mmolkg = c(0, 2000),
  h_al_mmolkg = c(0, 2000),
  cec_ph7_mmolkg = c(0, 3000),
  p_mgkg = c(0, 10000)
)
```

If `valid_ranges` is supplied, `attr_summary` reports:

```text
n_valid
pct_valid
n_invalid
pct_invalid
```

The valid ranges are only used to filter complete combinations when `use_valid_ranges_for_combinations = TRUE`.

### 6.10 use_valid_ranges_for_combinations

Logical value.

If `FALSE`, combinations are evaluated only with non-missing values.

If `TRUE`, combinations are evaluated using both non-missing values and valid ranges. In this case, a value is treated as available only if it is not `NA` and lies within the valid range supplied for that attribute.

Default:

```r
use_valid_ranges_for_combinations = FALSE
```

Recommended use:

```text
FALSE for initial diagnosis
TRUE for stricter dataset selection
```

### 6.11 min_size

Minimum number of attributes allowed in each combination.

Default:

```r
min_size = 2
```

Example:

```r
min_size = 10
```

This is useful when using `selection_priority = "rows_first"`, because otherwise the function may select very small combinations with many rows.

### 6.12 max_size

Maximum number of attributes allowed in each combination.

Default:

```r
max_size = length(attrs)
```

Reducing `max_size` is useful when `attrs` contains many attributes and the number of possible combinations becomes too large.

### 6.13 required_attrs

Optional character vector with attributes that must appear in every evaluated combination.

Example:

```r
required_attrs = "c_gkg"
```

This is useful when the analysis requires a response variable or mandatory attribute, such as soil organic carbon.

When `required_attrs` is supplied, the number of evaluated combinations is reduced, and bottleneck diagnostics for those required attributes are marked as:

```text
required_attribute_not_evaluated
```

### 6.14 target_attrs

Optional character vector with attributes of special interest.

Unlike `required_attrs`, `target_attrs` does not force the whole analysis to contain these attributes. It only creates target-specific summaries.

Example:

```r
target_attrs = "c_gkg"
```

Use `target_attrs` when you want to evaluate all combinations, but also want a specific summary of combinations containing carbon, pH, texture, or another attribute of interest.

### 6.15 target_mode

Rule used when more than one target attribute is supplied.

Possible values:

```r
target_mode = "all"
target_mode = "any"
```

`"all"` means a target combination must contain all target attributes.

`"any"` means a target combination may contain at least one target attribute.

Example:

```r
target_attrs = c("c_gkg", "clay_gkg")
target_mode = "all"
```

Example:

```r
target_attrs = c("c_gkg", "p_h_h2o")
target_mode = "any"
```

### 6.16 min_rows

Minimum number of complete rows required for a combination to enter `selection_candidates`.

Default:

```r
min_rows = 1
```

Example:

```r
min_rows = 10000
```

### 6.17 min_pct

Minimum proportion of complete rows required for a combination to enter `selection_candidates`.

Default:

```r
min_pct = 0
```

Example:

```r
min_pct = 0.10
```

This means that only combinations preserving at least 10 percent of the database are included in `selection_candidates`.

Example:

```r
min_pct = 0.50
```

This means that only combinations preserving at least 50 percent of the database are considered as selection candidates.

### 6.18 top_n

Number of top combinations to retain per combination size in `top_combo_by_size`.

Default:

```r
top_n = 20
```

### 6.19 ranking_metric

Numeric metric used to rank combinations within `best_combo_by_size`, `top_combo_by_size`, and `selection_candidates`.

Common values:

```r
ranking_metric = "n_rows_complete"
ranking_metric = "weighted_score"
ranking_metric = "representativeness_score"
```

`"n_rows_complete"` prioritizes combinations with more complete rows.

`"weighted_score"` prioritizes combinations with higher attribute weight and good row support.

`"representativeness_score"` prioritizes combinations with good row support, unit coverage, and depth coverage when these diagnostics are available.

Default:

```r
ranking_metric = "n_rows_complete"
```

### 6.20 selection_priority

Defines how `selection_candidates` are ordered after applying `min_rows` and `min_pct`.

Possible values:

```r
selection_priority = "richness_first"
selection_priority = "metric_first"
selection_priority = "rows_first"
```

`"richness_first"` prioritizes combinations with more attributes first, then the selected `ranking_metric`, then `n_rows_complete`.

Use this when the goal is to find the richest possible multivariate dataset while respecting a minimum completeness threshold.

`"metric_first"` prioritizes the selected `ranking_metric` first, then the number of attributes, then `n_rows_complete`.

Use this when attribute weights or representativeness should guide the selection more strongly than the raw number of attributes.

`"rows_first"` prioritizes `n_rows_complete` first, then the number of attributes, then the selected `ranking_metric`.

Use this when the goal is to maximize sample size. It is recommended to use `rows_first` together with `min_size`, otherwise very small combinations may be selected.

### 6.21 compute_marginal_loss

Logical value.

If `TRUE`, the function computes `marginal_loss_summary`.

This table shows how many rows are lost when moving from the best combination of one size to the best combination of the next size.

This helps identify the point where adding one more attribute causes a large loss of complete observations.

Default:

```r
compute_marginal_loss = TRUE
```

### 6.22 compute_bottleneck

Logical value.

If `TRUE`, the function computes `attribute_bottleneck_summary`.

This diagnostic identifies attributes that strongly reduce the number of complete combinations.

Attributes with low completeness, such as rare analyses, tend to appear as strong bottlenecks.

Default:

```r
compute_bottleneck = TRUE
```

### 6.23 compute_missingness_correlation

Logical value.

If `TRUE`, the function computes `missingness_correlation` and `missingness_correlation_long`.

This does not correlate soil attribute values. It correlates missingness patterns.

High positive missingness correlation means two attributes tend to be missing together.

High negative missingness correlation means one attribute tends to be present when the other is missing.

This helps identify analytical blocks, laboratory routines, or data sources with similar missingness structures.

Default:

```r
compute_missingness_correlation = TRUE
```

### 6.24 compute_representativeness

Logical value.

If `TRUE`, the function computes `representativeness_score`.

The score is based on:

```text
log1p(n_rows_complete)
log1p(mean_n_units_complete), when unit_cols is supplied
log1p(n_depth_classes_complete), when depth information is supplied
```

The score is intended as a practical ranking aid, not as a formal statistical measure of sampling representativeness.

Default:

```r
compute_representativeness = TRUE
```

### 6.25 return_selected

Logical value.

If `FALSE`, the function returns diagnostic tables only.

If `TRUE`, the function also returns:

```text
selected_summary
selected_data
```

If `selected_attrs = NULL`, `selected_data` is based on the first row of `selection_candidates`.

If `selected_attrs` is supplied, `selected_data` is filtered using the user-defined selected attribute combination.

Default:

```r
return_selected = FALSE
```

### 6.26 selected_attrs

Optional character vector defining a specific combination to filter.

Example:

```r
selected_attrs <- c(
  "c_gkg",
  "sand_gkg",
  "silt_gkg",
  "clay_gkg",
  "p_h_h2o"
)
```

If `return_selected = TRUE` and `selected_attrs = NULL`, the function automatically uses the first combination in `selection_candidates`.

### 6.27 graphs

Logical value.

If `FALSE`, no graphs are created.

If `TRUE`, the function returns a named list of `ggplot` objects in the output element `graphs`.

Graphs are created only at the end of the function using diagnostic tables already computed by the analysis. Therefore, graphical output does not affect combination generation, ranking, selected datasets, bottleneck diagnostics, or any tabular result.

The package `ggplot2` is required only when `graphs = TRUE`.

Default:

```r
graphs = FALSE
```

### 6.28 graph_top_n

Integer value defining how many top records should be used in graphs where showing all results may be excessive, such as selection candidates or bottleneck attributes.

Default:

```r
graph_top_n = 30
```

### 6.29 parallel

Logical value.

If `TRUE`, the combination evaluation is parallelized using `future` and `furrr`.

If `FALSE`, combinations are evaluated sequentially.

Default:

```r
parallel = TRUE
```

### 6.30 n_workers

Number of parallel workers.

If `NULL`, the function uses:

```r
max(1, parallelly::availableCores() - 1)
```

Example:

```r
n_workers = 6
```

### 6.31 chunk_size

Number of combinations evaluated per chunk.

Default:

```r
chunk_size = 500
```

Lower values may reduce memory pressure.

Higher values may reduce overhead in large analyses.

### 6.32 max_combinations

Safety limit for the number of generated attribute combinations.

Default:

```r
max_combinations = 500000
```

If the number of possible combinations exceeds this value, the function stops and asks the user to reduce `max_size`, use `required_attrs`, or increase `max_combinations`.

### 6.33 clean_names

Logical value.

If `TRUE`, the function applies `janitor::clean_names()` to `data` and also cleans argument vectors such as:

```text
attrs
unit_cols
depth_cols
required_attrs
target_attrs
selected_attrs
attribute_weights names
valid_ranges names
```

Default:

```r
clean_names = FALSE
```

Use `TRUE` when the input database has inconsistent column names.

### 6.34 load_packages

Logical value.

If `TRUE`, the function tries to load required packages using `install_load_pkg()` from the user's GitHub helper script.

If `install_load_pkg()` is not available, it checks and loads the packages directly.

Default:

```r
load_packages = TRUE
```

Set `load_packages = FALSE` if packages are already loaded or if this function is part of a package-like workflow.

## 7. Returned objects

The function returns a list.

### 7.1 input_summary

One-row tibble summarizing the analysis settings.

Main fields:

```text
n_rows
n_attributes_evaluated
n_combinations
min_size
max_size
ranking_metric
selection_priority
required_attrs
target_attrs
depth_source
depth_method
use_valid_ranges_for_combinations
compute_representativeness
graphs
graph_top_n
```

### 7.2 attr_summary

Attribute-level completeness table.

Main fields:

```text
attribute
n_rows
has_valid_range
n_non_na
n_na
pct_non_na
pct_na
n_valid
n_invalid
pct_valid
pct_invalid
```

### 7.3 row_attr_summary

Row-level attribute availability table.

Main fields:

```text
n_attr_available
n_rows
pct_rows
```

### 7.4 combo_summary

Complete table with all evaluated combinations.

Main fields:

```text
combo_id
n_attributes
attributes
attribute_weight_sum
attribute_weight_mean
n_rows_complete
pct_rows_complete
n_rows_missing
pct_rows_missing
mean_n_units_complete
n_depth_classes_complete
weighted_score
representativeness_score
```

If `unit_cols` is supplied, additional columns are created, such as:

```text
n_coord_id_complete
pct_coord_id_complete
```

### 7.5 best_combo_by_size

Best combination for each number of attributes according to `ranking_metric`.

### 7.6 top_combo_by_size

Top N combinations within each number of attributes.

### 7.7 selection_candidates

Combinations that pass `min_rows` and `min_pct`, ordered according to `selection_priority`.

This is the most important output for selecting a dataset.

When `return_selected = TRUE` and `selected_attrs = NULL`, the first row of `selection_candidates` is used to create `selected_data`.

### 7.8 pareto_combo

Pareto-style summary showing combinations that represent trade-offs between number of attributes and number of complete rows.

### 7.9 marginal_loss_summary

Shows how many rows are lost when moving from the best combination of one size to the best combination of the next size.

Main fields:

```text
previous_attributes
previous_n_rows_complete
marginal_rows_lost
marginal_pct_lost
cumulative_pct_remaining
```

### 7.10 attribute_bottleneck_summary

Attribute-level diagnostic showing which attributes reduce combination completeness most strongly.

Main fields:

```text
attribute
n_non_na
pct_non_na
n_valid
pct_valid
n_combinations_with_attribute
pct_combinations_with_attribute
mean_rows_with_attribute
median_rows_with_attribute
mean_rows_without_attribute
bottleneck_effect
bottleneck_intensity
bottleneck_class
n_pareto_combinations_with_attribute
n_best_by_size_combinations_with_attribute
```

Possible `bottleneck_class` values:

```text
required_attribute_not_evaluated
not_evaluated
no_bottleneck_detected
low_bottleneck
moderate_bottleneck
high_bottleneck
```

### 7.11 missingness_correlation

Matrix of correlations among missingness patterns.

### 7.12 missingness_correlation_long

Long-format table of pairwise missingness correlations.

Main fields:

```text
attribute_1
attribute_2
missingness_correlation
```

### 7.13 depth_completeness_summary

Completeness by depth class for diagnostic combinations.

Returned only when `depth_cols` or `depth_class_col` is supplied.

### 7.14 unit_completeness_summary

Completeness by sampling unit for diagnostic combinations.

Returned only when `unit_cols` is supplied.

### 7.15 target_combo_summary

List with target-specific combination summaries.

Returned only when `target_attrs` is supplied.

Includes:

```text
top_target_combinations
best_target_plus_by_size
```

### 7.16 selected_summary

Summary of the selected dataset when `return_selected = TRUE`.

Main fields:

```text
selected_attributes
n_attributes
n_rows_original
n_rows_selected
pct_rows_selected
n_rows_removed
```

If `unit_cols` is supplied, additional unit-level fields are added.

If depth information is supplied, depth-class counts are added.

### 7.17 selected_data

Dataset filtered to rows complete for `selected_attrs`.

Returned only when `return_selected = TRUE`.

### 7.18 graphs

Named list of `ggplot` objects.

Returned as `NULL` when `graphs = FALSE`.

When `graphs = TRUE`, possible graph names include:

```text
attribute_completeness
row_attribute_availability
best_combo_by_size
marginal_loss
bottleneck_intensity
selection_candidates
missingness_correlation
depth_selected_candidate
unit_selected_candidate
```

Some graphs are returned only when the required diagnostic information exists.

For example, `depth_selected_candidate` is returned only when depth information is supplied, and `unit_selected_candidate` is returned only when `unit_cols` is supplied.

### 7.19 decision_notes

Short automatically generated notes summarizing the analysis settings and key diagnostics.

## 8. Graphical outputs

Graphical outputs are optional.

Use:

```r
graphs = TRUE
```

to return diagnostic plots.

Graphs are generated after all tabular diagnostics have been computed.

This means that graphs do not alter:

1. Attribute combinations.
2. Ranking.
3. Selected datasets.
4. Bottleneck diagnostics.
5. Missingness diagnostics.
6. Depth diagnostics.
7. Unit diagnostics.

Example:

```r
res_graphs <- soil_attr_balance(
  data = soil_data,
  attrs = soil_attributes,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  return_selected = TRUE,
  graphs = TRUE,
  graph_top_n = 30
)
```

View available graph names:

```r
names(res_graphs$graphs)
```

View individual graphs:

```r
res_graphs$graphs$attribute_completeness

res_graphs$graphs$row_attribute_availability

res_graphs$graphs$best_combo_by_size

res_graphs$graphs$marginal_loss

res_graphs$graphs$bottleneck_intensity

res_graphs$graphs$selection_candidates

res_graphs$graphs$missingness_correlation

res_graphs$graphs$depth_selected_candidate

res_graphs$graphs$unit_selected_candidate
```

Save one graph:

```r
ggplot2::ggsave(
  filename = "attribute_completeness.png",
  plot = res_graphs$graphs$attribute_completeness,
  width = 8,
  height = 6,
  dpi = 300
)
```

Save all available graphs:

```r
dir.create(
  "soil_attr_balance_graphs",
  showWarnings = FALSE,
  recursive = TRUE
)

purrr::iwalk(
  res_graphs$graphs,
  function(plot_obj, plot_name) {
    
    ggplot2::ggsave(
      filename = file.path(
        "soil_attr_balance_graphs",
        paste0(plot_name, ".png")
      ),
      plot = plot_obj,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
)
```

## 9. Recommended workflows

### 9.1 General exploratory diagnosis

Use this when you want to evaluate all combinations and inspect the global database structure.

```r
res_general <- soil_attr_balance(
  data = k_bind,
  attrs = atr,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  target_attrs = "c_gkg",
  min_size = 2,
  max_size = length(atr),
  top_n = 20,
  parallel = TRUE
)

res_general$attr_summary
res_general$row_attr_summary
res_general$best_combo_by_size
res_general$target_combo_summary
res_general$decision_notes
```

### 9.2 Carbon-focused analysis

Use this when every evaluated combination must contain soil organic carbon.

```r
res_carbon <- soil_attr_balance(
  data = k_bind,
  attrs = atr,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  min_size = 2,
  max_size = length(atr),
  min_pct = 0.10,
  top_n = 20,
  parallel = TRUE
)

res_carbon$selection_candidates
res_carbon$attribute_bottleneck_summary
```

### 9.3 Weighted carbon-focused analysis

Use this when some attributes are more important than others for the intended modeling or pedotransfer application.

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
  p_mgkg = 2
)

res_weighted <- soil_attr_balance(
  data = k_bind,
  attrs = atr,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_soil,
  min_pct = 0.10,
  ranking_metric = "weighted_score",
  selection_priority = "metric_first",
  parallel = TRUE
)

res_weighted$selection_candidates
```

### 9.4 Strict analysis with valid ranges

Use this when invalid values should be treated as unavailable during combination analysis.

```r
valid_ranges_soil <- list(
  c_gkg = c(0, 600),
  sand_gkg = c(0, 1000),
  silt_gkg = c(0, 1000),
  clay_gkg = c(0, 1000),
  p_h_h2o = c(2, 12),
  ca_mmolkg = c(0, 2000),
  mg_mmolkg = c(0, 2000),
  k_mmolkg = c(0, 2000),
  al_mmolkg = c(0, 2000),
  h_al_mmolkg = c(0, 2000),
  cec_ph7_mmolkg = c(0, 3000),
  p_mgkg = c(0, 10000)
)

res_strict <- soil_attr_balance(
  data = k_bind,
  attrs = atr,
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
  return_selected = TRUE,
  parallel = TRUE
)

res_strict$selected_summary
res_strict$selected_data
```

### 9.5 Richness-first selection

Use this to select the richest possible dataset in terms of number of attributes, after applying `min_pct` or `min_rows`.

```r
res_richness <- soil_attr_balance(
  data = k_bind,
  attrs = atr_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_soil[names(attribute_weights_soil) %in% atr_core],
  valid_ranges = valid_ranges_soil[names(valid_ranges_soil) %in% atr_core],
  use_valid_ranges_for_combinations = TRUE,
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "richness_first",
  return_selected = TRUE,
  parallel = TRUE
)

res_richness$selected_summary
```

### 9.6 Rows-first selection with minimum size

Use this when sample size is the main priority, but only among combinations with at least a minimum number of attributes.

This is important because `rows_first` without `min_size` may select very small combinations such as two attributes.

```r
res_rows_10 <- soil_attr_balance(
  data = k_bind,
  attrs = atr_core,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  attribute_weights = attribute_weights_soil[names(attribute_weights_soil) %in% atr_core],
  valid_ranges = valid_ranges_soil[names(valid_ranges_soil) %in% atr_core],
  use_valid_ranges_for_combinations = TRUE,
  min_size = 10,
  max_size = length(atr_core),
  min_pct = 0.50,
  ranking_metric = "weighted_score",
  selection_priority = "rows_first",
  return_selected = TRUE,
  parallel = TRUE
)

res_rows_10$selected_summary
```

### 9.7 Comparing selection priorities

Use this when you want to compare the effect of `selection_priority`.

```r
dplyr::bind_rows(
  richness_first = res_richness$selected_summary,
  metric_first = res_metric$selected_summary,
  rows_first = res_rows$selected_summary,
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
  ) %>%
  print(width = Inf)
```

## 10. Practical interpretation guidelines

### 10.1 attr_summary

Use `attr_summary` to identify frequent and rare attributes.

Rare attributes may be important, but they can strongly reduce the number of complete combinations.

### 10.2 row_attr_summary

Use `row_attr_summary` to understand whether the database is broadly complete or highly fragmented.

### 10.3 combo_summary

Use `combo_summary` for full custom inspection.

This is the complete table with all evaluated combinations.

### 10.4 best_combo_by_size

Use `best_combo_by_size` to compare the best two-attribute, three-attribute, four-attribute, and larger combinations.

### 10.5 selection_candidates

Use `selection_candidates` for decision-making.

It respects:

```text
min_rows
min_pct
ranking_metric
selection_priority
```

### 10.6 pareto_combo

Use `pareto_combo` to visualize trade-offs between number of attributes and number of complete rows.

### 10.7 marginal_loss_summary

Use `marginal_loss_summary` to identify where adding one more attribute causes a large drop in complete rows.

### 10.8 attribute_bottleneck_summary

Use `attribute_bottleneck_summary` to identify attributes that strongly limit data availability.

### 10.9 missingness_correlation

Use `missingness_correlation` to identify groups of attributes that tend to be missing together.

### 10.10 depth_completeness_summary

Use `depth_completeness_summary` to check whether selected combinations are concentrated in specific depth intervals.

### 10.11 unit_completeness_summary

Use `unit_completeness_summary` to avoid decisions based only on row counts when the same profile, sample, or coordinate may contribute multiple rows.

### 10.12 selected_summary

Use `selected_summary` to inspect the final size of the selected dataset when `return_selected = TRUE`.

### 10.13 selected_data

Use `selected_data` as the filtered database for downstream analysis, modeling, or export.

### 10.14 graphs

Use `graphs` to visually inspect the main diagnostics.

Graphs are especially useful for reports, exploratory diagnosis, and communication of database trade-offs.

## 11. Suggested analysis sequence

1. Run a general diagnosis with all candidate attributes.
2. Inspect `attr_summary` and `attribute_bottleneck_summary`.
3. Remove attributes that are too rare for the main analysis, if needed.
4. Define `required_attrs` when the analysis has a mandatory attribute, such as `c_gkg`.
5. Define `attribute_weights` if some attributes are scientifically more important.
6. Define `valid_ranges` and decide whether they should be used only as diagnostics or directly in combination completeness.
7. Set `min_pct` according to the acceptable minimum database size.
8. Compare `selection_priority` values.
9. Use `graphs = TRUE` when visual diagnosis is useful.
10. Use `return_selected = TRUE` to generate the final selected dataset.

## 12. Notes for interpretation

A combination with more attributes is not always better.

A combination with more rows is not always better.

A combination with a high `weighted_score` depends on the weights supplied by the user.

A high `representativeness_score` should be interpreted as a practical screening metric, not as a formal statistical guarantee.

Rare attributes should often be evaluated in separate targeted analyses rather than forced into a general modeling dataset.

If `rows_first` is used, consider setting `min_size` to avoid selecting overly small combinations.

If `richness_first` is used, consider setting `min_pct` to avoid selecting very rich but very small datasets.

If `metric_first` is used, make sure `attribute_weights` or the selected `ranking_metric` reflects the actual scientific goal.

If `graphs = TRUE`, remember that graphs are returned as R objects and are not saved automatically.

## 13. Reproducibility note

This function does not change the input object in place.

The selected dataset is returned as `selected_data` only when `return_selected = TRUE`.

The graph outputs are returned as `ggplot` objects only when `graphs = TRUE`.

The function should be sourced from a script or function file.

Avoid using this inside the function file:

```r
rm(list = ls())
```

because it may remove objects from the user's working environment when sourced.

## 14. Minimal reproducible template

```r
source("https://raw.githubusercontent.com/moquedace/funcs/main/utils/soil_attr_balance.R")

attrs <- c(
  "c_gkg",
  "sand_gkg",
  "silt_gkg",
  "clay_gkg",
  "p_h_h2o",
  "ca_mmolkg",
  "mg_mmolkg",
  "k_mmolkg",
  "al_mmolkg",
  "h_al_mmolkg",
  "cec_ph7_mmolkg"
)

res <- soil_attr_balance(
  data = soil_data,
  attrs = attrs,
  unit_cols = "coord_id",
  depth_cols = c("upper_depth_cm", "lower_depth_cm"),
  required_attrs = "c_gkg",
  target_attrs = "c_gkg",
  min_pct = 0.50,
  ranking_metric = "n_rows_complete",
  selection_priority = "richness_first",
  return_selected = TRUE,
  graphs = TRUE,
  graph_top_n = 30
)

res$selected_summary
res$decision_notes

names(res$graphs)

res$graphs$attribute_completeness
```
