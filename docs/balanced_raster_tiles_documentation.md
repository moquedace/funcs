# balanced_raster_tiles

## Purpose

`balanced_raster_tiles()` creates a vector tile grid from a raster base using the number of valid pixels as the balancing criterion.

The function is designed for raster processing workflows in which the goal is to split a large raster domain into operational tiles with approximately the same number of valid cells. This is useful when a raster contains large empty regions, such as water, no-data areas, or regions outside the modeling domain, and a regular grid would create tiles with very different processing loads.

The function does not attempt to make tiles with equal geographic area. It prioritizes similarity in the number of valid pixels per tile.

## Typical use

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/balanced_raster_tiles.R"
)

rbase <- terra::rast("path/to/base_raster.tif")

tile_result <- balanced_raster_tiles(
  base_raster = rbase,
  n_tile_rows = 10,
  n_tile_cols = 10,
  output_dir = "output/balanced_tiles",
  output_prefix = "balanced_tiles_10x10",
  diagnostic_plots = TRUE
)
```

The main vector output is:

```r
tile_result$tiles
```

This object can be used as a spatial template to crop other rasters.

## Core idea

The function creates a logical valid-pixel mask from the base raster.

By default:

```r
valid pixel = finite non-NA value
invalid pixel = NA, NaN, Inf, or -Inf
```

If `valid_values` is supplied, valid pixels are defined by membership in that set of values.

The tiling procedure has two steps:

1. The raster rows are split into `n_tile_rows` horizontal ranges with approximately the same number of valid pixels.
2. Inside each row range, raster columns are split into `n_tile_cols` vertical ranges with approximately the same number of valid pixels.

This generates:

```r
n_tile_rows * n_tile_cols
```

tiles.

## Function name

The source file creates two equivalent function names:

```r
balanced_raster_tiles()
make_balanced_raster_tiles()
```

Both names call the same function.

## Arguments

### `base_raster`

A `terra::SpatRaster` object or a single raster file path.

If the raster has more than one layer, only the first layer is used to define valid pixels and tile geometry.

### `n_tile_rows`

Number of adaptive row groups.

Default:

```r
10
```

### `n_tile_cols`

Number of adaptive column groups within each row group.

Default:

```r
10
```

The total number of tiles is:

```r
n_tile_rows * n_tile_cols
```

### `valid_values`

Optional vector of values to be treated as valid.

Default:

```r
NULL
```

When `valid_values = NULL`, all finite non-NA raster cells are considered valid.

Example:

```r
valid_values = 1
```

This is useful when the base raster is a binary mask where `1` indicates the valid domain and `NA` or `0` indicates the invalid domain.

### `output_dir`

Directory where outputs will be saved.

Default:

```r
NULL
```

If `save_outputs = TRUE`, `output_dir` must be provided.

### `output_prefix`

Prefix used for output files.

Default:

```r
"balanced_raster_tiles"
```

### `save_outputs`

Logical value indicating whether outputs should be written to disk.

Default:

```r
!is.null(output_dir)
```

### `overwrite`

Logical value indicating whether existing output files should be overwritten.

Default:

```r
TRUE
```

### `diagnostic_plots`

Logical value indicating whether diagnostic plots should be created.

Default:

```r
FALSE
```

### `verbose`

Logical value indicating whether progress messages should be printed.

Default:

```r
TRUE
```

## Returned object

The function returns a list with five elements.

### `tiles`

An `sf` object with one polygon per tile.

Main fields:

```r
tile_id
tile_row
tile_col
row_start
row_end
col_start
col_end
n_raster_rows
n_raster_cols
n_total_pixels
n_valid_pixels
valid_fraction
target_valid_pixels
valid_pixel_error
valid_pixel_error_pct
```

### `tile_summary`

A non-spatial table with the same tile attributes used in `tiles`.

### `global_summary`

A one-row table with global diagnostics.

Main fields:

```r
total_tiles
total_valid_pixels
target_valid_pixels
min_valid_pixels
max_valid_pixels
mean_valid_pixels
median_valid_pixels
sd_valid_pixels
cv_valid_pixels
max_abs_valid_pixel_error
max_abs_valid_pixel_error_pct
empty_tiles
balance_status
```

### `row_ranges`

Table describing the adaptive row splits.

### `column_ranges`

Table describing the adaptive column splits inside each row group.

## Main diagnostic variables

### `n_valid_pixels`

Number of valid pixels inside the tile.

This is the main variable used to evaluate the quality of the tiling result.

### `target_valid_pixels`

Expected number of valid pixels per tile:

```r
total_valid_pixels / total_tiles
```

### `valid_pixel_error`

Difference between observed and target valid pixels:

```r
n_valid_pixels - target_valid_pixels
```

### `valid_pixel_error_pct`

Percent difference relative to the target:

```r
100 * valid_pixel_error / target_valid_pixels
```

### `cv_valid_pixels`

Coefficient of variation of `n_valid_pixels` across tiles.

This is the main global diagnostic.

Suggested interpretation:

| cv_valid_pixels | Interpretation |
|---:|---|
| <= 0.02 | excellent |
| <= 0.05 | good |
| <= 0.10 | acceptable |
| > 0.10 | poor |

### `balance_status`

Automatic classification based on `cv_valid_pixels`, `max_abs_valid_pixel_error_pct`, and empty tiles.

Possible values:

```r
"excellent"
"good"
"acceptable"
"poor"
```

## Important interpretation

The function balances the number of valid pixels per tile. It does not try to equalize:

```r
n_total_pixels
valid_fraction
geographic area
file size after crop
```

A tile can have a large rectangular extent and still be valid if it has a number of valid pixels close to the target.

This behavior is intentional.

## Recommended quality criterion

For processing workflows focused on valid pixels, the preferred result is:

```r
balance_status == "excellent"
```

or:

```r
cv_valid_pixels <= 0.02
max_abs_valid_pixel_error_pct <= 5
```

## Output files

When `save_outputs = TRUE`, the function writes:

```r
<output_prefix>.gpkg
<output_prefix>_tile_summary.csv
<output_prefix>_global_summary.csv
<output_prefix>_row_ranges.csv
<output_prefix>_column_ranges.csv
```

CSV files are written with:

```r
readr::write_csv2()
```

## Notes

The function uses `terra::as.matrix()` internally. This is appropriate for raster bases with manageable dimensions, such as global or continental masks at moderate resolution. For extremely large raster bases, consider creating a coarser mask raster first and using that mask as the tiling base.

The generated polygons are rectangular and aligned to the cell boundaries of the input raster.
