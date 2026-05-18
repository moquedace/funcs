# balanced_raster_tiles examples

## Example 1. Basic use with a raster already loaded in R

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
)

pkg <- c(
  "dplyr",
  "terra",
  "sf",
  "readr"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/balanced_raster_tiles.R"
)

project_root <- "D:/usuario_armazenamento/cassio/R/balanced_tiles"

setwd(project_root)

rbase <- terra::rast(
  "../predictors_resolution_20000m/ensemble_digital_terrain_model_v1_1.tif"
)

tile_result <- balanced_raster_tiles(
  base_raster = rbase,
  n_tile_rows = 10,
  n_tile_cols = 10,
  output_dir = "output/final_10x10",
  output_prefix = "balanced_tiles_10x10",
  diagnostic_plots = TRUE,
  overwrite = TRUE
)

tile_result$global_summary %>%
  print(width = Inf)
```

## Example 2. Inspect the most imbalanced tiles

```r
tile_result$tile_summary %>%
  dplyr::arrange(
    dplyr::desc(abs(valid_pixel_error_pct))
  ) %>%
  dplyr::select(
    tile_id,
    tile_row,
    tile_col,
    n_valid_pixels,
    target_valid_pixels,
    valid_pixel_error,
    valid_pixel_error_pct,
    n_total_pixels,
    valid_fraction
  ) %>%
  print(n = 20, width = Inf)
```

## Example 3. Inspect the lightest and heaviest tiles

```r
tile_result$tile_summary %>%
  dplyr::arrange(n_valid_pixels) %>%
  dplyr::select(
    tile_id,
    tile_row,
    tile_col,
    n_valid_pixels,
    target_valid_pixels,
    valid_pixel_error_pct
  ) %>%
  print(n = 20, width = Inf)

tile_result$tile_summary %>%
  dplyr::arrange(
    dplyr::desc(n_valid_pixels)
  ) %>%
  dplyr::select(
    tile_id,
    tile_row,
    tile_col,
    n_valid_pixels,
    target_valid_pixels,
    valid_pixel_error_pct
  ) %>%
  print(n = 20, width = Inf)
```

## Example 4. Use a raster file path directly

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/balanced_raster_tiles.R"
)

tile_result <- balanced_raster_tiles(
  base_raster = "../predictors_resolution_20000m/ensemble_digital_terrain_model_v1_1.tif",
  n_tile_rows = 10,
  n_tile_cols = 10,
  output_dir = "output/from_file_path",
  output_prefix = "balanced_tiles_from_file_path",
  diagnostic_plots = FALSE,
  overwrite = TRUE
)

tile_result$global_summary %>%
  print(width = Inf)
```

## Example 5. Use a binary mask as the valid domain

Use this when the base raster contains `1` for valid cells and `0` or `NA` elsewhere.

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/balanced_raster_tiles.R"
)

mask_raster <- terra::rast(
  "data/masks/modeling_domain_mask.tif"
)

tile_result <- balanced_raster_tiles(
  base_raster = mask_raster,
  n_tile_rows = 10,
  n_tile_cols = 10,
  valid_values = 1,
  output_dir = "output/mask_based_tiles",
  output_prefix = "balanced_tiles_mask_10x10",
  diagnostic_plots = TRUE,
  overwrite = TRUE
)
```

## Example 6. Compare different layouts

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
)

pkg <- c(
  "dplyr",
  "terra",
  "tibble",
  "readr",
  "purrr"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/balanced_raster_tiles.R"
)

rbase <- terra::rast(
  "../predictors_resolution_20000m/ensemble_digital_terrain_model_v1_1.tif"
)

layout_tests <- tibble::tibble(
  n_tile_rows = c(5, 10, 10, 20),
  n_tile_cols = c(20, 10, 20, 10)
)

layout_diagnostics <- purrr::pmap_dfr(
  .l = list(
    n_tile_rows = layout_tests$n_tile_rows,
    n_tile_cols = layout_tests$n_tile_cols
  ),
  .f = function(n_tile_rows, n_tile_cols) {

    tile_result_i <- balanced_raster_tiles(
      base_raster = rbase,
      n_tile_rows = n_tile_rows,
      n_tile_cols = n_tile_cols,
      output_dir = file.path(
        "output",
        paste0("layout_", n_tile_rows, "x", n_tile_cols)
      ),
      output_prefix = paste0(
        "balanced_tiles_",
        n_tile_rows,
        "x",
        n_tile_cols
      ),
      diagnostic_plots = FALSE,
      overwrite = TRUE,
      verbose = TRUE
    )

    tile_result_i$global_summary %>%
      dplyr::mutate(
        n_tile_rows = n_tile_rows,
        n_tile_cols = n_tile_cols,
        .before = 1
      )
  }
)

layout_diagnostics %>%
  dplyr::arrange(cv_valid_pixels) %>%
  dplyr::select(
    n_tile_rows,
    n_tile_cols,
    total_tiles,
    target_valid_pixels,
    cv_valid_pixels,
    max_abs_valid_pixel_error_pct,
    balance_status
  ) %>%
  print(width = Inf)

readr::write_csv2(
  x = layout_diagnostics,
  file = "output/layout_diagnostics.csv"
)
```

## Example 7. Crop a stack of rasters using the generated tiles

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
)

pkg <- c(
  "dplyr",
  "terra",
  "sf",
  "stringr"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/balanced_raster_tiles.R"
)

project_root <- "D:/usuario_armazenamento/cassio/R/balanced_tiles"

setwd(project_root)

rbase <- terra::rast(
  "../predictors_resolution_20000m/ensemble_digital_terrain_model_v1_1.tif"
)

tile_result <- balanced_raster_tiles(
  base_raster = rbase,
  n_tile_rows = 10,
  n_tile_cols = 10,
  output_dir = "output/final_10x10",
  output_prefix = "balanced_tiles_10x10",
  diagnostic_plots = FALSE,
  overwrite = TRUE
)

raster_files <- list.files(
  path = "../predictors_resolution_20000m",
  pattern = "\\.tif$",
  full.names = TRUE
)

tile_output_dir <- "output/raster_tiles"

if (!dir.exists(tile_output_dir)) {
  dir.create(
    tile_output_dir,
    recursive = TRUE
  )
}

for (tile_i in seq_len(nrow(tile_result$tiles))) {

  tile_vect_i <- terra::vect(
    tile_result$tiles[tile_i, ]
  )

  tile_id_i <- tile_result$tiles$tile_id[tile_i]

  tile_dir_i <- file.path(
    tile_output_dir,
    paste0("tile_", stringr::str_pad(tile_id_i, width = 3, pad = "0"))
  )

  if (!dir.exists(tile_dir_i)) {
    dir.create(
      tile_dir_i,
      recursive = TRUE
    )
  }

  for (raster_file_i in raster_files) {

    raster_name_i <- tools::file_path_sans_ext(
      basename(raster_file_i)
    )

    output_file_i <- file.path(
      tile_dir_i,
      paste0(
        raster_name_i,
        "_tile_",
        stringr::str_pad(tile_id_i, width = 3, pad = "0"),
        ".tif"
      )
    )

    raster_i <- terra::rast(raster_file_i)

    raster_tile_i <- raster_i %>%
      terra::crop(tile_vect_i)

    terra::writeRaster(
      x = raster_tile_i,
      filename = output_file_i,
      overwrite = TRUE,
      gdal = c(
        "COMPRESS=LZW",
        "TILED=YES"
      )
    )

    rm(raster_i, raster_tile_i)
    gc()
  }
}
```

## Example 8. Read the saved tile vector later

```r
tiles <- sf::st_read(
  "output/final_10x10/balanced_tiles_10x10.gpkg",
  layer = "balanced_raster_tiles",
  quiet = TRUE
)

tiles %>%
  dplyr::select(
    tile_id,
    tile_row,
    tile_col,
    n_valid_pixels,
    target_valid_pixels,
    valid_pixel_error_pct
  ) %>%
  print(n = 20)
```
