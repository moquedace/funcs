balanced_raster_tiles <- local({

  balanced_raster_tiles_load_packages <- function() {

    pkg <- c(
      "dplyr",
      "terra",
      "sf",
      "tibble",
      "readr"
    )

    if (!exists("install_load_pkg", mode = "function")) {
      try(
        source(
          "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
        ),
        silent = TRUE
      )
    }

    if (exists("install_load_pkg", mode = "function")) {
      invisible(
        install_load_pkg(pkg)
      )
    } else {
      missing_pkg <- pkg[
        !vapply(
          pkg,
          requireNamespace,
          quietly = TRUE,
          FUN.VALUE = logical(1)
        )
      ]

      if (length(missing_pkg) > 0) {
        stop(
          paste0(
            "These packages are missing and could not be loaded: ",
            paste(missing_pkg, collapse = ", ")
          )
        )
      }

      invisible(
        lapply(
          pkg,
          library,
          character.only = TRUE
        )
      )
    }
  }

  balanced_raster_tiles_load_packages()

  balanced_message <- function(..., verbose = TRUE) {

    if (isTRUE(verbose)) {
      message(...)
    }

    invisible(NULL)
  }

  balanced_sum_range <- function(weights, start_index, end_index) {

    vapply(
      X = end_index,
      FUN = function(end_index_i) {
        sum(
          weights[start_index:end_index_i],
          na.rm = TRUE
        )
      },
      FUN.VALUE = numeric(1)
    )
  }

  balanced_safe_ratio <- function(x, y) {

    ratio <- rep(NA_real_, length(x))
    valid_denominator <- !is.na(y) & y != 0
    ratio[valid_denominator] <- x[valid_denominator] / y[valid_denominator]
    ratio
  }

  balanced_prepare_raster <- function(base_raster) {

    if (inherits(base_raster, "character")) {
      if (length(base_raster) != 1) {
        stop("base_raster must be a single file path when provided as character.")
      }

      if (!file.exists(base_raster)) {
        stop(
          paste0(
            "base_raster file was not found: ",
            base_raster
          )
        )
      }

      base_raster <- terra::rast(base_raster)
    }

    if (!inherits(base_raster, "SpatRaster")) {
      stop("base_raster must be a terra SpatRaster or a valid raster file path.")
    }

    if (terra::nlyr(base_raster) > 1) {
      message("base_raster has more than one layer. Only the first layer will be used.")
      base_raster <- base_raster[[1]]
    }

    base_raster
  }

  balanced_make_valid_matrix <- function(base_raster, valid_values = NULL) {

    base_matrix <- terra::as.matrix(
      x = base_raster,
      wide = TRUE
    )

    if (is.null(valid_values)) {

      if (is.numeric(base_matrix) || is.integer(base_matrix)) {
        valid_matrix <- is.finite(base_matrix)
      } else {
        valid_matrix <- !is.na(base_matrix)
      }

    } else {

      valid_matrix <- base_matrix %in% valid_values
    }

    rm(base_matrix)
    gc()

    storage.mode(valid_matrix) <- "logical"

    valid_matrix
  }

  balanced_make_ranges <- function(weights, n_groups, axis_name = "index") {

    weights <- as.numeric(weights)
    n_items <- length(weights)

    if (n_groups < 1) {
      stop("n_groups must be greater than or equal to 1.")
    }

    if (n_groups > n_items) {
      stop(
        paste0(
          "n_groups cannot be greater than the number of available ",
          axis_name,
          " positions."
        )
      )
    }

    total_weight <- sum(
      weights,
      na.rm = TRUE
    )

    if (total_weight <= 0) {
      stop(
        paste0(
          "The total valid pixel weight is zero for ",
          axis_name,
          "."
        )
      )
    }

    if (total_weight < n_groups) {
      stop(
        paste0(
          "The number of requested groups for ",
          axis_name,
          " is greater than the number of valid pixels available in this split."
        )
      )
    }

    target_weight <- total_weight / n_groups
    cumulative_weight <- cumsum(weights)

    ranges <- vector(
      mode = "list",
      length = n_groups
    )

    start_index <- 1

    for (group_id in seq_len(n_groups)) {

      if (group_id == n_groups) {

        end_index <- n_items

      } else {

        target_boundary <- target_weight * group_id

        candidate_high <- which(cumulative_weight >= target_boundary)[1]

        if (is.na(candidate_high)) {
          candidate_high <- n_items
        }

        candidate_low <- candidate_high - 1

        min_end_index <- start_index
        max_end_index <- n_items - (n_groups - group_id)

        candidate_high <- max(candidate_high, min_end_index)
        candidate_high <- min(candidate_high, max_end_index)

        candidate_low <- max(candidate_low, min_end_index)
        candidate_low <- min(candidate_low, max_end_index)

        candidate_table <- tibble::tibble(
          candidate_index = unique(c(candidate_low, candidate_high))
        ) %>%
          dplyr::mutate(
            candidate_weight = balanced_sum_range(
              weights = weights,
              start_index = start_index,
              end_index = candidate_index
            ),
            candidate_error = abs(candidate_weight - target_weight)
          ) %>%
          dplyr::arrange(
            candidate_error,
            candidate_index
          )

        end_index <- candidate_table$candidate_index[1]
      }

      ranges[[group_id]] <- tibble::tibble(
        group_id = group_id,
        start_index = start_index,
        end_index = end_index,
        n_items = end_index - start_index + 1,
        weight = sum(
          weights[start_index:end_index],
          na.rm = TRUE
        ),
        target_weight = target_weight,
        weight_error = weight - target_weight,
        weight_error_pct = 100 * (weight - target_weight) / target_weight
      )

      start_index <- end_index + 1
    }

    ranges %>%
      dplyr::bind_rows()
  }

  balanced_make_tile_polygon <- function(
      raster_base,
      row_start,
      row_end,
      col_start,
      col_end
  ) {

    xres_value <- terra::xres(raster_base)
    yres_value <- terra::yres(raster_base)

    xmin_value <- terra::xFromCol(
      object = raster_base,
      col = col_start
    ) - xres_value / 2

    xmax_value <- terra::xFromCol(
      object = raster_base,
      col = col_end
    ) + xres_value / 2

    ymax_value <- terra::yFromRow(
      object = raster_base,
      row = row_start
    ) + yres_value / 2

    ymin_value <- terra::yFromRow(
      object = raster_base,
      row = row_end
    ) - yres_value / 2

    sf::st_polygon(
      list(
        matrix(
          c(
            xmin_value, ymin_value,
            xmax_value, ymin_value,
            xmax_value, ymax_value,
            xmin_value, ymax_value,
            xmin_value, ymin_value
          ),
          ncol = 2,
          byrow = TRUE
        )
      )
    )
  }

  balanced_summarise_global <- function(tile_summary) {

    tile_summary %>%
      dplyr::summarise(
        total_tiles = dplyr::n(),
        total_valid_pixels = sum(n_valid_pixels, na.rm = TRUE),
        target_valid_pixels = dplyr::first(target_valid_pixels),
        min_valid_pixels = min(n_valid_pixels, na.rm = TRUE),
        max_valid_pixels = max(n_valid_pixels, na.rm = TRUE),
        mean_valid_pixels = mean(n_valid_pixels, na.rm = TRUE),
        median_valid_pixels = stats::median(n_valid_pixels, na.rm = TRUE),
        sd_valid_pixels = stats::sd(n_valid_pixels, na.rm = TRUE),
        cv_valid_pixels = sd_valid_pixels / mean_valid_pixels,
        max_abs_valid_pixel_error = max(abs(valid_pixel_error), na.rm = TRUE),
        max_abs_valid_pixel_error_pct = max(abs(valid_pixel_error_pct), na.rm = TRUE),
        min_total_pixels = min(n_total_pixels, na.rm = TRUE),
        max_total_pixels = max(n_total_pixels, na.rm = TRUE),
        mean_total_pixels = mean(n_total_pixels, na.rm = TRUE),
        median_total_pixels = stats::median(n_total_pixels, na.rm = TRUE),
        min_valid_fraction = min(valid_fraction, na.rm = TRUE),
        max_valid_fraction = max(valid_fraction, na.rm = TRUE),
        mean_valid_fraction = mean(valid_fraction, na.rm = TRUE),
        median_valid_fraction = stats::median(valid_fraction, na.rm = TRUE),
        empty_tiles = sum(n_valid_pixels == 0, na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        balance_status = dplyr::case_when(
          empty_tiles > 0 ~ "poor",
          cv_valid_pixels <= 0.02 & max_abs_valid_pixel_error_pct <= 5 ~ "excellent",
          cv_valid_pixels <= 0.05 & max_abs_valid_pixel_error_pct <= 10 ~ "good",
          cv_valid_pixels <= 0.10 & max_abs_valid_pixel_error_pct <= 20 ~ "acceptable",
          TRUE ~ "poor"
        )
      )
  }

  balanced_save_outputs <- function(
      tiles_sf,
      tile_summary,
      global_summary,
      row_ranges,
      column_ranges,
      output_dir,
      output_prefix,
      overwrite,
      verbose
  ) {

    if (is.null(output_dir)) {
      return(invisible(NULL))
    }

    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }

    gpkg_file <- file.path(
      output_dir,
      paste0(output_prefix, ".gpkg")
    )

    tile_summary_file <- file.path(
      output_dir,
      paste0(output_prefix, "_tile_summary.csv")
    )

    global_summary_file <- file.path(
      output_dir,
      paste0(output_prefix, "_global_summary.csv")
    )

    row_ranges_file <- file.path(
      output_dir,
      paste0(output_prefix, "_row_ranges.csv")
    )

    column_ranges_file <- file.path(
      output_dir,
      paste0(output_prefix, "_column_ranges.csv")
    )

    if (file.exists(gpkg_file) && isTRUE(overwrite)) {
      file.remove(gpkg_file)
    }

    sf::st_write(
      obj = tiles_sf,
      dsn = gpkg_file,
      layer = "balanced_raster_tiles",
      delete_layer = overwrite,
      quiet = TRUE
    )

    readr::write_csv2(
      x = tile_summary,
      file = tile_summary_file
    )

    readr::write_csv2(
      x = global_summary,
      file = global_summary_file
    )

    readr::write_csv2(
      x = row_ranges,
      file = row_ranges_file
    )

    readr::write_csv2(
      x = column_ranges,
      file = column_ranges_file
    )

    balanced_message(
      "Saved outputs:",
      verbose = verbose
    )

    balanced_message(
      gpkg_file,
      verbose = verbose
    )

    balanced_message(
      tile_summary_file,
      verbose = verbose
    )

    balanced_message(
      global_summary_file,
      verbose = verbose
    )

    balanced_message(
      row_ranges_file,
      verbose = verbose
    )

    balanced_message(
      column_ranges_file,
      verbose = verbose
    )

    invisible(NULL)
  }

  balanced_plot_diagnostics <- function(base_raster, tiles_sf, tile_summary) {

    plot(
      base_raster,
      main = "Base raster with balanced tiles"
    )

    plot(
      sf::st_geometry(tiles_sf),
      add = TRUE,
      border = "black",
      lwd = 0.7
    )

    hist(
      tile_summary$n_valid_pixels,
      breaks = 30,
      main = "Valid pixels per tile",
      xlab = "Valid pixels"
    )

    abline(
      v = tile_summary$target_valid_pixels[1],
      lwd = 2,
      lty = 2
    )

    plot(
      tiles_sf["n_valid_pixels"],
      main = "Tile load by valid pixels"
    )

    plot(
      tiles_sf["valid_pixel_error_pct"],
      main = "Valid pixel error by tile"
    )

    invisible(NULL)
  }

  function(
      base_raster,
      n_tile_rows = 10,
      n_tile_cols = 10,
      valid_values = NULL,
      output_dir = NULL,
      output_prefix = "balanced_raster_tiles",
      save_outputs = !is.null(output_dir),
      overwrite = TRUE,
      diagnostic_plots = FALSE,
      verbose = TRUE
  ) {

    base_raster <- balanced_prepare_raster(
      base_raster = base_raster
    )

    balanced_message(
      "Reading raster values and creating valid pixel mask.",
      verbose = verbose
    )

    valid_matrix <- balanced_make_valid_matrix(
      base_raster = base_raster,
      valid_values = valid_values
    )

    n_rows <- nrow(valid_matrix)
    n_cols <- ncol(valid_matrix)

    total_valid_pixels <- sum(
      valid_matrix,
      na.rm = TRUE
    )

    total_tiles <- n_tile_rows * n_tile_cols
    target_valid_pixels <- total_valid_pixels / total_tiles

    if (total_valid_pixels <= 0) {
      stop("The base raster has zero valid pixels.")
    }

    if (target_valid_pixels < 1) {
      stop("The requested number of tiles is too high for the number of valid pixels.")
    }

    balanced_message(
      "Creating balanced row ranges.",
      verbose = verbose
    )

    row_weights <- rowSums(
      valid_matrix,
      na.rm = TRUE
    )

    row_ranges <- balanced_make_ranges(
      weights = row_weights,
      n_groups = n_tile_rows,
      axis_name = "row"
    ) %>%
      dplyr::rename(
        tile_row = group_id,
        row_start = start_index,
        row_end = end_index,
        n_raster_rows = n_items,
        n_valid_pixels_row_range = weight,
        target_valid_pixels_row_range = target_weight,
        valid_pixel_error_row_range = weight_error,
        valid_pixel_error_pct_row_range = weight_error_pct
      )

    balanced_message(
      "Creating balanced column ranges inside each row range.",
      verbose = verbose
    )

    tile_list <- list()
    polygon_list <- list()
    column_range_list <- list()
    tile_id <- 1

    for (i in seq_len(nrow(row_ranges))) {

      row_start_i <- row_ranges$row_start[i]
      row_end_i <- row_ranges$row_end[i]
      tile_row_i <- row_ranges$tile_row[i]

      row_block_matrix <- valid_matrix[
        row_start_i:row_end_i,
        ,
        drop = FALSE
      ]

      col_weights <- colSums(
        row_block_matrix,
        na.rm = TRUE
      )

      col_ranges <- balanced_make_ranges(
        weights = col_weights,
        n_groups = n_tile_cols,
        axis_name = paste0("column in tile row ", tile_row_i)
      ) %>%
        dplyr::rename(
          tile_col = group_id,
          col_start = start_index,
          col_end = end_index,
          n_raster_cols = n_items,
          n_valid_pixels_col_range = weight,
          target_valid_pixels_col_range = target_weight,
          valid_pixel_error_col_range = weight_error,
          valid_pixel_error_pct_col_range = weight_error_pct
        ) %>%
        dplyr::mutate(
          tile_row = tile_row_i,
          row_start = row_start_i,
          row_end = row_end_i,
          .before = 1
        )

      column_range_list[[i]] <- col_ranges

      for (j in seq_len(nrow(col_ranges))) {

        col_start_j <- col_ranges$col_start[j]
        col_end_j <- col_ranges$col_end[j]
        tile_col_j <- col_ranges$tile_col[j]

        tile_matrix <- valid_matrix[
          row_start_i:row_end_i,
          col_start_j:col_end_j,
          drop = FALSE
        ]

        n_total_pixels_j <- length(tile_matrix)
        n_valid_pixels_j <- sum(
          tile_matrix,
          na.rm = TRUE
        )

        valid_fraction_j <- n_valid_pixels_j / n_total_pixels_j

        tile_list[[tile_id]] <- tibble::tibble(
          tile_id = tile_id,
          tile_row = tile_row_i,
          tile_col = tile_col_j,
          row_start = row_start_i,
          row_end = row_end_i,
          col_start = col_start_j,
          col_end = col_end_j,
          n_raster_rows = row_end_i - row_start_i + 1,
          n_raster_cols = col_end_j - col_start_j + 1,
          n_total_pixels = n_total_pixels_j,
          n_valid_pixels = n_valid_pixels_j,
          valid_fraction = valid_fraction_j,
          target_valid_pixels = target_valid_pixels,
          valid_pixel_error = n_valid_pixels_j - target_valid_pixels,
          valid_pixel_error_pct = 100 * (n_valid_pixels_j - target_valid_pixels) / target_valid_pixels
        )

        polygon_list[[tile_id]] <- balanced_make_tile_polygon(
          raster_base = base_raster,
          row_start = row_start_i,
          row_end = row_end_i,
          col_start = col_start_j,
          col_end = col_end_j
        )

        tile_id <- tile_id + 1
      }
    }

    tile_summary <- tile_list %>%
      dplyr::bind_rows()

    column_ranges <- column_range_list %>%
      dplyr::bind_rows()

    tiles_sf <- sf::st_sf(
      tile_summary,
      geometry = sf::st_sfc(
        polygon_list,
        crs = terra::crs(base_raster)
      )
    )

    global_summary <- balanced_summarise_global(
      tile_summary = tile_summary
    )

    if (any(tile_summary$n_valid_pixels == 0)) {
      warning("At least one tile has zero valid pixels.")
    }

    if (isTRUE(save_outputs) && is.null(output_dir)) {
      stop("output_dir must be provided when save_outputs = TRUE.")
    }

    if (isTRUE(save_outputs)) {
      balanced_save_outputs(
        tiles_sf = tiles_sf,
        tile_summary = tile_summary,
        global_summary = global_summary,
        row_ranges = row_ranges,
        column_ranges = column_ranges,
        output_dir = output_dir,
        output_prefix = output_prefix,
        overwrite = overwrite,
        verbose = verbose
      )
    }

    if (isTRUE(diagnostic_plots)) {
      balanced_plot_diagnostics(
        base_raster = base_raster,
        tiles_sf = tiles_sf,
        tile_summary = tile_summary
      )
    }

    list(
      tiles = tiles_sf,
      tile_summary = tile_summary,
      global_summary = global_summary,
      row_ranges = row_ranges,
      column_ranges = column_ranges
    )
  }
})

make_balanced_raster_tiles <- balanced_raster_tiles
