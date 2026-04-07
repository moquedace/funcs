process_landsat_indices <- function(
    blue_file,
    green_file,
    red_file,
    nir_file,
    swir1_file,
    swir2_file,
    thermal_file = NULL,
    output_dir,
    prefix = "landsat",
    indices = "all",
    save_thermal = TRUE,
    thermal_name = "surface_temperature_celsius",
    thermal_to_celsius = TRUE,
    savi_l = 0.5,
    apply_reflectance_scale = FALSE,
    reflectance_mult = 0.0000275,
    reflectance_add = -0.2,
    apply_thermal_scale = FALSE,
    thermal_mult = 0.00341802,
    thermal_add = 149.0,
    parallel = FALSE,
    workers = max(1, future::availableCores() - 1),
    plot_before_save = TRUE,
    overwrite = TRUE,
    compression = "LZW",
    poll_interval = 1
) {
  
  
  source(
    "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
  )
  
  pkg <- c(
    "terra", "future", "future.apply", "dplyr"
  )
  
  install_load_pkg(pkg)
  
  options(scipen = 999)
  
  
  
  format_duration_value <- function(start_time, end_time = Sys.time()) {
    duration <- difftime(end_time, start_time, units = "auto")
    
    list(
      value = as.numeric(duration),
      unit = as.character(units(duration))
    )
  }
  
  cat_time <- function(...) {
    cat(
      sprintf(
        "[%s] %s\n",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        paste0(..., collapse = "")
      )
    )
  }
  
  safe_div_terra <- function(num, den) {
    terra::ifel(is.na(den) | den == 0, NA, num / den)
  }
  
  get_available_indices <- function() {
    c(
      "ndvi", "savi", "msavi", "bsi", "mbi", "ndmi", "nbr2",
      "brightness_index", "clay_index", "nci", "ri", "fei", "coi", "si"
    )
  }
  
  resolve_indices <- function(indices) {
    available_indices <- get_available_indices()
    
    if (length(indices) == 1 && identical(indices, "all")) {
      return(available_indices)
    }
    
    if (length(indices) == 0) {
      stop("The 'indices' argument is empty.")
    }
    
    invalid_indices <- setdiff(indices, available_indices)
    
    if (length(invalid_indices) > 0) {
      stop(
        sprintf(
          "Invalid indices found in 'indices': %s",
          paste(invalid_indices, collapse = ", ")
        )
      )
    }
    
    unique(indices)
  }
  
  check_file_exists <- function(file_path, band_name) {
    if (length(file_path) == 0) {
      stop(sprintf("No file found for '%s'.", band_name))
    }
    
    if (length(file_path) > 1) {
      stop(
        sprintf(
          "More than one file found for '%s':\n%s",
          band_name,
          paste(file_path, collapse = "\n")
        )
      )
    }
    
    if (is.null(file_path) || is.na(file_path) || !nzchar(file_path)) {
      stop(sprintf("Argument '%s' is missing or empty.", band_name))
    }
    
    if (!file.exists(file_path)) {
      stop(sprintf("File not found for '%s': %s", band_name, file_path))
    }
  }
  
  find_single_band_file <- function(input_dir, band_regex, scene_regex) {
    tif_files <- list.files(
      path = input_dir,
      pattern = "\\.tif$",
      full.names = TRUE
    )
    
    tif_names <- basename(tif_files)
    
    keep <- grepl(band_regex, tif_names, ignore.case = TRUE) &
      grepl(scene_regex, tif_names, ignore.case = TRUE)
    
    files_found <- tif_files[keep]
    
    if (length(files_found) == 0) {
      stop(
        sprintf(
          "No file found with band_regex = '%s' and scene_regex = '%s'.",
          band_regex,
          scene_regex
        )
      )
    }
    
    if (length(files_found) > 1) {
      stop(
        sprintf(
          "More than one file found with band_regex = '%s' and scene_regex = '%s':\n%s",
          band_regex,
          scene_regex,
          paste(files_found, collapse = "\n")
        )
      )
    }
    
    files_found
  }
  
  load_landsat_bands <- function(
    blue_file,
    green_file,
    red_file,
    nir_file,
    swir1_file,
    swir2_file,
    thermal_file = NULL,
    include_thermal = FALSE,
    apply_reflectance_scale = FALSE,
    reflectance_mult = 0.0000275,
    reflectance_add = -0.2,
    apply_thermal_scale = FALSE,
    thermal_mult = 0.00341802,
    thermal_add = 149.0,
    thermal_to_celsius = TRUE,
    verbose = TRUE
  ) {
    t_load <- Sys.time()
    
    if (verbose) {
      cat_time("Checking input files")
    }
    
    check_file_exists(blue_file, "blue_file")
    check_file_exists(green_file, "green_file")
    check_file_exists(red_file, "red_file")
    check_file_exists(nir_file, "nir_file")
    check_file_exists(swir1_file, "swir1_file")
    check_file_exists(swir2_file, "swir2_file")
    
    if (include_thermal) {
      check_file_exists(thermal_file, "thermal_file")
    }
    
    if (verbose) {
      cat_time("Loading raster bands")
    }
    
    blue <- terra::rast(blue_file)
    green <- terra::rast(green_file)
    red <- terra::rast(red_file)
    nir <- terra::rast(nir_file)
    swir1 <- terra::rast(swir1_file)
    swir2 <- terra::rast(swir2_file)
    
    band_list <- list(
      blue = blue,
      green = green,
      red = red,
      nir = nir,
      swir1 = swir1,
      swir2 = swir2
    )
    
    if (include_thermal) {
      thermal <- terra::rast(thermal_file)
      band_list[["thermal"]] <- thermal
    }
    
    if (verbose) {
      cat_time("Checking band geometry")
    }
    
    layer_check <- vapply(
      band_list,
      function(x) terra::nlyr(x) == 1,
      logical(1)
    )
    
    if (!all(layer_check)) {
      stop("All input files must contain a single raster layer.")
    }
    
    base_raster <- band_list[[1]]
    
    geom_check <- vapply(
      band_list,
      function(x) terra::compareGeom(base_raster, x, stopOnError = FALSE),
      logical(1)
    )
    
    if (!all(geom_check)) {
      stop("All bands must have identical extent, resolution, CRS, and origin.")
    }
    
    if (apply_reflectance_scale) {
      if (verbose) {
        cat_time("Applying reflectance scale factor")
      }
      
      band_list[c("blue", "green", "red", "nir", "swir1", "swir2")] <- lapply(
        band_list[c("blue", "green", "red", "nir", "swir1", "swir2")],
        function(x) x * reflectance_mult + reflectance_add
      )
    }
    
    if (include_thermal && apply_thermal_scale) {
      if (verbose) {
        cat_time("Applying thermal scale factor")
      }
      
      band_list[["thermal"]] <- band_list[["thermal"]] * thermal_mult + thermal_add
    }
    
    if (include_thermal && thermal_to_celsius) {
      if (verbose) {
        cat_time("Converting thermal band to Celsius")
      }
      
      band_list[["thermal"]] <- band_list[["thermal"]] - 273.15
    }
    
    if (verbose) {
      duration_load <- format_duration_value(t_load)
      
      cat(
        sprintf(
          "[%s] bands loaded | duration: %.2f %s\n",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          duration_load$value,
          duration_load$unit
        )
      )
    }
    
    band_list
  }
  
  calculate_single_index <- function(index_name, bands, savi_l = 0.5) {
    blue <- bands[["blue"]]
    green <- bands[["green"]]
    red <- bands[["red"]]
    nir <- bands[["nir"]]
    swir1 <- bands[["swir1"]]
    swir2 <- bands[["swir2"]]
    
    index_raster <- switch(
      index_name,
      
      ndvi = safe_div_terra(nir - red, nir + red),
      
      savi = safe_div_terra((nir - red) * (1 + savi_l), nir + red + savi_l),
      
      msavi = {
        msavi_term <- (2 * nir + 1)^2 - 8 * (nir - red)
        msavi_term <- terra::ifel(msavi_term < 0, NA, msavi_term)
        (2 * nir + 1 - sqrt(msavi_term)) / 2
      },
      
      bsi = safe_div_terra(
        (swir1 + red) - (nir + blue),
        (swir1 + red) + (nir + blue)
      ),
      
      mbi = safe_div_terra(swir1 - swir2 - nir, swir1 + swir2 + nir) + 0.5,
      
      ndmi = safe_div_terra(nir - swir1, nir + swir1),
      
      nbr2 = safe_div_terra(swir1 - swir2, swir1 + swir2),
      
      brightness_index = sqrt(blue^2 + green^2 + red^2),
      
      clay_index = safe_div_terra(swir1, swir2),
      
      nci = safe_div_terra(swir1 - swir2, swir1 + swir2),
      
      ri = safe_div_terra(red^2, blue * (green^3)),
      
      fei = safe_div_terra(red, swir1),
      
      coi = safe_div_terra(red - green, red + green),
      
      si = safe_div_terra(red - blue, red + blue),
      
      stop(sprintf("Index '%s' is not implemented.", index_name))
    )
    
    index_raster
  }
  
  plot_single_raster <- function(raster_obj, main_title) {
    terra::plot(raster_obj, main = main_title)
  }
  
  write_single_raster <- function(
    raster_obj,
    output_file,
    overwrite = TRUE,
    compression = "LZW"
  ) {
    terra::writeRaster(
      x = raster_obj,
      filename = output_file,
      overwrite = overwrite,
      gdal = paste0("COMPRESS=", compression)
    )
  }
  
  
  t_total <- Sys.time()
  indices_to_run <- resolve_indices(indices)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat_time("Created output directory: ", output_dir)
  }
  
  cat_time("Starting Landsat indices processing")
  cat_time("Prefix: ", prefix)
  cat_time("Output directory: ", output_dir)
  cat_time("Indices requested: ", paste(indices_to_run, collapse = ", "))
  cat_time("Save thermal: ", save_thermal)
  cat_time("Parallel mode: ", parallel)
  
  output_files <- list()
  
  if (!parallel) {
    cat_time("Running in sequential mode")
    
    bands <- load_landsat_bands(
      blue_file = blue_file,
      green_file = green_file,
      red_file = red_file,
      nir_file = nir_file,
      swir1_file = swir1_file,
      swir2_file = swir2_file,
      thermal_file = thermal_file,
      include_thermal = save_thermal,
      apply_reflectance_scale = apply_reflectance_scale,
      reflectance_mult = reflectance_mult,
      reflectance_add = reflectance_add,
      apply_thermal_scale = apply_thermal_scale,
      thermal_mult = thermal_mult,
      thermal_add = thermal_add,
      thermal_to_celsius = thermal_to_celsius,
      verbose = TRUE
    )
    
    for (i in seq_along(indices_to_run)) {
      index_name <- indices_to_run[i]
      t_index <- Sys.time()
      
      cat_time("[", i, "/", length(indices_to_run), "] processing: ", index_name)
      
      index_raster <- calculate_single_index(
        index_name = index_name,
        bands = bands,
        savi_l = savi_l
      )
      
      layer_name <- paste0(prefix, "_", index_name)
      output_file <- file.path(output_dir, paste0(layer_name, ".tif"))
      
      names(index_raster) <- layer_name
      
      if (plot_before_save) {
        cat_time("[", i, "/", length(indices_to_run), "] plotting: ", layer_name)
        
        plot_single_raster(
          raster_obj = index_raster,
          main_title = layer_name
        )
      }
      
      cat_time("[", i, "/", length(indices_to_run), "] writing: ", output_file)
      
      write_single_raster(
        raster_obj = index_raster,
        output_file = output_file,
        overwrite = overwrite,
        compression = compression
      )
      
      output_files[[index_name]] <- output_file
      
      duration_index <- format_duration_value(t_index)
      duration_total <- format_duration_value(t_total)
      
      cat(
        sprintf(
          "[%s] [%s/%s] | finished: %s | duration: %.2f %s | total: %.2f %s\n",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          i,
          length(indices_to_run),
          index_name,
          duration_index$value,
          duration_index$unit,
          duration_total$value,
          duration_total$unit
        )
      )
    }
    
    if (save_thermal) {
      t_thermal <- Sys.time()
      
      cat_time("processing: thermal covariate")
      
      thermal_raster <- bands[["thermal"]]
      thermal_layer_name <- paste0(prefix, "_", thermal_name)
      thermal_output_file <- file.path(
        output_dir,
        paste0(thermal_layer_name, ".tif")
      )
      
      names(thermal_raster) <- thermal_layer_name
      
      if (plot_before_save) {
        cat_time("plotting: ", thermal_layer_name)
        
        plot_single_raster(
          raster_obj = thermal_raster,
          main_title = thermal_layer_name
        )
      }
      
      cat_time("writing: ", thermal_output_file)
      
      write_single_raster(
        raster_obj = thermal_raster,
        output_file = thermal_output_file,
        overwrite = overwrite,
        compression = compression
      )
      
      output_files[[thermal_name]] <- thermal_output_file
      
      duration_thermal <- format_duration_value(t_thermal)
      duration_total <- format_duration_value(t_total)
      
      cat(
        sprintf(
          "[%s] finished: %s | duration: %.2f %s | total: %.2f %s\n",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          thermal_name,
          duration_thermal$value,
          duration_thermal$unit,
          duration_total$value,
          duration_total$unit
        )
      )
    }
  }
  
  if (parallel) {
    cat_time("Running in parallel mode with ", workers, " workers")
    
    if (plot_before_save) {
      cat_time("Plotting is skipped in parallel mode")
    }
    
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    
    future::plan(future::multisession, workers = workers)
    
    future_list <- lapply(
      seq_along(indices_to_run),
      function(i) {
        index_name <- indices_to_run[i]
        
        future::future(
          {
            t_index <- Sys.time()
            
            bands_local <- load_landsat_bands(
              blue_file = blue_file,
              green_file = green_file,
              red_file = red_file,
              nir_file = nir_file,
              swir1_file = swir1_file,
              swir2_file = swir2_file,
              thermal_file = NULL,
              include_thermal = FALSE,
              apply_reflectance_scale = apply_reflectance_scale,
              reflectance_mult = reflectance_mult,
              reflectance_add = reflectance_add,
              apply_thermal_scale = FALSE,
              thermal_mult = thermal_mult,
              thermal_add = thermal_add,
              thermal_to_celsius = FALSE,
              verbose = FALSE
            )
            
            index_raster <- calculate_single_index(
              index_name = index_name,
              bands = bands_local,
              savi_l = savi_l
            )
            
            layer_name <- paste0(prefix, "_", index_name)
            output_file <- file.path(output_dir, paste0(layer_name, ".tif"))
            
            names(index_raster) <- layer_name
            
            write_single_raster(
              raster_obj = index_raster,
              output_file = output_file,
              overwrite = overwrite,
              compression = compression
            )
            
            duration_index <- format_duration_value(t_index)
            
            list(
              index_name = index_name,
              output_file = output_file,
              duration_value = duration_index$value,
              duration_unit = duration_index$unit
            )
          },
          seed = TRUE
        )
      }
    )
    
    completed <- rep(FALSE, length(future_list))
    
    while (!all(completed)) {
      resolved_now <- vapply(
        future_list,
        future::resolved,
        logical(1)
      )
      
      new_done <- which(resolved_now & !completed)
      
      if (length(new_done) > 0) {
        for (i in new_done) {
          res_i <- future::value(future_list[[i]])
          completed[i] <- TRUE
          output_files[[res_i$index_name]] <- res_i$output_file
          
          duration_total <- format_duration_value(t_total)
          
          cat(
            sprintf(
              "[%s] [%s/%s] | finished: %s | duration: %.2f %s | total: %.2f %s\n",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              sum(completed),
              length(future_list),
              res_i$index_name,
              res_i$duration_value,
              res_i$duration_unit,
              duration_total$value,
              duration_total$unit
            )
          )
        }
      }
      
      Sys.sleep(poll_interval)
    }
    
    if (save_thermal) {
      t_thermal <- Sys.time()
      
      cat_time("processing: thermal covariate")
      
      bands_thermal <- load_landsat_bands(
        blue_file = blue_file,
        green_file = green_file,
        red_file = red_file,
        nir_file = nir_file,
        swir1_file = swir1_file,
        swir2_file = swir2_file,
        thermal_file = thermal_file,
        include_thermal = TRUE,
        apply_reflectance_scale = apply_reflectance_scale,
        reflectance_mult = reflectance_mult,
        reflectance_add = reflectance_add,
        apply_thermal_scale = apply_thermal_scale,
        thermal_mult = thermal_mult,
        thermal_add = thermal_add,
        thermal_to_celsius = thermal_to_celsius,
        verbose = TRUE
      )
      
      thermal_raster <- bands_thermal[["thermal"]]
      thermal_layer_name <- paste0(prefix, "_", thermal_name)
      thermal_output_file <- file.path(
        output_dir,
        paste0(thermal_layer_name, ".tif")
      )
      
      names(thermal_raster) <- thermal_layer_name
      
      cat_time("writing: ", thermal_output_file)
      
      write_single_raster(
        raster_obj = thermal_raster,
        output_file = thermal_output_file,
        overwrite = overwrite,
        compression = compression
      )
      
      output_files[[thermal_name]] <- thermal_output_file
      
      duration_thermal <- format_duration_value(t_thermal)
      duration_total <- format_duration_value(t_total)
      
      cat(
        sprintf(
          "[%s] finished: %s | duration: %.2f %s | total: %.2f %s\n",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          thermal_name,
          duration_thermal$value,
          duration_thermal$unit,
          duration_total$value,
          duration_total$unit
        )
      )
    }
  }
  
  output_files <- unlist(output_files)
  duration_total <- format_duration_value(t_total)
  
  cat(
    sprintf(
      "[%s] all requested covariates finished | total: %.2f %s\n",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      duration_total$value,
      duration_total$unit
    )
  )
  
  output_files
}
