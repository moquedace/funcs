standardize_crop_mask_raster <- function(
    rst,
    vct,
    resolution = 500,
    method = "cubicspline",
    threads = TRUE,
    output_dir,
    crs = c("EPSG:4326", "ESRI:54052"),
    round = NULL,
    resample = FALSE,
    integer = FALSE,
    overwrite = FALSE
) {
  
  # Function to install and load required packages
  source(
    "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/utils/install_load_pkg.R"
  )
  
  pkg <- c(
    "terra", "dplyr", "janitor"
  )
  
  invisible(
    install_load_pkg(pkg)
  )
  
  validate_inputs <- function(
      rst, vct, output_dir, crs, resolution, round,
      integer, resample, overwrite, threads
  ) {
    
    if (!file.exists(rst)) {
      stop("The raster file does not exist: ", rst)
    }
    
    if (!file.exists(vct)) {
      stop("The vector file does not exist: ", vct)
    }
    
    if (!is.character(output_dir) || length(output_dir) != 1 || is.na(output_dir)) {
      stop("The 'output_dir' parameter must be a valid character value.")
    }
    
    if (!is.character(crs) || length(crs) == 0) {
      stop("The 'crs' parameter must be a non-empty character vector.")
    }
    
    if (!is.null(resolution)) {
      if (
        !is.numeric(resolution) ||
        length(resolution) != 1 ||
        is.na(resolution) ||
        resolution <= 0
      ) {
        stop("The 'resolution' parameter must be a positive numeric value.")
      }
    }
    
    if (!is.null(round)) {
      if (
        !is.numeric(round) ||
        length(round) != 1 ||
        is.na(round) ||
        round < 0
      ) {
        stop("The 'round' parameter must be a non-negative numeric value.")
      }
    }
    
    if (!is.logical(integer) || length(integer) != 1 || is.na(integer)) {
      stop("The 'integer' parameter must be TRUE or FALSE.")
    }
    
    if (!is.logical(resample) || length(resample) != 1 || is.na(resample)) {
      stop("The 'resample' parameter must be TRUE or FALSE.")
    }
    
    if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
      stop("The 'overwrite' parameter must be TRUE or FALSE.")
    }
    
    if (!is.logical(threads) || length(threads) != 1 || is.na(threads)) {
      stop("The 'threads' parameter must be TRUE or FALSE.")
    }
  }
  
  create_crs_template <- function(crs_target) {
    terra::rast(
      nrows = 1,
      ncols = 1,
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      crs = crs_target
    )
  }
  
  compare_crs <- function(r, crs_template) {
    same_crs <- tryCatch(
      terra::same.crs(r, crs_template),
      error = function(e) FALSE
    )
    
    isTRUE(same_crs)
  }
  
  compare_resolution <- function(r, target_resolution) {
    
    if (is.null(target_resolution)) {
      return(TRUE)
    }
    
    current_resolution <- terra::res(r)
    
    tolerance <- max(abs(target_resolution), 1) * 1e-6
    
    all(abs(current_resolution - target_resolution) <= tolerance)
  }
  
  validate_inputs(
    rst = rst,
    vct = vct,
    output_dir = output_dir,
    crs = crs,
    resolution = resolution,
    round = round,
    integer = integer,
    resample = resample,
    overwrite = overwrite,
    threads = threads
  )
  
  r <- terra::rast(rst)
  v <- terra::vect(vct)
  
  raster_name <- names(r)[1]
  
  if (is.na(raster_name) || raster_name == "") {
    raster_name <- tools::file_path_sans_ext(basename(rst))
  }
  
  if (is.na(terra::crs(r)) || terra::crs(r) == "") {
    stop("The raster has no defined CRS: ", rst)
  }
  
  if (is.na(terra::crs(v)) || terra::crs(v) == "") {
    stop("The vector has no defined CRS: ", vct)
  }
  
  processed_count <- 0L
  skipped_count <- 0L
  
  for (crs_target in crs) {
    
    message("\nProcessing CRS: ", crs_target)
    
    crs_template <- create_crs_template(crs_target)
    
    is_geographic <- terra::is.lonlat(crs_template)
    
    if (is_geographic) {
      
      v_proj <- terra::project(v, crs_target)
      centroid <- terra::centroids(v_proj, inside = TRUE)
      lat <- terra::crds(centroid)[, "y"][1]
      
      res_adjusted <- resolution / 111319
      
      message(
        sprintf(
          "Resolution conversion: %s m ~= %.6f degrees based on latitude %.1f",
          resolution,
          res_adjusted,
          lat
        )
      )
      
    } else {
      
      res_adjusted <- resolution
    }
    
    output_subdir <- file.path(
      output_dir,
      janitor::make_clean_names(crs_target),
      paste0("resolution_", resolution, "m")
    )
    
    dir.create(
      output_subdir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    output_file <- file.path(
      output_subdir,
      paste0(raster_name, ".tif")
    )
    
    if (file.exists(output_file) && !overwrite) {
      message("Output already exists and overwrite = FALSE. Skipped: ", output_file)
      skipped_count <- skipped_count + 1L
      next
    }
    
    is_same_crs <- compare_crs(
      r = r,
      crs_template = crs_template
    )
    
    is_same_resolution <- compare_resolution(
      r = r,
      target_resolution = res_adjusted
    )
    
    needs_projection <- !is_same_crs
    needs_resampling <- isTRUE(resample) || !is_same_resolution
    
    if (needs_projection || needs_resampling) {
      
      r_proj <- terra::project(
        r,
        y = crs_target,
        method = method,
        threads = threads,
        res = res_adjusted
      )
      
      if (needs_projection) {
        message("Raster reprojected to: ", crs_target)
      }
      
      if (needs_resampling) {
        message(
          "Raster resampled to resolution: ",
          if (is_geographic) {
            paste0(base::round(res_adjusted, 6), " degrees")
          } else {
            paste0(resolution, " m")
          }
        )
      }
      
    } else {
      
      r_proj <- r
      
      message(
        "CRS and resolution already match the target. Using original raster."
      )
    }
    
    v_proj <- terra::project(v, crs_target)
    
    r_crop <- terra::crop(r_proj, v_proj)
    r_mask <- terra::mask(r_crop, v_proj)
    
    names(r_mask) <- raster_name
    
    if (isTRUE(integer)) {
      r_mask <- terra::as.int(r_mask)
    } else if (!is.null(round)) {
      r_mask <- terra::round(r_mask, digits = round)
    }
    
    terra::writeRaster(
      r_mask,
      filename = output_file,
      overwrite = overwrite,
      gdal = "COMPRESS=LZW"
    )
    
    message("Raster saved at: ", output_file)
    
    processed_count <- processed_count + 1L
  }
  
  message(
    "\nProcessing completed! Processed: ",
    processed_count,
    " | skipped: ",
    skipped_count
  )
}
