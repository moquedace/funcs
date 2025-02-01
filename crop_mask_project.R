crop_mask_project <- function(rst,
                              vct,
                              resolution = 500,
                              method = "cubicspline",
                              threads = TRUE,
                              output_dir,
                              crs = c("EPSG:4326", "ESRI:54052"),
                              round = NULL,
                              resample = FALSE) {
  
  # Function to install and load required packages
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  # Required packages
  required_packages <- c("terra", "dplyr", "janitor")
  invisible(install_and_load_packages(required_packages))
  
  # Input validation function
  validate_inputs <- function(rst, vct, output_dir, crs, resolution, round) {
    if (!file.exists(rst)) {
      stop("The raster file does not exist: ", rst)
    }
    if (!file.exists(vct)) {
      stop("The vector file (vct) does not exist: ", vct)
    }
    if (!is.character(crs) || length(crs) == 0) {
      stop("The 'crs' parameter must be a non-empty character vector.")
    }
    if (!is.null(resolution)) {
      if (!is.numeric(resolution) || length(resolution) != 1 || resolution <= 0) {
        stop("The 'resolution' parameter must be a positive numeric value.")
      }
    }
    if (!is.null(round)) {
      if (!is.numeric(round) || length(round) != 1 || round < 0) {
        stop("The 'round' parameter must be a non-negative numeric value.")
      }
    }
  }
  
  # Validate inputs
  validate_inputs(rst, vct, output_dir, crs, resolution, round)
  
  # Load raster and vector
  r <- terra::rast(rst)
  v <- terra::vect(vct)
  
  # Process each CRS
  for (crs_target in crs) {
    message("\nProcessing CRS: ", crs_target)
    
    # Check if CRS is geographic (degrees)
    is_geographic <- terra::is.lonlat(crs_target)
    
    # Convert resolution to degrees if needed
    if (is_geographic) {
      # Get centroid latitude of the vector
      v_proj <- terra::project(v, crs_target)
      centroid <- terra::centroids(v_proj, inside = TRUE)
      lat <- terra::crds(centroid)[, "y"][1]
      
      # Use fixed reference: 1° latitude ≈ 111,319 m
      res_adjusted <- resolution / 111319
      message(
        sprintf(
          "Resolution conversion: %d m ≈ %.6f° (based on latitude ~%.1f°N)",
          resolution, res_adjusted, lat
        )
      )
    } else {
      res_adjusted <- resolution
    }
    
    # Check if reprojection/resampling is needed
    current_crs <- paste0(terra::crs(r, proj = FALSE, describe = TRUE,
                                     parse = FALSE)$authority, ":",
                          terra::crs(r, proj = FALSE, describe = TRUE,
                                     parse = FALSE)$code)
    
    if (current_crs != crs_target || resample) {
      r_proj <- terra::project(
        r, 
        y = crs_target,
        method = method,
        threads = threads,
        res = res_adjusted
      )
      message(
        if (current_crs != crs_target) "Reprojected" else "Resampled",
        " to resolution: ", 
        if (is_geographic) paste0(round(res_adjusted, 6), "°") else paste0(resolution, "m")
      )
    } else {
      r_proj <- r
      message("CRS unchanged and resample = FALSE. Using original raster.")
    }
    
    # Crop and mask
    v_proj <- terra::project(v, crs_target)
    r_crop <- terra::crop(r_proj, v_proj)
    r_mask <- terra::mask(r_crop, v_proj)
    
    # Optional rounding
    if (!is.null(round)) {
      r_mask <- terra::round(r_mask, digits = round)
    }
    
    # Save raster
    output_subdir <- file.path(
      output_dir,
      janitor::make_clean_names(crs_target),
      paste0("resolution_", resolution, "m")
    )
    dir.create(output_subdir, recursive = TRUE, showWarnings = FALSE)
    
    output_file <- file.path(output_subdir, paste0(names(r_mask), ".tif"))
    terra::writeRaster(r_mask, filename = output_file, overwrite = TRUE, gdal = "COMPRESS=LZW")
    message("Raster saved at: ", output_file)
  }
  
  message("\nProcessing completed!")
}
