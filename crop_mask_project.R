crop_mask_project <- function(rst,
                              vct,
                              method = "cubicspline",
                              threads = TRUE,
                              output_dir,
                              crs = c("EPSG:4326", "ESRI:54052")) {
  
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
  install_and_load_packages(required_packages)
  
  # Function to validate inputs
  validate_inputs <- function(rst, vct, output_dir, crs) {
    if (!file.exists(rst)) {
      stop("The raster file does not exist: ", rst)
    }
    if (!inherits(vct, "SpatVector")) {
      stop("The 'vct' object must be of type 'SpatVector'")
    }
    if (!is.character(crs) || length(crs) == 0) {
      stop("The 'crs' parameter must be a non-empty character vector")
    }
  }
  
  validate_inputs(rst, vct, output_dir, crs)
  
  # Check and create output directory if necessary
  if (!file.exists(output_dir)) {
    message("Output directory does not exist. Creating: ", output_dir)
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load the raster
  message("Loading the raster...")
  r <- tryCatch({
    terra::rast(rst)
  }, error = function(e) {
    stop("Error loading the raster: ", e$message)
  })
  
  # Crop and mask the raster
  message("Starting crop and mask of the raster...")
  r_crop <- tryCatch({
    r %>% terra::crop(vct, mask = TRUE, overwrite = TRUE)
  }, error = function(e) {
    stop("Error cropping and masking the raster: ", e$message)
  })
  
  # CRS processing
  for (i in seq_along(crs)) {
    message("Processing CRS: ", crs[i])
    
    current_crs <- paste0(terra::crs(r_crop, proj = FALSE, describe = TRUE, parse = FALSE)$authority, ":",
                          terra::crs(r_crop, proj = FALSE, describe = TRUE, parse = FALSE)$code)
    
    if (current_crs != crs[i]) {
      message("Reprojecting raster to ", crs[i])
      r_crop_mask_project <- tryCatch({
        r_crop %>% terra::project(y = crs[i], method = method, threads = threads)
      }, error = function(e) {
        stop("Error reprojecting the raster: ", e$message)
      })
    } else {
      r_crop_mask_project <- r_crop
    }
    
    # Create output subdirectory for each CRS if necessary
    outdir_run <- file.path(output_dir, janitor::make_clean_names(crs[i]))
    if (!file.exists(outdir_run)) {
      message("Creating subdirectory for CRS: ", outdir_run)
      dir.create(outdir_run, recursive = TRUE)
    }
    
    # Save the reprojected raster
    outfile <- file.path(outdir_run, paste0(names(r_crop_mask_project), ".tif"))
    tryCatch({
      writeRaster(r_crop_mask_project, overwrite = TRUE, filename = outfile, gdal = "COMPRESS=LZW")
      message("Raster saved to: ", outfile)
    }, error = function(e) {
      stop("Error saving the raster: ", e$message)
    })
  }
  
  message("Processing complete.")
}
