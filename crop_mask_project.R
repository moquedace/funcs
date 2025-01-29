crop_mask_project <- function(rst,
                              vct,
                              resolution = NULL,
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
  invisible(install_and_load_packages(required_packages))
  
  # Function to validate inputs
  validate_inputs <- function(rst, vct, output_dir, crs, resolution) {
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
  }
  
  # Validate inputs
  validate_inputs(rst, vct, output_dir, crs, resolution)
  
  # Check and create output directory if necessary
  if (!file.exists(output_dir)) {
    message("The output directory does not exist. Creating: ", output_dir)
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load the raster file
  message("Loading the raster file...")
  r <- tryCatch({
    terra::rast(rst)
  }, error = function(e) {
    stop("Error loading the raster: ", e$message)
  })
  
  # Load the vector file
  message("Loading the vector file...")
  v <- tryCatch({
    terra::vect(vct)
  }, error = function(e) {
    stop("Error loading the vector file: ", e$message)
  })
  
  # CRS processing (first, reproject)
  for (i in seq_along(crs)) {
    message("Processing CRS: ", crs[i])
    
    current_crs <- paste0(terra::crs(r, proj = FALSE, describe = TRUE,
                                     parse = FALSE)$authority, ":",
                          terra::crs(r, proj = FALSE, describe = TRUE,
                                     parse = FALSE)$code)
    
    if (current_crs != crs[i]) {
      message("Reprojecting raster to ", crs[i])
      r_projected <- tryCatch({
        r %>% terra::project(y = crs[i], method = method, threads = threads,
                             res = resolution)
      }, error = function(e) {
        stop("Error reprojecting the raster: ", e$message)
      })
    } else {
      r_projected <- r
    }
    
    # Apply crop and mask to the reprojected raster
    message("Applying crop and mask...")
    
    v <- project(v, y = crs[i])
    
    r_crop_mask <- tryCatch({
      r_projected %>% terra::crop(v, overwrite = TRUE) %>% terra::mask(v)
    }, error = function(e) {
      stop("Error cropping and masking the raster: ", e$message)
    })
    
    # Create output subdirectory for each CRS, if necessary
    outdir_run <- file.path(output_dir, janitor::make_clean_names(crs[i]))
    if (!file.exists(outdir_run)) {
      message("Creating subdirectory for CRS: ", outdir_run)
      dir.create(outdir_run, recursive = TRUE)
    }
    
    # Save the reprojected and cropped raster
    outfile <- file.path(outdir_run, paste0(names(r_crop_mask), ".tif"))
    tryCatch({
      writeRaster(r_crop_mask, overwrite = TRUE, filename = outfile, gdal = "COMPRESS=LZW")
      message("Raster saved at: ", outfile)
    }, error = function(e) {
      stop("Error saving the raster: ", e$message)
    })
  }
  
  message("Processing completed.")
}
