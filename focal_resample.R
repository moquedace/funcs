focal_resample <- function(file,
                           r_base,
                           nfocal,
                           output_dir,
                           subs = -0,
                           apply_subst = TRUE,
                           method = "cubicspline",
                           resample_threads = TRUE) {
  # Function to check if a package is installed, if not, install it
  check_and_install <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Package", pkg, "is not installed. Installing now..."))
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
    message(paste("Package", pkg, "loaded successfully."))
  }
  
  # Check and install necessary packages
  check_and_install("terra")
  check_and_install("dplyr")
  
  message("Checking if input files exist...")
  # Check if input files exist
  if (!file.exists(file)) {
    stop("Input file not found.")
  }
  if (!file.exists(r_base)) {
    stop("Reference base file not found.")
  }
  message("Input files verified.")
  
  message("Loading rasters...")
  # Measuring the time to load rasters
  start_time <- Sys.time()
  rres <- rast(r_base)
  rrast <- rast(file)
  nm_r <- names(rrast)
  end_time <- Sys.time()
  message("Rasters loaded successfully in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  
  message("Applying focal function...")
  # Measuring the time to apply the focal function
  start_time <- Sys.time()
  r_focal <- focal(x = rrast, w = nfocal,
                   fun = "mean",
                   na.policy = "only",
                   na.rm = TRUE)
  end_time <- Sys.time()
  message("Focal function applied in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  
  if (apply_subst) {
    message("Replacing NA values...")
    # Measuring the time to replace NA values
    start_time <- Sys.time()
    r_focal <- subst(r_focal, from = NA, to = subs)
    end_time <- Sys.time()
    message("NA values replaced in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  }
  names(r_focal) <- nm_r
  
  message("Resampling the focal raster...")
  # Measuring the time to resample the raster
  start_time <- Sys.time()
  rst <- resample(r_focal, rres, method = method, threads = resample_threads)
  end_time <- Sys.time()
  message("Resampling completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  
  message("Checking output directory...")
  # Measuring the time to check/create the output directory
  start_time <- Sys.time()
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Output directory created.")
  } else {
    message("Output directory already exists.")
  }
  end_time <- Sys.time()
  message("Output directory check/creation completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  
  # Define the name of the output file
  output_file <- file.path(output_dir, paste0(names(rst), ".tif"))
  
  message("Saving the resampled raster...")
  # Measuring the time to save the raster
  start_time <- Sys.time()
  writeRaster(rst, overwrite = TRUE, filename = output_file, gdal = "COMPRESS=LZW")
  end_time <- Sys.time()
  message("Raster saved successfully in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  
  # Free up memory
  gc()
  
  message("Process completed.")
  
  # Return the output file path
  return(output_file)
}
