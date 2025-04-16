rst_class_name_max <- function(
    rst, custom_crs = NULL, plot_map = TRUE
    ) {
  start_time <- Sys.time()
  message("Starting function rst_class_name_max...")
  
  source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
  
  pkg <- c(
    "dplyr", "terra", "stringr"
  )
  
  message("Loading required packages...")
  
  install_load_pkg(pkg)
 
  message("Checking raster format...")
  if (inherits(rst, c("RasterStack", "RasterLayer"))) {
    rst <- rast(rst)
    message("Raster converted to SpatRaster.")
  } else if (!inherits(rst, "SpatRaster")) {
    stop("Error: the input must be of class RasterStack, RasterLayer, or SpatRaster.")
  } else {
    message("Raster is already in SpatRaster format.")
  }
  
  message("Setting coordinate reference system (CRS)...")
  crs_str <- if (is.null(custom_crs)) {
    crs_info <- crs(rst, describe = TRUE)
    if (!is.null(crs_info$authority) && !is.null(crs_info$code)) {
      paste0(crs_info$authority, ":", crs_info$code)
    } else if (!is.null(crs_info$proj4)) {
      crs_info$proj4
    } else {
      warning("Could not retrieve raster CRS. Using default WGS84.")
      "EPSG:4326"
    }
  } else {
    custom_crs
  }
  crs(rst) <- crs_str
  message("CRS set to: ", crs_str)
  
  message("Retrieving layer names...")
  soils_name <- sort(names(rst))
  df_code_clas <- data.frame(code_classe_dom = seq_along(soils_name),
                             classe_dom = soils_name)
  
  gc()
  
  message("Converting raster to DataFrame and processing data...")
  df <- as.data.frame(rst, xy = TRUE, na.rm = TRUE)
  classes_name <- df %>%
    select(-x, -y) %>%
    mutate(classe_dom = names(.)[apply(abs(.), 1, which.max)]) %>%  
    select(classe_dom)
  
  gc()
  
  message("Creating raster with dominant class...")
  r_dom <- df %>%
    select(x, y) %>%
    cbind(classes_name) %>%
    left_join(df_code_clas, by = "classe_dom") %>%
    select(x, y, code_classe_dom) %>%
    rast(type = "xyz", crs = crs_str)
  
  gc()
  
  levels(r_dom) <- list(df_code_clas)
  
  if (plot_map) {
    message("Plotting dominant class raster...")
    plot(r_dom, main = "Dominant Class from Raster")
  }
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  message("Function executed successfully in ", round(elapsed_time, 2), " ",
          units(elapsed_time), "...")
  
  return(r_dom)
}
