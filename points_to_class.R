points_to_class <- function(points, polygons, class_column, na = TRUE) {
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("sf", "dplyr")
  install_and_load_packages(required_packages)
  
  start_time <- Sys.time()
  
  message("Checking if 'points' and 'polygons' arguments are sf objects...")
  if (!inherits(points, "sf")) {
    stop("The 'points' argument is not an sf object.")
  }
  if (!inherits(polygons, "sf")) {
    stop("The 'polygons' argument is not an sf object.")
  }
  
  message("Checking if the 'class_column' argument is a string...")
  if (!is.character(class_column) || length(class_column) != 1) {
    stop("The 'class_column' argument must be a single string.")
  }
  
  message("Checking if the column exists in the polygons sf object...")
  if (!class_column %in% names(polygons)) {
    stop("The specified column does not exist in the polygons sf object.")
  }
  
  message("Performing spatial join...")
  intersections <- tryCatch({
    st_join(points, polygons, join = st_intersects)
  }, error = function(e) {
    stop("Error performing spatial join: ", e$message)
  })
  
  message("Counting the number of points per class...")
  count_per_class <- tryCatch({
    if (na) {
      intersections %>%
        group_by(.data[[class_column]]) %>%
        summarise(num_points = n(), .groups = 'drop')
    } else {
      intersections %>%
        filter(!is.na(.data[[class_column]])) %>%
        group_by(.data[[class_column]]) %>%
        summarise(num_points = n(), .groups = 'drop')
    }
  }, error = function(e) {
    stop("Error counting points per class: ", e$message)
  })
  
  message("Process completed. Returning the result...")
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  message(sprintf("Total function time: %.2f seconds", as.numeric(duration)))
  
  return(count_per_class)
}
