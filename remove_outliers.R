remove_outliers <- function(df, columns, group_info = NULL, multiplier = 1.5, verbose = FALSE) {
  
    source(
    "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
  )
  
  pkg <- c(
    "dplyr"
           )
  
  install_load_pkg(pkg)
  
  if (!is.data.frame(df)) stop("error: 'df' must be a dataframe")
  if (!is.numeric(multiplier) || multiplier <= 0) stop("error: 'multiplier' must be a positive number")
  if (!is.logical(verbose)) stop("error: 'verbose' must be TRUE or FALSE")
  
  invalid_columns <- columns[!columns %in% names(df)]
  if (length(invalid_columns) > 0) {
    stop("error: the following columns do not exist in the dataframe: ", paste(invalid_columns, collapse = ", "))
  }
  
  non_numeric_columns <- columns[!sapply(df[columns], is.numeric)]
  if (length(non_numeric_columns) > 0) {
    stop("error: the following columns are not numeric: ", paste(non_numeric_columns, collapse = ", "))
  }
  
  group_name <- if (!is.null(group_info)) {
    paste(names(group_info), "=", as.character(group_info), collapse = " | ")
  } else {
    "entire dataset"
  }
  
  for (column in columns) {
    min_before <- min(df[[column]], na.rm = TRUE)
    max_before <- max(df[[column]], na.rm = TRUE)
    
    Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - multiplier * IQR
    upper_bound <- Q3 + multiplier * IQR
    
    initial_rows <- nrow(df)
    df <- df %>% filter(.data[[column]] >= lower_bound & .data[[column]] <= upper_bound)
    removed_rows <- initial_rows - nrow(df)
    
    min_after <- min(df[[column]], na.rm = TRUE)
    max_after <- max(df[[column]], na.rm = TRUE)
    
    if (verbose) {
      cat("\ngroup:", group_name, "\n")
      cat("column:", column, "\n")
      cat("cut-off min limit:", lower_bound, "| cut-off max limit:", upper_bound, "\n")
      cat("column min value before:", min_before, "| column max value before:", max_before, "\n")
      cat("column min value after:", min_after, "| column max value after:", max_after, "\n")
      cat("removed", removed_rows, "outliers\n")
      cat("-----------------------------------------------------------\n")
    }
  }
  return(df)
}
