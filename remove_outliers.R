
remove_outliers <- function(
    df, columns, multiplier = 1.5, group_cols = NULL, verbose = FALSE
    ) {
  
  source(
    "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
  )
  
  pkg <- c(
    "dplyr", "purrr"
  )
  
  install_load_pkg(pkg)
  
  if (!is.data.frame(df)) stop("error: 'df' must be a dataframe")
  if (!is.numeric(multiplier) || multiplier <= 0) stop("error: 'multiplier' must be a positive number")
  if (!is.logical(verbose)) stop("error: 'verbose' must be TRUE or FALSE")
  
  invalid_columns <- columns[!columns %in% names(df)]
  if (length(invalid_columns) > 0) {
    stop("error: the following columns do not exist in the dataframe: ", paste(invalid_columns, collapse = ", "))
  }
  
  non_numeric_columns <- purrr::keep(columns, function(col) {
    !(is.numeric(df[[col]]) || is.integer(df[[col]]))
  })
  if (length(non_numeric_columns) > 0) {
    stop("error: the following columns are not numeric: ", paste(non_numeric_columns, collapse = ", "))
  }
  
  if (!is.null(group_cols)) {
    invalid_groups <- group_cols[!group_cols %in% names(df)]
    if (length(invalid_groups) > 0) {
      stop("error: the following grouping columns do not exist in the dataframe: ", paste(invalid_groups, collapse = ", "))
    }
    df <- df %>% group_by(across(all_of(group_cols)))
  }
  
  process_subset <- function(sub_df, group_info = NULL) {
    group_name <- if (!is.null(group_info)) {
      paste(names(group_info), "=", as.character(group_info), collapse = " | ")
    } else {
      "entire dataset"
    }
    for (col in columns) {
      min_before <- min(sub_df[[col]], na.rm = TRUE)
      max_before <- max(sub_df[[col]], na.rm = TRUE)
      
      Q1 <- quantile(sub_df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(sub_df[[col]], 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      
      lower_bound <- Q1 - multiplier * IQR_val
      upper_bound <- Q3 + multiplier * IQR_val
      
      initial_rows <- nrow(sub_df)
      sub_df <- sub_df %>% filter(.data[[col]] >= lower_bound & .data[[col]] <= upper_bound)
      removed_rows <- initial_rows - nrow(sub_df)
      
      if (verbose) {
        cat("\ngroup:", group_name, "\n")
        cat("column:", col, "\n")
        cat("cut-off min limit:", lower_bound, "| cut-off max limit:", upper_bound, "\n")
        cat("column min value before:", min_before, "| column max value before:", max_before, "\n")
        if (nrow(sub_df) > 0) {
          min_after <- min(sub_df[[col]], na.rm = TRUE)
          max_after <- max(sub_df[[col]], na.rm = TRUE)
        } else {
          min_after <- NA
          max_after <- NA
        }
        cat("column min value after:", min_after, "| column max value after:", max_after, "\n")
        cat("removed", removed_rows, "outliers\n")
        cat("-----------------------------------------------------------\n")
      }
    }
    return(sub_df)
  }
  
  if (!is.null(group_cols)) {
    df <- df %>% group_modify(~ process_subset(.x, .y)) %>% ungroup()
  } else {
    df <- process_subset(df, NULL)
  }
  
  return(df)
}
