spl <- function(obj = NULL,
                id = NULL,
                upper_limit = NULL,
                lower_limit = NULL,
                var_name = NULL,
                lam = 0.1,
                d = c(0, 5, 15, 30, 60, 100, 200),
                vlow = 0,
                vhigh = 1000,
                rmse = TRUE,
                fill_depth_error = TRUE) {
  
  # start timer
  t_start <- Sys.time()
  
  # load required packages (external dependency)
  source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
  install_load_pkg(c("dplyr", "mpspline2"))
  
  # basic input checks
  if (is.null(obj) || !is.data.frame(obj)) stop("'obj' must be a non-null data.frame.")
  if (is.null(id) || is.null(upper_limit) || is.null(lower_limit) || is.null(var_name)) {
    stop("arguments 'id', 'upper_limit', 'lower_limit', and 'var_name' must be provided.")
  }
  if (!all(c(id, upper_limit, lower_limit, var_name) %in% names(obj))) {
    stop("specified column names not found in 'obj'.")
  }
  
  # prepare data frame for mpspline
  df_spl <- obj %>%
    dplyr::relocate(all_of(c(id, upper_limit, lower_limit))) %>%
    dplyr::rename(
      upper_limit = !!upper_limit,
      lower_limit = !!lower_limit,
      profile = !!id
    )
  
  # check if depth columns are numeric
  if (!is.numeric(df_spl$upper_limit) || !is.numeric(df_spl$lower_limit)) {
    stop("'upper_limit' and 'lower_limit' columns must be numeric.")
  }
  
  # perform mass-preserving spline interpolation
  spline_list <- mpspline(
    obj = df_spl,
    var_name = var_name,
    lam = lam,
    d = d,
    vlow = vlow,
    vhigh = vhigh
  )
  
  # column names
  colnames_est <- paste0(var_name, "_", d[-length(d)], "_", d[-1], "cm")
  
  # build final dataframe
  if (fill_depth_error) {
    df_result <- lapply(names(spline_list), function(nm) {
      dcm <- spline_list[[nm]]$est_dcm
      icm <- spline_list[[nm]]$est_icm
      
      if (all(is.na(dcm))) {
        icm_name <- names(icm)[1]
        icm_value <- icm[1]
        
        filled <- as.list(rep(icm_value, length(colnames_est)))
        names(filled) <- colnames_est
        
        df <- data.frame(
          id_value = nm,
          filled,
          error_estimate_depth = icm_name
        )
        names(df)[1] <- id
        return(df)
        
      } else {
        values <- t(dcm[1:(length(d) - 1)])
        df <- setNames(data.frame(nm, values), c(id, colnames_est))
        df$error_estimate_depth <- NA_character_
        return(df)
      }
    }) %>% bind_rows()
    
  } else {
    df_result <- lapply(names(spline_list), function(nm) {
      est_dcm <- spline_list[[nm]]$est_dcm
      est_vec <- t(est_dcm[1:(length(d) - 1)])
      setNames(data.frame(nm, est_vec), c(id, colnames_est))
    }) %>% bind_rows()
  }
  
  # add rmse if requested
  if (rmse) {
    rmse_colname <- paste0(var_name, "_rmse")
    rmse_values <- data.frame(
      id_value = names(spline_list),
      rmse = sapply(spline_list, function(x) x$est_err[["RMSE"]])
    )
    names(rmse_values) <- c(id, rmse_colname)
    df_result <- dplyr::left_join(df_result, rmse_values, by = id)
  }
  
  # end timer and print duration
  t_end <- Sys.time()
  duration <- t_end - t_start
  cat(sprintf(
    "time for spline | variable: %s | profiles: %d | duration: %.2f %s\n",
    var_name,
    length(spline_list),
    duration,
    units(duration)
  ))
  
  return(df_result)
}
