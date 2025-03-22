spl <- function(obj = NULL,
                id = NULL,
                upper_limit = NULL,
                lower_limit = NULL,
                var_name = NULL,
                lam = 0.1,
                d = c(0, 5, 15, 30, 60, 100, 200),
                vlow = 0,
                vhigh = 1000) {
  
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
  
  # compile output into tidy data.frame
  df_result <- lapply(names(spline_list), function(nm) {
    est_dcm <- spline_list[[nm]]$est_dcm
    est_vec <- t(est_dcm[1:(length(d) - 1)])
    data.frame(profile = nm, est_vec)
  }) %>% bind_rows()
  
  # rename depth columns
  names(df_result)[-1] <- paste0(var_name, "_", d[-length(d)], "_", d[-1], "cm")
  
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
