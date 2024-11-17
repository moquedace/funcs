
# spline function ---------------------------------------------------------


spl <- function(obj = NULL,
                id = NULL,
                upper_limit = NULL,
                lower_limit = NULL,
                var_name = NULL,
                lam = 0.1,
                d = c(0, 5, 15, 30, 60, 100, 200),
                vlow = 0,
                vhigh = 1000) {
  
  
  
  
  pkg <- c("dplyr", "mpspline2")
  
  
  installed_packages <- pkg %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("installing package", pkg[!installed_packages]))
    install.packages(pkg[!installed_packages])
    print("installed packages")
  }
  
  
  invisible(lapply(pkg, library, character.only = TRUE))
  
  
  
  
  df_spl <- obj %>% 
    relocate(id, upper_limit, lower_limit) %>% 
    rename("upper_limit" = upper_limit,
           "lower_limit" = lower_limit)
  
  if (is.numeric(df_spl$upper_limit) & is.numeric(df_spl$lower_limit)) {
    
    
    spl <- mpspline(obj = df_spl, var_name = var_name,
                    lam = lam, d = d, vlow = vlow, 
                    vhigh = vhigh)
    
    
    
    
    
    
    for (i in seq_along(spl)) {
      
      if (i == 1) {
        
        df_spline <- data.frame(perfil = names(spl)[i], t(spl[[i]][["est_dcm"]]))
        
        
      } else {
        df_spline <- rbind(df_spline,
                           data.frame(perfil = names(spl)[i],
                                      t(spl[[i]][["est_dcm"]][1:(length(d)-1)])))
        
      }
      
    }
    
    
    return(df_spline) 
  } else {
    print("upper bound or lower bound column is not numeric")
  }
  
}


