partial_dependence <- function(model, fullvar = TRUE, xname = NULL,
                               lci = 0.25, uci = 0.75, delta = FALSE,
                               ncores = detectCores() - 1) {
  
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library,
                                          character.only = TRUE))
  }
  
  required_packages <- c("dplyr", "caret", "rfUtilities", "future.apply",
                         "quantregForest", "tidyr", "stringr", "future")
  
  install_and_load_packages(required_packages)
  
  m <- model$finalModel
  x <- model$trainingData %>% select(-.outcome)
  yname <- model[["terms"]][[2]]
  
  if (!any(class(m) %in% c("randomForest", "list"))) 
    stop("m is not um objeto randomForest")
  if (m$type != "regression") 
    stop("classificação não é suportada")
  
  start_time <- Sys.time()
  message("Iniciando a execução da função partial_dependence...")
  
  df_dep_final <- data.frame()
  
  if (fullvar) {
    varnames <- model$trainingData %>% select(-.outcome) %>% names() %>% sort()
  } else {
    if (is.null(xname)) {
      stop("xname deve ser fornecido quando fullvar é FALSE")
    }
    varnames <- xname
  }
  
  
  
  for (xnamefvar in varnames) {
    
    message(paste("Processando a variável:", xnamefvar))
    
    conf.int <- (uci - lci) * 100
    temp <- sort(x[, xnamefvar])
    y <- stats::predict(m, x, what = 0.5)
    
    
    plan(multisession, workers = ncores)  
    
    
    results <- future_lapply(1:length(temp), function(i) {
      
      required_packages <- c("dplyr", "caret", "rfUtilities", "future.apply",
                             "quantregForest", "tidyr", "stringr", "future")
      suppressPackageStartupMessages(sapply(required_packages, require,
                                            character.only = TRUE))
      
      
      x_temp <- x
      x_temp[, xnamefvar] <- temp[i]
      y.hat <- stats::predict(m, x_temp, what = mean)
      if (delta == TRUE) {
        y.hat <- y.hat - y
      }
      y.hat.mean <- stats::weighted.mean(y.hat, na.rm = TRUE)
      y.hat.lb <- stats::quantile(y.hat, lci, na.rm = TRUE)
      y.hat.ub <- stats::quantile(y.hat, uci, na.rm = TRUE)
      c(temp[i], y.hat.mean, y.hat.lb, y.hat.ub)
    }, future.seed = TRUE)
    
    
    results <- do.call(rbind, results)
    m.ci <- as.data.frame(results)
    names(m.ci) <- c(xnamefvar, yname, "lci", "uci")
    
    
    df_dep <- m.ci %>% 
      gather(key = "var", value = "cci", -c(yname, lci, uci))
    
    
    if(exists("df_dep_final")) {
      
      df_dep_final <- rbind(df_dep_final, df_dep)
      
    } else {
      
      df_dep_final <- df_dep 
      
    }
    
    
  }
  
  end_time <- Sys.time()
  message(paste("Execução finalizada. Tempo total:", round(difftime(end_time, start_time, units = "secs"), 2), "segundos."))
  
  return(df_dep_final)
  
}

