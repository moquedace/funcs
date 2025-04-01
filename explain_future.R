explain_future <- function (
    object, feature_names = NULL, X = NULL, nsim = 1, pred_wrapper = NULL,
    newdata = NULL, adjust = FALSE, baseline = NULL, shap_only = TRUE,
    parallel = FALSE, cores = NULL, n_blocks = 10, ...
) 
{
  cat("🔧 Installing and loading required packages...\n")
  source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
  
  pkg <- c(
    "dplyr", "future.apply", "stringr", "fastshap", "randomForest", "future",
    "parallelly", "gbm", "parallel", "doParallel", "shapviz", "quantregForest"
  )
  install_load_pkg(pkg)
  cat("✅ Packages successfully loaded.\n")
  
  if (is.null(X)) {
    stop("❌ Argument `X` (training features) is required.", call. = FALSE)
  }
  
  if (is.null(pred_wrapper)) {
    stop("❌ Argument `pred_wrapper` (prediction function) is required.", call. = FALSE)
  }
  
  cat("📦 Validating inputs and setting defaults...\n")
  if (inherits(X, what = "tbl_df")) X <- as.data.frame(X)
  if (is.null(newdata)) newdata <- X
  if (is.null(feature_names)) feature_names <- colnames(X)
  
  if (!is.null(newdata) && nrow(newdata) == 1L && nsim > 1L) {
    cat("🔄 Handling single-row `newdata` with `nsim > 1`...\n")
    newdata.stacked <- newdata[rep(1L, times = nsim), ]
    phis <- Recall(object, feature_names = feature_names, 
                   X = X, nsim = 1L, pred_wrapper = pred_wrapper, 
                   newdata = newdata.stacked, adjust = FALSE, parallel = parallel, 
                   cores = cores, ...)
    phi.avg <- t(colMeans(phis))
    
    if (isTRUE(adjust)) {
      cat("🛠️ Adjusting SHAP values for single observation...\n")
      fx <- pred_wrapper(object, newdata = newdata)
      fnull <- if (is.null(baseline)) mean(pred_wrapper(object, newdata = X)) else baseline
      phi.var <- apply(phis, MARGIN = 2, FUN = stats::var)
      err <- fx - sum(phi.avg) - fnull
      v <- (phi.var / max(phi.var)) * 1e6
      adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
      phi.avg <- phi.avg + adj
    } else {
      fnull <- 0
    }
    
    cat("✅ SHAP computation completed for single observation.\n")
    if (isFALSE(shap_only)) {
      return(structure(list(
        shapley_values = phi.avg,
        feature_values = newdata[, feature_names, drop = FALSE],
        baseline = fnull
      ), class = "explain"))
    } else {
      attr(phi.avg, which = "baseline") <- fnull
      class(phi.avg) <- c("explain", class(phi.avg))
      return(phi.avg)
    }
  }
  
  if (isTRUE(parallel)) {
    cat("⚙️ Enabling parallel execution...\n")
    if (is.null(cores)) cores <- pmax(1, future::availableCores() - 1)
    cat(paste0("🧵 Using ", cores, " cores...\n"))
    future::plan(future::multisession, workers = cores)
    options(future.globals.maxSize = 300 * 1024^3)
    on.exit(future::plan(future::sequential), add = TRUE)
  }
  
  if (isTRUE(adjust)) {
    if (nsim < 2) stop("❌ Argument `nsim` must be > 1 when `adjust = TRUE`.", call. = FALSE)
    cat("📐 Computing SHAP values with adjustment...\n")   
    
    
    pred_in_parallel_blocks <- function(
    object, newdata, n_blocks = n_blocks, workers = cores,
    pred_wrapper, pkg = pkg
    ) {
      future::plan(future::multisession, workers = workers)
      
      n_rows <- nrow(newdata)
      blocks <- split(seq_len(n_rows), cut(seq_len(n_rows), breaks = n_blocks, labels = FALSE))
      
      results <- future.apply::future_lapply(
        blocks,
        FUN = function(idx) {
          gc()
          source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
          suppressPackageStartupMessages(suppressMessages(
            invisible(capture.output(install_load_pkg(pkg)))
          ))
          block_data <- newdata[idx, , drop = FALSE]
          pred_wrapper(object, newdata = block_data)
        },
        future.globals = list(
          object = object,
          newdata = newdata,
          pred_wrapper = pred_wrapper,
          pkg = pkg,
          n_blocks = n_blocks,
          cores = cores
        ),
        future.packages = pkg
      )
      
      do.call(c, results)
    }
    
    
    
    fx <- pred_in_parallel_blocks(
      object = model_fit,
      newdata = shap_input,
      n_blocks = n_blocks,
      workers = cores,
      pred_wrapper = pred_fun,
      pkg = pkg
    )
    
   
    
    # fx <- pred_wrapper(object, newdata = newdata)
    fnull <- if (is.null(baseline)) mean(pred_wrapper(object, newdata = X)) else baseline
    
    compute_stats <- function(feature) {
      source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
      suppressPackageStartupMessages(suppressMessages(
        invisible(capture.output(install_load_pkg(pkg)))
      ))
      
      reps <- replicate(nsim, {
        fastshap::explain(
          object = object,
          feature_names = feature,
          X = X,
          pred_wrapper = pred_wrapper, 
          newdata = newdata
        )
      }, simplify = "matrix")
      
      means <- rowMeans(reps)
      vars  <- apply(reps, 1, stats::var)
      cbind(means, vars)
    }
    
    cat("🚀 Launching computation...\n")
    results <- if (parallel) {
      future.apply::future_lapply(feature_names, compute_stats, future.seed = TRUE, ...)
    } else {
      lapply(feature_names, compute_stats, ...)
    }
    
    cat("🧮 Aggregating results and applying adjustment...\n")
    phis.stats <- abind::abind(results, along = 3)
    
    if (length(dim(phis.stats)) == 3L) {
      for (i in seq_len(dim(phis.stats)[1L])) {
        err <- fx[i] - sum(phis.stats[i, 1L, ]) - fnull
        v <- (phis.stats[i, 2L, ] / max(phis.stats[i, 2L, ])) * 1e6
        adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
        phis.stats[i, 1L, ] <- phis.stats[i, 1L, ] + adj
      }
      phis <- phis.stats[, 1L, ]
    } else {
      err <- fx - sum(phis.stats[, 1L]) - fnull
      v <- (phis.stats[, 2L] / max(phis.stats[, 2L])) * 1e6
      adj <- err * (v - (v * sum(v)) / (1 + sum(v)))
      phis.stats[, 1L] <- phis.stats[, 1L] + adj
      phis <- phis.stats[, 1L]
    }
    
  } else {
    cat("📊 Computing SHAP values without adjustment...\n")
    
    compute_means <- function(feature) {
      source("https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R")
      suppressPackageStartupMessages(suppressMessages(
        invisible(capture.output(install_load_pkg(pkg)))
      ))
      
      fastshap::explain(
        object = object,
        feature_names = feature,
        X = X,
        pred_wrapper = pred_wrapper,
        newdata = newdata,
        nsim = nsim,
        shap_only = TRUE,
        adjust = FALSE
      )
    }
    
    phis <- if (parallel) {
      cat("🚀 Running computation in parallel...\n")
      future.apply::future_sapply(feature_names, compute_means,
                                  future.seed = TRUE, simplify = "matrix", ...)
    } else {
      sapply(feature_names, compute_means, simplify = "matrix", ...)
    }
    
    fnull <- 0
  }
  
  if (length(feature_names) == 1L) {
    phis <- as.matrix(phis)
  }
  
  colnames(phis) <- feature_names
  cat("✅ SHAP computation finished. Returning results...\n")
  
  if (isFALSE(shap_only)) {
    return(structure(list(
      shapley_values = phis,
      feature_values = newdata[, feature_names, drop = FALSE],
      baseline = fnull
    ), class = "explain"))
  } else {
    attr(phis, which = "baseline") <- fnull
    class(phis) <- c("explain", class(phis))
    return(phis)
  }
}
