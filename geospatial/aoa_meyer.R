aoa_meyer <- function(
    newdata, model = NA, trainDI = NA, train = NULL, weight = NA, 
    variables = "all", CVtest = NULL, CVtrain = NULL, method = "L2", 
    useWeight = TRUE, useCV = TRUE, LPD = FALSE, maxLPD = 1, 
    indices = FALSE, verbose = TRUE, algorithm = "brute", 
    parallel = FALSE, ncores = 2
) {
  
  
  
  source(
    "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
  )
  
  pkg <- c(
    "dplyr", "future.apply", "stringr", "terra", "CAST", "future"
  )
  install_load_pkg(pkg)
  
  
  
  start_time <- Sys.time()
  message("starting execution...")
  
  options(future.globals.maxSize = 300 * 1024^3)
  
  .knnindexfun <- function(point, reference, method, S_inv = NULL,
                           maxLPD = maxLPD) {
    if (method == "L2") {
      return(FNN::knnx.index(reference, point, k = maxLPD))
    } else if (method == "MD") {
      stop("MD currently not implemented for LPD")
    }
  }
  
  .knndistfun <- function(point, reference, method, S_inv = NULL,
                          maxLPD = maxLPD) {
    if (method == "L2") {
      return(FNN::knnx.dist(reference, point, k = maxLPD))
    } else if (method == "MD") {
      return(
        t(sapply(1:dim(point)[1], function(y) sort(
          sapply(1:dim(reference)[1], 
                 function(x) sqrt(t(point[y, ] - reference[x, ]) %*% 
                                    S_inv %*% (
                                      point[y, ] - reference[x, ])
                 )))[1:maxLPD]))
      )
    }
  }
  
  
  newdata <- terra::rast(newdata)
  as_stars <- FALSE
  leading_digit <- any(grepl("^{1}[0-9]", names(newdata)))
  
  if (inherits(newdata, "stars")) {
    if (!requireNamespace("stars", quietly = TRUE)) stop(
      "Package 'stars' required but not installed."
    )
    newdata <- methods::as(newdata, "SpatRaster")
    as_stars <- TRUE
  }
  if (inherits(newdata, "Raster")) {
    message(
      "warning: 'raster' will soon not be supported. converting to 'terra' object."
    )
    newdata <- methods::as(newdata, "SpatRaster")
  }
  
  calc_LPD <- LPD
  
  if (LPD) {
    message("calculating local prediction density (LPD)...")
    if (is.numeric(maxLPD)) {
      if (maxLPD <= 0) stop("maxLPD must be greater than 0.")
      if (maxLPD <= 1) {
        if (inherits(model, "train")) {
          maxLPD <- round(
            maxLPD * as.integer(length(model$trainingData[[1]]))
          )
        } else if (!is.null(train)) {
          maxLPD <- round(maxLPD * as.integer(length(train[[1]])))
        }
        if (maxLPD <= 1) stop(
          "The percentage provided for maxLPD is too small."
        )
      }
      if (maxLPD > 1 && maxLPD %% 1 != 0) {
        stop("maxLPD must be an integer if larger than 1.")
      }
    } else {
      stop("maxLPD must be numeric.")
    }
  }
  
  if (!inherits(trainDI, "trainDI")) {
    if (verbose) message("no trainDI provided. creating it...")
    trainDI <- trainDI(
      model, train, variables, weight, CVtest,
      CVtrain, method, useWeight, useCV, LPD, verbose, algorithm
    )
  }
  
  if (calc_LPD) trainDI$maxLPD <- maxLPD
  
  if (any(trainDI$variables %in% names(newdata) == FALSE)) {
    if (leading_digit) stop(
      "Mismatch between variable names: leading digits issue."
    )
    stop("Mismatch between newdata and training data variable names.")
  }
  
  out <- NA
  if (inherits(newdata, "SpatRaster")) {
    out <- newdata[[1]]
    names(out) <- "DI"
  }
  if (inherits(newdata, "SpatRaster")) {
    if (any(is.factor(newdata))) {
      newdata[[which(is.factor(newdata))]] <- as.numeric(
        newdata[[which(is.factor(newdata))]]
      )
    }
    newdata <- terra::as.data.frame(newdata, na.rm = FALSE)
  }
  
  newdata <- newdata[, na.omit(
    match(trainDI$variables, names(newdata))
  ), drop = FALSE]
  
  catvars <- trainDI$catvars
  if (!inherits(catvars, "error") && length(catvars) > 0) {
    message("handling categorical variables...")
    for (catvar in catvars) {
      trainDI$train[, catvar] <- droplevels(trainDI$train[, catvar])
      newdata[, catvar] <- factor(newdata[, catvar])
      newdata[!newdata[, catvar] %in% unique(
        trainDI$train[, catvar]
      ), catvar] <- NA
      newdata[, catvar] <- droplevels(newdata[, catvar])
      
      dvi_train <- predict(
        caret::dummyVars(
          paste0("~", catvar), data = trainDI$train
        ), trainDI$train
      )
      dvi_newdata <- predict(
        caret::dummyVars(paste0("~", catvar), data = trainDI$train), newdata
      )
      dvi_newdata[is.na(newdata[, catvar]), ] <- 0
      
      trainDI$train <- data.frame(trainDI$train, dvi_train)
      newdata <- data.frame(newdata, dvi_newdata)
    }
    newdata <- newdata[, -which(names(newdata) %in% catvars)]
    trainDI$train <- trainDI$train[, -which(
      names(trainDI$train) %in% catvars
    )]
  }
  
  newdata <- scale(
    newdata, center = trainDI$scaleparam$`scaled:center`,
    scale = trainDI$scaleparam$`scaled:scale`
  )
  
  if (!inherits(trainDI$weight, "error")) {
    tmpnames <- names(newdata)
    newdata <- sapply(
      1:ncol(newdata), function(x) newdata[, x] * unlist(trainDI$weight[x])
    )
    names(newdata) <- tmpnames
  }
  
  train_scaled <- scale(
    trainDI$train,
    center = trainDI$scaleparam$`scaled:center`,
    scale = trainDI$scaleparam$`scaled:scale`
  )
  train_scaled <- sapply(
    1:ncol(train_scaled),
    function(x) train_scaled[, x] * unlist(trainDI$weight[x])
  )
  
  okrows <- which(apply(newdata, 1, function(x) all(!is.na(x))))
  newdataCC <- newdata[okrows, , drop = FALSE]
  
  if (method == "MD") {
    if (ncol(train_scaled) == 1) {
      S <- matrix(stats::var(train_scaled), 1, 1)
      newdataCC <- as.matrix(newdataCC, ncol = 1)
    } else {
      S <- stats::cov(train_scaled)
    }
    S_inv <- MASS::ginv(S)
  }
  
  if (!calc_LPD) {
    message("computing distance index (DI) only...")
    mindist <- rep(NA, nrow(newdata))
    mindist[okrows] <- .mindistfun(
      newdataCC, train_scaled, method, S_inv, algorithm
    )
    DI_out <- mindist / trainDI$trainDist_avrgmean
  } else {
    message("computing DI and LPD...")
    DI_out <- rep(NA, nrow(newdata))
    LPD_out <- rep(NA, nrow(newdata))
    if (indices) {
      Indices_out <- matrix(NA, nrow = nrow(newdata), ncol = maxLPD)
    }
    
    if (parallel) {
      message(
        "parallel execution started with ", ncores, " cores..."
      )
      future::plan(multisession, workers = ncores)
      on.exit(future::plan(future::sequential), add = TRUE)
      
      results <- future.apply::future_lapply(
        seq_len(nrow(newdataCC)), function(i) {
          knnDist <- .knndistfun(
            t(matrix(newdataCC[i, ])), train_scaled, method,
            S_inv, maxLPD = maxLPD
          )
          knnDI <- knnDist / trainDI$trainDist_avrgmean
          DI_val <- knnDI[1]
          LPD_val <- sum(knnDI < trainDI$threshold)
          
          indices_val <- NULL
          if (indices && LPD_val > 0) {
            indices_val <- .knnindexfun(
              t(matrix(newdataCC[i, ])), train_scaled,
              method, S_inv, maxLPD = LPD_val
            )
          }
          
          list(DI = DI_val, LPD = LPD_val, indices = indices_val)
        }
      )
      
      for (j in seq_along(results)) {
        DI_out[okrows[j]] <- results[[j]]$DI
        LPD_out[okrows[j]] <- results[[j]]$LPD
        if (indices && !is.null(results[[j]]$indices)) {
          Indices_out[okrows[j],
                      1:LPD_out[okrows[j]]] <- results[[j]]$indices
        }
      }
      
      future::plan(sequential)
      message("parallel execution finished. Returning to sequential mode.")
    } else {
      message("sequential execution started...")
      pb <- txtProgressBar(min = 0, max = nrow(newdataCC), style = 3)
      for (i in seq_len(nrow(newdataCC))) {
        knnDist <- .knndistfun(
          t(matrix(newdataCC[i, ])), train_scaled, method,
          S_inv, maxLPD = maxLPD
        )
        knnDI <- knnDist / trainDI$trainDist_avrgmean
        knnDI <- c(knnDI)
        DI_out[okrows[i]] <- knnDI[1]
        LPD_out[okrows[i]] <- sum(knnDI < trainDI$threshold)
        knnIndex <- .knnindexfun(
          t(matrix(newdataCC[i, ])), train_scaled, method,
          S_inv, maxLPD = LPD_out[okrows[i]]
        )
        if (indices && LPD_out[okrows[i]] > 0) {
          Indices_out[okrows[i], 1:LPD_out[okrows[i]]] <- knnIndex
        }
        setTxtProgressBar(pb, i)
      }
      close(pb)
      message("sequential execution finished.")
    }
    
    realMaxLPD <- max(LPD_out, na.rm = TRUE)
    if (maxLPD > realMaxLPD) {
      message(
        "specified maxLPD is larger than real maximum LPD. adjusting."
      )
      trainDI$maxLPD <- realMaxLPD
    }
    
    if (indices) {
      Indices_out <- Indices_out[, 1:trainDI$maxLPD, drop = FALSE]
    }
  }
  
  if (verbose) {
    message("computing area of applicability (AOA)...")
  }
  
  if (inherits(out, "SpatRaster")) {
    terra::values(out) <- DI_out
    AOA <- out
    terra::values(AOA) <- 1
    AOA[out > trainDI$thres] <- 0
    AOA <- terra::mask(AOA, out)
    names(AOA) <- "AOA"
    if (calc_LPD) {
      LPD <- out
      terra::values(LPD) <- LPD_out
      names(LPD) <- "LPD"
    }
    if (as_stars) {
      out <- stars::st_as_stars(out)
      AOA <- stars::st_as_stars(AOA)
      if (calc_LPD) {
        LPD <- stars::st_as_stars(LPD)
      }
    }
  } else {
    out <- DI_out
    AOA <- rep(1, length(out))
    AOA[out > trainDI$thres] <- 0
    if (calc_LPD) {
      LPD <- LPD_out
    }
  }
  
  result <- list(parameters = trainDI, DI = out, AOA = AOA)
  if (calc_LPD) {
    result$LPD <- LPD
    if (indices) {
      result$indices <- Indices_out
    }
  }
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  message(
    "finished execution in ",
    round(as.numeric(duration), 2), " ", units(duration)
  )
  
  class(result) <- "aoa"
  return(result)
}
