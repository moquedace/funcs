aoa_meyer <- function(newdata, model = NA, trainDI = NA, train = NULL, weight = NA, 
                      variables = "all", CVtest = NULL, CVtrain = NULL, method = "L2", 
                      useWeight = TRUE, LPD = FALSE, maxLPD = 1, indices = FALSE, 
                      verbose = TRUE, parallel = FALSE, ncores) 
{
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    invisible(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("future", "foreach", "doFuture",
                         "doParallel", "dplyr", "caret", "CAST")
  install_and_load_packages(required_packages)
  
  
  
  options(future.globals.maxSize = 2 * 1024^3)
  .knnindexfun <- function (point, reference, method, S_inv = NULL, maxLPD = maxLPD) 
  {
    if (method == "L2") {
      return(FNN::knnx.index(reference, point, k = maxLPD))
    }
    else if (method == "MD") {
      stop("MD currently not implemented for LPD")
    }
  }
  
  
  
  .knndistfun <- function (point, reference, method, S_inv = NULL, maxLPD = maxLPD) 
  {
    if (method == "L2") {
      return(FNN::knnx.dist(reference, point, k = maxLPD))
    }
    else if (method == "MD") {
      return(t(sapply(1:dim(point)[1], function(y) sort(sapply(1:dim(reference)[1], 
                                                               function(x) sqrt(t(point[y, ] - reference[x, ]) %*% 
                                                                                  S_inv %*% (point[y, ] - reference[x, ]))))[1:maxLPD])))
    }
  }
  
  
  
  
  
  
  
  
  
  newdata <- rast(newdata)
  as_stars <- FALSE
  leading_digit <- any(grepl("^{1}[0-9]", names(newdata)))
  if (inherits(newdata, "stars")) {
    if (!requireNamespace("stars", quietly = TRUE)) 
      stop("package stars required: install that first")
    newdata <- methods::as(newdata, "SpatRaster")
    as_stars <- TRUE
  }
  if (inherits(newdata, "Raster")) {
    message("Raster will soon not longer be supported. Use terra or stars instead")
    newdata <- methods::as(newdata, "SpatRaster")
  }
  calc_LPD <- LPD
  if (LPD == TRUE) {
    if (is.numeric(maxLPD)) {
      if (maxLPD <= 0) {
        stop("maxLPD cannot be negative or equal to 0. Define a number between 0 and 1 for a percentage of the number of training samples, or a whole number larger than 1 and smaller than the number of training samples.")
      }
      if (maxLPD <= 1) {
        if (inherits(model, "train")) {
          maxLPD <- round(maxLPD * as.integer(length(model$trainingData[[1]])))
        } else if (!is.null(train)) {
          maxLPD <- round(maxLPD * as.integer(length(train[[1]])))
        }
        if (maxLPD <= 1) {
          stop("The percentage you provided for maxLPD is too small.")
        }
      }
      if (maxLPD > 1) {
        if (maxLPD %% 1 == 0) {
          maxLPD <- as.integer(maxLPD)
        } else {
          stop("If maxLPD is bigger than 0, it should be a whole number. Define a number between 0 and 1 for a percentage of the number of training samples, or a whole number larger than 1 and smaller than the number of training samples.")
        }
      }
      if ((maxLPD > length(if (inherits(model, "train")) {
        model$trainingData[[1]]
      } else if (!is.null(train)) {
        train[[1]]
      })) || maxLPD %% 1 != 0) {
        stop("maxLPD cannot be bigger than the number of training samples. Define a number between 0 and 1 for a percentage of the number of training samples, or a whole number larger than 1 and smaller than the number of training samples.")
      }
    } else {
      stop("maxLPD must be a number. Define a number between 0 and 1 for a percentage of the number of training samples, or a whole number larger than 1 e smaller than the number of training samples.")
    }
  }
  if (!inherits(trainDI, "trainDI")) {
    if (verbose) {
      message("No trainDI provided.")
    }
    trainDI <- trainDI(model, train, variables, weight, 
                       CVtest, CVtrain, method, useWeight, LPD, verbose)
  }
  if (calc_LPD == TRUE) {
    trainDI$maxLPD <- maxLPD
  }
  if (any(trainDI$variables %in% names(newdata) == FALSE)) {
    if (leading_digit) {
      stop("names of newdata start with leading digits, automatically added 'X' results in mismatching names of train data in the model")
    }
    stop("names of newdata don't match names of train data in the model")
  }
  out <- NA
  if (inherits(newdata, "SpatRaster")) {
    out <- newdata[[1]]
    names(out) <- "DI"
  }
  if (inherits(newdata, "SpatRaster")) {
    if (any(is.factor(newdata))) {
      newdata[[which(is.factor(newdata))]] <- as.numeric(newdata[[which(is.factor(newdata))]])
    }
    newdata <- terra::as.data.frame(newdata, na.rm = FALSE)
  }
  newdata <- newdata[, na.omit(match(trainDI$variables, names(newdata))), 
                     drop = FALSE]
  catvars <- trainDI$catvars
  if (!inherits(catvars, "error") & length(catvars) > 0) {
    for (catvar in catvars) {
      trainDI$train[, catvar] <- droplevels(trainDI$train[, 
                                                          catvar])
      newdata[, catvar] <- factor(newdata[, catvar])
      newdata[!newdata[, catvar] %in% unique(trainDI$train[, 
                                                           catvar]), catvar] <- NA
      newdata[, catvar] <- droplevels(newdata[, catvar])
      dvi_train <- predict(caret::dummyVars(paste0("~", 
                                                   catvar), data = trainDI$train), trainDI$train)
      dvi_newdata <- predict(caret::dummyVars(paste0("~", 
                                                     catvar), data = trainDI$train), newdata)
      dvi_newdata[is.na(newdata[, catvar]), ] <- 0
      trainDI$train <- data.frame(trainDI$train, dvi_train)
      newdata <- data.frame(newdata, dvi_newdata)
    }
    newdata <- newdata[, -which(names(newdata) %in% catvars)]
    trainDI$train <- trainDI$train[, -which(names(trainDI$train) %in% 
                                              catvars)]
  }
  newdata <- scale(newdata, center = trainDI$scaleparam$`scaled:center`, 
                   scale = trainDI$scaleparam$`scaled:scale`)
  if (!inherits(trainDI$weight, "error")) {
    tmpnames <- names(newdata)
    newdata <- sapply(1:ncol(newdata), function(x) {
      newdata[, x] * unlist(trainDI$weight[x])
    })
    names(newdata) <- tmpnames
  }
  train_scaled <- scale(trainDI$train, center = trainDI$scaleparam$`scaled:center`, 
                        scale = trainDI$scaleparam$`scaled:scale`)
  train_scaled <- sapply(1:ncol(train_scaled), function(x) {
    train_scaled[, x] * unlist(trainDI$weight[x])
  })
  okrows <- which(apply(newdata, 1, function(x) all(!is.na(x))))
  newdataCC <- newdata[okrows, , drop = F]
  if (method == "MD") {
    if (dim(train_scaled)[2] == 1) {
      S <- matrix(stats::var(train_scaled), 1, 1)
      newdataCC <- as.matrix(newdataCC, ncol = 1)
    }
    else {
      S <- stats::cov(train_scaled)
    }
    S_inv <- MASS::ginv(S)
  }
  if (calc_LPD == FALSE) {
    if (verbose) {
      message("Computing DI of new data...")
    }
    mindist <- rep(NA, nrow(newdata))
    mindist[okrows] <- .mindistfun(newdataCC, train_scaled, 
                                   method, S_inv)
    DI_out <- mindist/trainDI$trainDist_avrgmean
  }
  if (calc_LPD == TRUE) {
    if (verbose) {
      message("Computing DI and LPD of new data...")
    }
    if (verbose) {
      pb <- txtProgressBar(min = 0, max = nrow(newdataCC), 
                           style = 3)
    }
    DI_out <- rep(NA, nrow(newdata))
    LPD_out <- rep(NA, nrow(newdata))
    if (indices) {
      Indices_out <- matrix(NA, nrow = nrow(newdata), 
                            ncol = maxLPD)
    }
    
    if (parallel) {
      # Paralelizar com o future
      plan(multisession, workers = ncores)
      
      results <- future.apply::future_lapply(seq(nrow(newdataCC)), function(i) {
        knnDist <- .knndistfun(t(matrix(newdataCC[i, ])), train_scaled, method, S_inv, maxLPD = maxLPD)
        knnDI <- knnDist / trainDI$trainDist_avrgmean
        knnDI <- c(knnDI)
        di_out <- knnDI[1]
        lpd_out <- sum(knnDI < trainDI$threshold)
        knnIndex <- .knnindexfun(t(matrix(newdataCC[i, ])), train_scaled, method, S_inv, maxLPD = lpd_out)
        list(di_out, lpd_out, knnIndex)
      })
      
      # Encerrar o plano de paralelização
      plan(sequential)
      
    } else {
      # Sem paralelização
      results <- lapply(seq(nrow(newdataCC)), function(i) {
        knnDist <- .knndistfun(t(matrix(newdataCC[i, ])), train_scaled, method, S_inv, maxLPD = maxLPD)
        knnDI <- knnDist / trainDI$trainDist_avrgmean
        knnDI <- c(knnDI)
        di_out <- knnDI[1]
        lpd_out <- sum(knnDI < trainDI$threshold)
        knnIndex <- .knnindexfun(t(matrix(newdataCC[i, ])), train_scaled, method, S_inv, maxLPD = lpd_out)
        list(di_out, lpd_out, knnIndex)
      })
    }
    
    DI_out[okrows] <- sapply(results, `[[`, 1)
    LPD_out[okrows] <- sapply(results, `[[`, 2)
    if (indices) {
      for (i in seq_along(okrows)) {
        if (LPD_out[okrows[i]] > 0) {
          Indices_out[okrows[i], 1:LPD_out[okrows[i]]] <- results[[i]][[3]]
        }
      }
    }
    if (verbose) {
      close(pb)
    }
    realMaxLPD <- max(LPD_out, na.rm = T)
    if (maxLPD > realMaxLPD) {
      if (inherits(maxLPD, c("numeric", "integer")) && verbose) {
        message("Your specified maxLPD is bigger than the real maxLPD of you predictor data.")
      }
      if (verbose) {
        message(paste("maxLPD is set to", realMaxLPD))
      }
      trainDI$maxLPD <- realMaxLPD
    }
    if (indices) {
      Indices_out <- Indices_out[, 1:trainDI$maxLPD]
    }
  }
  if (verbose) {
    message("Computing AOA...")
  }
  if (inherits(out, "SpatRaster")) {
    terra::values(out) <- DI_out
    AOA <- out
    terra::values(AOA) <- 1
    AOA[out > trainDI$thres] <- 0
    AOA <- terra::mask(AOA, out)
    names(AOA) = "AOA"
    if (calc_LPD == TRUE) {
      LPD <- out
      terra::values(LPD) <- LPD_out
      names(LPD) = "LPD"
    }
    if (as_stars) {
      out <- stars::st_as_stars(out)
      AOA <- stars::st_as_stars(AOA)
      if (calc_LPD == TRUE) {
        LPD <- stars::st_as_stars(LPD)
      }
    }
  } else {
    out <- DI_out
    AOA <- rep(1, length(out))
    AOA[out > trainDI$thres] <- 0
    if (calc_LPD == TRUE) {
      LPD <- LPD_out
    }
  }
  result <- list(parameters = trainDI, DI = out, AOA = AOA)
  if (calc_LPD == TRUE) {
    result$LPD <- LPD
    if (indices) {
      result$indices <- Indices_out
    }
  }
  if (verbose) {
    message("Finished!")
  }
  class(result) <- "aoa"
  return(result)
}
