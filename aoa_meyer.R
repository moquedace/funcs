aoa_meyer <- function(newdata, model = NA, trainDI = NA, train = NULL, weight = NA, 
                      variables = "all", CVtest = NULL, CVtrain = NULL, method = "L2", 
                      useWeight = TRUE, LPD = FALSE, maxLPD = 1, indices = FALSE, 
                      verbose = TRUE, parallel = FALSE, ncores = 1) 
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

  required_packages <- c("future", "foreach", "doFuture", "doParallel", "dplyr", "caret", "CAST")
  install_and_load_packages(required_packages)
  
  # Iniciar temporizador
  start_time <- Sys.time()
  if (verbose) message("Iniciando a execução da função...")
  
  options(future.globals.maxSize = 2 * 1024^3)
  
  .knnindexfun <- function(point, reference, method, S_inv = NULL, maxLPD = maxLPD) {
    if (method == "L2") {
      return(FNN::knnx.index(reference, point, k = maxLPD))
    } else if (method == "MD") {
      stop("MD currently not implemented for LPD")
    }
  }

  .knndistfun <- function(point, reference, method, S_inv = NULL, maxLPD = maxLPD) {
    if (method == "L2") {
      return(FNN::knnx.dist(reference, point, k = maxLPD))
    } else if (method == "MD") {
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
      message("Nenhum trainDI fornecido. Criando trainDI...")
    }
    trainDI <- trainDI(model, train, variables, weight, CVtest, CVtrain, method, useWeight, LPD, verbose)
  }
  if (calc_LPD == TRUE) {
    trainDI$maxLPD <- maxLPD
  }
  if (any(trainDI$variables %in% names(newdata) == FALSE)) {
    if (leading_digit) {
      stop("Os nomes dos novos dados começam com dígitos iniciais. Adicionar automaticamente 'X' resulta em nomes incompatíveis com os dados de treino no modelo")
    }
    stop("Os nomes dos novos dados não correspondem aos nomes dos dados de treino no modelo")
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
  newdata <- newdata[, na.omit(match(trainDI$variables, names(newdata))), drop = FALSE]
  catvars <- trainDI$catvars
  if (!inherits(catvars, "error") & length(catvars) > 0) {
    for (catvar in catvars) {
      trainDI$train[, catvar] <- droplevels(trainDI$train[, catvar])
      newdata[, catvar] <- factor(newdata[, catvar])
      newdata[!newdata[, catvar] %in% unique(trainDI$train[, catvar]), catvar] <- NA
      newdata[, catvar] <- droplevels(newdata[, catvar])
      dvi_train <- predict(caret::dummyVars(paste0("~", catvar), data = trainDI$train), trainDI$train)
      dvi_newdata <- predict(caret::dummyVars(paste0("~", catvar), data = trainDI$train), newdata)
      dvi_newdata[is.na(newdata[, catvar]), ] <- 0
      trainDI$train <- data.frame(trainDI$train, dvi_train)
      newdata <- data.frame(newdata, dvi_newdata)
    }
    newdata <- newdata[, -which(names(newdata) %in% catvars)]
    trainDI$train <- trainDI$train[, -which(names(trainDI$train) %in% catvars)]
  }
  newdata <- scale(newdata, center = trainDI$scaleparam$`scaled:center`, scale = trainDI$scaleparam$`scaled:scale`)
  if (!inherits(trainDI$weight, "error")) {
    tmpnames <- names(newdata)
    newdata <- sapply(1:ncol(newdata), function(x) {
      newdata[, x] * unlist(trainDI$weight[x])
    })
    names(newdata) <- tmpnames
  }
  train_scaled <- scale(trainDI$train, center = trainDI$scaleparam$`scaled:center`, scale = trainDI$scaleparam$`scaled:scale`)
  train_scaled <- sapply(1:ncol(train_scaled), function(x) {
    train_scaled[, x] * unlist(trainDI$weight[x])
  })
  okrows <- which(apply(newdata, 1, function(x) all(!is.na(x))))
  newdataCC <- newdata[okrows, , drop = F]
  if (method == "MD") {
    if (dim(train_scaled)[2] == 1) {
      S <- matrix(stats::var(train_scaled), 1, 1)
      newdataCC <- as.matrix(newdataCC, ncol = 1)
    } else {
      S <- stats::cov(train_scaled)
    }
    detS <- try(det(S), silent = T)
    if (inherits(detS, "try-error")) {
      stop("Covariância não é invertível.")
    }
    S_inv <- try(solve(S), silent = T)
    if (inherits(S_inv, "try-error")) {
      stop("Covariância não é invertível.")
    }
  } else {
    S_inv <- NULL
  }
  if (calc_LPD == FALSE) {
    mindist <- rep(NA, nrow(newdata))
    mindist[okrows] <- apply(.knndistfun(newdataCC, train_scaled, method, S_inv = S_inv), 1, min)
    if (inherits(out, "SpatRaster")) {
      outvect <- rep(NA, ncell(out))
      outvect[okrows] <- mindist[okrows]
      outvect <- as.matrix(outvect, ncol = 1)
      outvect <- terra::setValues(out, outvect)
    } else {
      outvect <- mindist
    }
  }
  if (calc_LPD == TRUE) {
    if (parallel == TRUE) {
      n_cores <- ncores
      if (verbose) {
        message(paste0("Computando LPD dos novos dados usando ", n_cores, " núcleos em paralelo"))
      }
      plan(multisession, workers = n_cores)
      disttrain <- future.apply::future_sapply(1:dim(newdataCC)[1], function(y) .knndistfun(newdataCC[y, , drop = F], train_scaled, method, S_inv = S_inv), future.seed = TRUE)
    } else {
      if (verbose) {
        message("Computando LPD dos novos dados...")
      }
      disttrain <- sapply(1:dim(newdataCC)[1], function(y) .knndistfun(newdataCC[y, , drop = F], train_scaled, method, S_inv = S_inv))
    }
    if (verbose) message("LPD calculado. Continuando com a execução...")
    if (maxLPD == 1) {
      mindist <- rep(NA, nrow(newdata))
      mindist[okrows] <- apply(disttrain, 2, min)
      if (inherits(out, "SpatRaster")) {
        outvect <- rep(NA, ncell(out))
        outvect[okrows] <- mindist[okrows]
        outvect <- as.matrix(outvect, ncol = 1)
        outvect <- terra::setValues(out, outvect)
      } else {
        outvect <- mindist
      }
    } else {
      lpdout <- sapply(1:dim(disttrain)[2], function(x) mean(sort(disttrain[, x])[1:maxLPD]))
      if (inherits(out, "SpatRaster")) {
        outvect <- rep(NA, ncell(out))
        outvect[okrows] <- lpdout
        outvect <- as.matrix(outvect, ncol = 1)
        outvect <- terra::setValues(out, outvect)
      } else {
        outvect <- lpdout
      }
    }
  }
  aoa <- outvect
  AOA <- if (inherits(out, "SpatRaster")) {
    outvect <= trainDI$threshold
  } else {
    outvect <= trainDI$threshold
  }
  out <- list(DI = outvect, AOA = AOA, parameters = trainDI)
  if (as_stars) {
    if (!requireNamespace("stars", quietly = TRUE)) 
      stop("package stars required: install that first")
    out$DI <- methods::as(out$DI, "Raster")
    out$DI <- stars::st_as_stars(out$DI)
    out$AOA <- methods::as(out$AOA, "Raster")
    out$AOA <- stars::st_as_stars(out$AOA)
  }
  
  # Registrar o tempo final
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  if (verbose) message(paste("Tempo total de execução:", duration))
  
  if (verbose) message("Execução concluída.")
  return(out)
}
