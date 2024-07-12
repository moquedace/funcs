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
  
  # Pacotes necessários para a função
  required_packages <- c("future", "foreach", "doFuture", "doParallel", "dplyr", "caret", "CAST", "terra", "stars", "FNN", "MASS")
  install_and_load_packages(required_packages)
  
  # Função para encontrar o índice do vizinho mais próximo
  .knnindexfun <- function (point, reference, method, S_inv = NULL, maxLPD = maxLPD) {
    if (method == "L2") {
      return(FNN::knnx.index(reference, point, k = maxLPD))
    } else if (method == "MD") {
      stop("MD currently not implemented for LPD")
    }
  }
  
  # Função para calcular a distância do vizinho mais próximo
  .knndistfun <- function (point, reference, method, S_inv = NULL, maxLPD = maxLPD) {
    if (method == "L2") {
      return(FNN::knnx.dist(reference, point, k = maxLPD))
    } else if (method == "MD") {
      return(t(sapply(1:dim(point)[1], function(y) sort(sapply(1:dim(reference)[1], 
                                                               function(x) sqrt(t(point[y, ] - reference[x, ]) %*% 
                                                                                  S_inv %*% (point[y, ] - reference[x, ]))))[1:maxLPD])))
    }
  }
  
  # Início da medição do tempo
  start_time <- Sys.time()
  if (verbose) {
    message("Iniciando o cálculo de AOA...")
  }

  # Preparação dos dados
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

  # Verificação de maxLPD
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
        stop("maxLPD cannot be bigger than the number of training samples. Define a number between 0 and 1 for a percentage of the number of training samples, or um número inteiro maior que 1 e menor que o número de amostras de treinamento.")
      }
    } else {
      stop("maxLPD must be a number. Define a number between 0 and 1 for a percentage of the number of training samples, or um número inteiro maior que 1 e menor que o número de amostras de treinamento.")
    }
  }

  # Calculo do trainDI se não fornecido
  if (!inherits(trainDI, "trainDI")) {
    if (verbose) {
      message("No trainDI provided. Calculando trainDI...")
    }
    trainDI <- trainDI(model, train, variables, weight, 
                       CVtest, CVtrain, method, useWeight, LPD, verbose)
  }
  if (LPD == TRUE) {
    trainDI$maxLPD <- maxLPD
  }
  
  # Verificação das variáveis correspondentes entre newdata e trainDI
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

  # Normalização dos dados
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

  # Identificação de linhas válidas
  okrows <- which(apply(newdata, 1, function(x) all(!is.na(x))))
  newdataCC <- newdata[okrows, , drop = F]
  if (method == "MD") {
    if (dim(train_scaled)[2] == 1) {
      S <- matrix(stats::var(train_scaled), 1, 1)
      S_inv <- matrix(1/S)
    } else {
      S <- stats::cov(train_scaled)
      S_inv <- MASS::ginv(S)
    }
  }

  # Paralelização opcional
  if (parallel) {
    if (missing(ncores)) {
      ncores <- parallel::detectCores() - 1
    }
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    parallel_flag <- TRUE
  } else {
    parallel_flag <- FALSE
  }

  # Calculo de DI e LPD
  if (parallel_flag) {
    if (verbose) {
      message("Computing DI and LPD (if requested) in parallel...")
    }
    out_list <- foreach::foreach(i = seq_len(nrow(newdata)), .combine = "c", .packages = required_packages) %dopar% {
      if (is.na(newdata[i, ])) {
        return(NA)
      } else {
        LPDs <- .knndistfun(newdata[i, , drop = FALSE], train_scaled, method, S_inv, maxLPD)
        DI <- mean(LPDs)
        if (indices) {
          LPD_index <- .knnindexfun(newdata[i, , drop = FALSE], train_scaled, method, S_inv, maxLPD)
          return(list(DI, LPD_index))
        } else {
          return(DI)
        }
      }
    }
    DI_out <- sapply(out_list, function(x) x[[1]])
    if (indices) {
      index_out <- do.call(rbind, lapply(out_list, function(x) x[[2]]))
    }
  } else {
    if (verbose) {
      message("Computing DI and LPD (if requested)...")
    }
    LPDs <- matrix(NA, nrow(newdata), maxLPD)
    LPDs[okrows, ] <- .knndistfun(newdataCC, train_scaled, method, S_inv, maxLPD)
    DI_out <- rowMeans(LPDs[, 1:maxLPD], na.rm = TRUE)
    if (indices) {
      LPD_index <- matrix(NA, nrow(newdata), maxLPD)
      LPD_index[okrows, ] <- .knnindexfun(newdataCC, train_scaled, method, S_inv, maxLPD)
      index_out <- LPD_index
    }
  }

  # Transformação do resultado
  if (inherits(out, "SpatRaster")) {
    outvect <- rep(NA, nrow(out))
    outvect[okrows] <- DI_out
    terra::values(out) <- outvect
    if (indices) {
      for (i in 1:ncol(index_out)) {
        temp <- out
        outvect[okrows] <- index_out[, i]
        terra::values(temp) <- outvect
        if (i == 1) 
          out <- c(out, temp)
        else out <- c(out, temp)
      }
    }
    names(out) <- if (indices) {
      c("DI", paste0("DI_index", 1:ncol(index_out)))
    } else "DI"
    if (as_stars) {
      out <- stars::st_as_stars(out)
    }
  } else {
    if (indices) {
      out <- data.frame(DI = DI_out, index_out)
      names(out) <- c("DI", paste0("DI_index", 1:ncol(index_out)))
    } else {
      out <- DI_out
    }
  }

  # Finalização e medição do tempo
  if (parallel_flag) {
    parallel::stopCluster(cl)
  }
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  if (verbose) {
    message("Cálculo concluído em ", round(elapsed_time, 2), " segundos.")
  }

  return(out)
}
