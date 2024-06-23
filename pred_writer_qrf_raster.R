
pred_writer_qrf_raster <- function(model = NULL,
                                   varfact = NULL,
                                   tile = NULL,
                                   what = list(quantile_0.25 = 0.25,
                                               quantile_0.5 = 0.5,
                                               quantile_0.75 = 0.75,
                                               mean = mean,
                                               sd = sd),
                                   custom_crs = NULL,
                                   outdir = "./predict",
                                   file = "tile_1",
                                   plot_results = FALSE) {
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    invisible(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("randomForest", "data.table", "microbenchmark",
                         "terra", "dplyr", "caret")
  install_and_load_packages(required_packages)
  
  start_time <- Sys.time()
  message("Iniciando a função.")
  
  # Verificação do argumento 'model'
  if (!inherits(model, c("train", "train.formula"))) {
    stop("O argumento 'model' precisa ser um objeto treinado com o pacote caret.")
  }
  
  # Verificação do argumento 'tile'
  if (is.null(tile)) {
    stop("O argumento 'tile' não pode ser NULL.")
  }
  
  # Verificação do argumento 'what'
  if (!is.list(what) || length(what) == 0) {
    stop("O argumento 'what' precisa ser uma lista não vazia.")
  }
  
  message("Carregando o raster do arquivo especificado...")
  rst <- try(rast(tile), silent = TRUE)
  if (inherits(rst, "try-error")) {
    stop("Não foi possível carregar o raster do arquivo especificado.")
  }
  
  # Verificação do diretório de saída
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
    message("Diretório de saída criado: ", outdir)
  }
  
  message("Extraindo o nome das variáveis...")
  varnames <- try(names(model$trainingData %>% select(-.outcome)), silent = TRUE)
  if (inherits(varnames, "try-error")) {
    stop("Não foi possível extrair os nomes das variáveis do modelo.")
  }
  
  object <- model$finalModel
  
  message("Selecionando camadas e convertendo o raster para data.frame...")
  df_rst <- try(as.data.table(as.data.frame(subset(rst, varnames), xy = TRUE, na.rm = TRUE)), silent = TRUE)
  if (inherits(df_rst, "try-error")) {
    stop("Não foi possível converter o raster para data.frame.")
  }
  
  gc()
  
  message("Verificando se o data.frame contém linhas válidas para a predição...")
  if (nrow(df_rst) == 0) {
    stop("O raster especificado não contém dados válidos.")
  }
  
  if (!is.null(varfact)) {
    message("Convertendo as variáveis em fator...")
    df_rst <- df_rst %>%
      mutate(across(contains(varfact), as.factor))
  }
  
  xy <- df_rst %>% select(x, y)
  newdata <- df_rst %>% select(-x, -y)
  
  gc()
  
  message("Configurando o sistema de referência de coordenadas (CRS)...")
  crs_str <- if (is.null(custom_crs)) {
    crs_info <- crs(rst, describe = TRUE)
    if (!is.null(crs_info$authority) && !is.null(crs_info$code)) {
      paste0(crs_info$authority, ":", crs_info$code)
    } else if (!is.null(crs_info$proj4)) {
      crs_info$proj4
    } else {
      warning("Não foi possível obter o CRS do raster. Usando CRS padrão WGS84.")
      "EPSG:4326"
    }
  } else {
    custom_crs
  }
  message("CRS configurado: ", crs_str)
  
  class(object) <- "randomForest"
  message("Classe do objeto definida como 'randomForest'.")
  
  if (is.null(newdata)) {
    if (is.null(object[["valuesOOB"]])) {
      stop("Precisa ajustar com a opção keep.inbag=TRUE para obter observações out-of-bag")
    }
    valuesPredict <- object[["valuesOOB"]]
    message("Usando valores fora da bolsa (out-of-bag).")
  } else {
    message("Prevendo valores para os novos dados...")
    predictNodes <- try(attr(predict(object, newdata = newdata, nodes = TRUE), "nodes"), silent = TRUE)
    if (inherits(predictNodes, "try-error")) {
      stop("Não foi possível prever os nós para os novos dados.")
    }
    rownames(predictNodes) <- NULL
    valuesPredict <- matrix(0, nrow(predictNodes), ncol(predictNodes))
    ntree <- ncol(object[["valuesNodes"]])
    for (tree in 1:ntree) {
      valuesPredict[, tree] <- object[["valuesNodes"]][predictNodes[, tree], tree]
    }
    message("Valores preditos para novos dados.")
  }
  
  process_item <- function(item, item_name) {
    if (is.function(item)) {
      result <- apply(valuesPredict, 1, item)
      if (is.null(dim(result))) {
        result <- matrix(result, ncol = 1)
        colnames(result) <- item_name
      } else {
        colnames(result) <- paste(item_name, 1:ncol(result), sep = "_")
      }
    } else {
      if (!is.numeric(item)) 
        stop("`what` precisa ser uma função ou um vetor de quantis")
      if (min(item) < 0) 
        stop("Se `what` especifica quantis, o valor mínimo precisa ser não-negativo")
      if (max(item) > 1) 
        stop("Se `what` especifica quantis, o valor máximo não pode exceder 1")
      result <- apply(valuesPredict, 1, quantile, item, na.rm = TRUE)
      if (length(item) == 1) {
        result <- matrix(result, ncol = 1)
        colnames(result) <- paste("quantile_", item, sep = "")
      } else {
        result <- t(result)
        colnames(result) <- paste("quantile_", item, sep = "")
      }
    }
    return(result)
  }
  
  message("Processando itens em 'what'...")
  results_list <- mapply(process_item, what, names(what), SIMPLIFY = FALSE)
  
  message("Combinando os resultados e criando raster...")
  combined_results <- lapply(seq_along(results_list), function(i) {
    result <- results_list[[i]]
    colnames(result) <- if (is.null(names(what)[i])) colnames(result) else names(what)[i]
    cbind(xy, result)
  })
  
  r_res <- lapply(combined_results, function(df) {
    as.data.table(df) %>%
      relocate(c(x, y)) %>%
      rast(type = "xyz", crs = crs_str)
  }) %>% rast()
  
    filewrite <- paste0(outdir, "/", file, "_", names(r_res), ".tif")
  
    gc()
    
  message("Salvando rasters no disco...")
  for (i in 1:nlyr(r_res)) {
    r_what <- r_res[[i]]
    writeRaster(r_what, filename = filewrite[i], gdal = "COMPRESS=LZW", overwrite = TRUE)
  }
  
  if (plot_results) {
    message("Plotando os resultados...")
    plot(r_res)
  }
  
  end_time <- Sys.time()
  total_time <- end_time - start_time
  message("Função concluída. Tempo total de execução: ", round(total_time, 2), " ", units(total_time))
  
  return(filewrite)
  gc()
}

