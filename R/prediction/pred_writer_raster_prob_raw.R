pred_writer_raster_prob_raw <- function(model = NULL,
                                        varfact = NULL,
                                        tile = NULL,
                                        custom_crs = NULL,
                                        outdir = "./predict",
                                        file = "tile_1",
                                        type_predict = c("prob", "raw"),
                                        plot_results = FALSE,
                                        n_divisions = 5) {
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("randomForest", "data.table", "microbenchmark",
                         "terra", "dplyr", "caret")
  install_and_load_packages(required_packages)
  
  start_time <- Sys.time()
  message("Iniciando a função.")
  
  # Validação dos argumentos
  if (!inherits(model, c("train", "train.formula"))) {
    stop("O argumento 'model' precisa ser um objeto treinado com o pacote caret.")
  }
  if (is.null(tile)) stop("O argumento 'tile' não pode ser NULL.")
  
  # Carregamento do raster
  rst <- try(trim(rast(tile)), silent = TRUE)
  if (inherits(rst, "try-error")) stop("Não foi possível carregar o raster do arquivo especificado.")
  
  # Extração das variáveis e validação de variáveis ausentes
  varnames <- names(model$trainingData %>% select(-.outcome))
  missing_vars <- setdiff(varnames, names(rst))
  if (length(missing_vars) > 0) {
    stop("O raster especificado está faltando as seguintes variáveis: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Validação de argumentos
  type_predict <- match.arg(type_predict, choices = c("prob", "raw"), several.ok = TRUE)
  
  # Verificação do diretório de saída
  if (!dir.exists(outdir)) {
    if (!dir.create(outdir, recursive = TRUE)) {
      stop("Não foi possível criar o diretório de saída: ", outdir)
    }
  }
  
  # Extração das variáveis do raster
  df_rst <- as.data.table(as.data.frame(subset(rst, varnames), xy = TRUE, na.rm = TRUE))
  if (nrow(df_rst) == 0) stop("O raster especificado não contém dados válidos.")
  
  if (!is.null(varfact)) {
    df_rst <- df_rst %>% mutate(across(contains(varfact), as.factor))
  }
  
  xy <- df_rst %>% select(x, y)
  newdata <- df_rst %>% select(-x, -y)
  
  # CRS
  crs_str <- if (is.null(custom_crs)) "EPSG:4326" else custom_crs
  
  # Função auxiliar para predição em chunks
  predict_by_chunks <- function(model, newdata, type, n_divisions) {
    chunk_size <- ceiling(nrow(newdata) / n_divisions)
    data_chunks <- split(newdata, ceiling(seq_len(nrow(newdata)) / chunk_size))
    
    predictions_list <- list()
    for (i in seq_along(data_chunks)) {
      message(sprintf("Processando chunk %d de %d...", i, length(data_chunks)))
      predictions_list[[i]] <- predict(model, newdata = data_chunks[[i]], type = type)
    }
    
    if (type == "raw") {
      predictions <- unlist(predictions_list, use.names = FALSE)
    } else if (type == "prob") {
      predictions <- do.call(rbind, predictions_list)
    }
    return(predictions)
  }
  
  # Lista para armazenar os resultados
  results <- list()
  
  # Execução da predição para cada tipo
  message("Executando predições...")
  for (t in type_predict) {
    if (t == "prob") {
      message(paste("Realizando predição do tipo:", t))
      r_res <- predict_by_chunks(model, newdata, type = t, n_divisions = n_divisions)
      for (class_name in colnames(r_res)) {
        class_raster <- rast(cbind(xy, r_res[, class_name, drop = FALSE]), type = "xyz", crs = crs_str)
        results[[paste0(t, "_", class_name)]] <- class_raster
      }
    } else {
      message(paste("Realizando predição do tipo:", t))
      code_df <- data.frame(code = 1:length(model$levels),
                            class_name = model$levels)
      r_res <- predict_by_chunks(model, newdata, type = t, n_divisions = n_divisions) %>% 
        data.frame(class_name = .) %>% 
        left_join(code_df, by = "class_name") %>% 
        select(code)
      r_raster <- rast(cbind(xy, r_res), type = "xyz", crs = crs_str)
      levels(r_raster) <- code_df
      results[[t]] <- r_raster
    }
  }
  
  # Salvando os resultados
  message("Salvando rasters...")
  file_paths <- c()
  for (t in names(results)) {
    file_path <- paste0(outdir, "/", file, "_", t, ".tif")
    writeRaster(results[[t]], filename = file_path, gdal = "COMPRESS=LZW", overwrite = TRUE)
    file_paths <- c(file_paths, file_path)
  }
  
  # Plotando, se necessário
  if (plot_results) {
    for (t in names(results)) {
      plot(results[[t]], main = paste("Resultado:", t))
    }
  }
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  message("Função concluída. Tempo total de execução: ",
          round(duration, 2), " ",
          units(duration), ".")
  
  return(file_paths)
}
