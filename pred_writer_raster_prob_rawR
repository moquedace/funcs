pred_writer_raster_prob_raw <- function(model = NULL,
                               varfact = NULL,
                               tile = NULL,
                               custom_crs = NULL,
                               outdir = "./predict",
                               file = "tile_1",
                               type_predict = c("prob", "raw"),
                               plot_results = FALSE) {
  
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
  
  # Verificação do diretório de saída
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Extração das variáveis
  varnames <- names(model$trainingData %>% select(-.outcome))
  df_rst <- as.data.table(as.data.frame(subset(rst, varnames), xy = TRUE, na.rm = TRUE))
  if (nrow(df_rst) == 0) stop("O raster especificado não contém dados válidos.")
  
  if (!is.null(varfact)) {
    df_rst <- df_rst %>% mutate(across(contains(varfact), as.factor))
  }
  
  xy <- df_rst %>% select(x, y)
  newdata <- df_rst %>% select(-x, -y)
  
  # CRS
  crs_str <- if (is.null(custom_crs)) "EPSG:4326" else custom_crs
  
  # Verificação do type_predict
  valid_types <- c("raw", "prob")
  type_predict <- intersect(type_predict, valid_types)
  if (length(type_predict) == 0) stop("Argumento 'type_predict' inválido. Use 'raw' ou 'prob'.")
  
  # Lista para armazenar os resultados
  results <- list()
  
  # Execução da predição para cada tipo
  message("Executando predições...")
  for (t in type_predict) {
    if (t == "prob") {
      message(paste("Realizando predição do tipo:", t))
      r_res <- predict(model, newdata, type = t)
      r_raster <- rast(cbind(xy, r_res), type = "xyz", crs = crs_str)
      results[[t]] <- r_raster
    } else {
      message(paste("Realizando predição do tipo:", t))
      code_df <- data.frame(code = 1:length(model$levels),
                            class_name = model$levels)
      r_res <- predict(model, newdata, type = t) %>% 
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
  message("Função concluída. Tempo total de execução: ", round(end_time - start_time, 2), " segundos.")
  
  return(file_paths)
}
