
rst_class_name_max <- function(rst, custom_crs = NULL, plot_map = TRUE) {
  start_time <- Sys.time()
  message("Iniciando a função rst_class_name_max...")
  
  required_packages <- c("dplyr", "terra", "stringr")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
  
  if (length(missing_packages) > 0) {
    message("Instalando pacotes ausentes: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
    message("Pacotes instalados com sucesso.")
  }
  
  message("Carregando pacotes necessários...")
  lapply(required_packages, library, character.only = TRUE)
  
  message("Verificando o formato do raster...")
  if (inherits(rst, c("RasterStack", "RasterLayer"))) {
    rst <- rast(rst)
    message("Raster convertido para SpatRaster.")
  } else if (!inherits(rst, "SpatRaster")) {
    stop("Erro: o arquivo deve estar no formato RasterStack, RasterLayer ou SpatRaster.")
  } else {
    message("Raster já está no formato SpatRaster.")
  }
  
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
  crs(rst) <- crs_str
  message("CRS configurado como: ", crs_str)
  
  message("Obtendo nomes das camadas do raster...")
  soils_name <- sort(names(rst))
  df_code_clas <- data.frame(code_classe_dom = seq_along(soils_name),
                             classe_dom = soils_name)

  gc()
  
  message("Convertendo raster para DataFrame e processando dados...")
  df <- as.data.frame(rst, xy = TRUE, na.rm = TRUE)
  classes_name <- df %>%
    select(-x, -y) %>%
    mutate(classe_dom = names(.)[apply(., 1, which.max)]) %>%
    select(classe_dom)

  gc()
  
  message("Criando raster com a classe dominante...")
  r_dom <- df %>%
    select(x, y) %>%
    cbind(classes_name) %>%
    left_join(df_code_clas, by = "classe_dom") %>%
    select(x, y, code_classe_dom) %>%
    rast(type = "xyz", crs = crs_str)

  gc()
  
  levels(r_dom) <- list(df_code_clas)
  
  if (plot_map) {
    message("Plotando o raster de classe dominante...")
    plot(r_dom, main = "Classe Dominante do Raster")
  }
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  message("Função executada com sucesso em ", round(elapsed_time, 2), " ",
          units(elapsed_time), "...")
  
  return(r_dom)
}
