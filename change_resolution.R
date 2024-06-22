change_resolution <- function(rst,
                              resolution,
                              method = "cubicspline",
                              threads = TRUE,
                              custom_crs = NULL,
                              integer = FALSE) {
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    invisible(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("terra", "dplyr")
  install_and_load_packages(required_packages)
  
  
  # Verificação inicial do raster de entrada
  message("Verificando o raster de entrada...")
  if (!inherits(rst, "SpatRaster")) stop("O parâmetro 'rst' deve ser um objeto 'SpatRaster'.")
  if (is.null(rst)) stop("O raster de entrada não pode ser nulo.")
  
  # Verificação da resolução
  message("Verificando a resolução...")
  if (any(resolution <= 0))  stop("A resolução deve ser um valor positivo.")
  
  # Verificação se o método de reamostragem é válido
  message("Verificando o método de reamostragem...")
  valid_methods <- c("near", "bilinear", "cubic", "cubicspline", "lanczos", "sum")
  if (!(method %in% valid_methods)) {
    stop("Método de reamostragem inválido. Escolha entre: 'bilinear', 'cubicspline', 'cubic', 'nearest', 'average'.")
  }
  
  # Configuração do CRS
  message("Configurando o sistema de referência de coordenadas (CRS)...")
  crs_str <- if (is.null(custom_crs)) {
    crs_info <- crs(rst, describe = TRUE)
    if (!is.null(crs_info$authority) && !is.null(crs_info$code)) {
      paste0(crs_info$authority, ":", crs_info$code)
    } else if (!is.null(crs_info$proj4)) {
      crs_info$proj4
    } else {
      warning("Não foi possível obter o CRS do raster. Usando CRS padrão WGS84.")
      "EPSG:4326"  # CRS padrão (WGS84) como fallback
    }
  } else {
    custom_crs
  }
  
  # Verificação se o CRS é métrico
  message("Verificando o CRS do raster de entrada...")
  if (is.lonlat(crs_str)) {
    warning("O raster de entrada não possui um CRS com coordenadas métricas. Certifique-se de que a resolução fornecida está em graus e não em metros.")
  }
  
  # Criação do raster base
  r_base <- rast(nlyrs = nlyr(rst), resolution = resolution,
                 crs = crs_str,
                 extent = ext(rst))
  
  # Reamostragem do raster
  message("Realizando a reamostragem...")
  if (integer) {
    rst_res <- resample(rst, r_base, method = method, threads = threads) %>% 
      as.int()
  } else {
    rst_res <- resample(rst, r_base, method = method, threads = threads)
  }
  
  message("Processo de reamostragem concluído.")
  
  return(rst_res)
}
