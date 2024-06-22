tile_raster <- function(rst, rows, cols, outdir, graph = TRUE,
                        custom_crs = NULL, extend = TRUE,
                        parallel = TRUE, n_cores = 4) {
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    invisible(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("terra", "dplyr", "future.apply", "future", "furrr", "purrr")
  install_and_load_packages(required_packages)
  
  # Verificação de parâmetros de entrada
  if (!inherits(rst, "SpatRaster")) stop("O parâmetro 'rst' deve ser um objeto 'SpatRaster'.")
  if (!is.numeric(rows) || rows <= 0) stop("O parâmetro 'rows' deve ser um número positivo.")
  if (!is.numeric(cols) || cols <= 0) stop("O parâmetro 'cols' deve ser um número positivo.")
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
    message("Diretório de saída criado: ", outdir)
  }
  
  message("Iniciando a criação dos tiles...")
  start_time <- Sys.time()
  
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
  message("CRS configurado: ", crs_str)
  
  # Configuração dos parâmetros dos tiles
  message("Configurando os parâmetros dos tiles...")
  r_tile <- rast(ncols = cols, nrows = rows, nlyrs = nlyr(rst), crs = crs_str, extent = ext(rst))
  
  # Extensão dos tiles
  message("Calculando as extensões dos tiles...")
  blocks <- terra::getTileExtents(rst, r_tile, extend = extend) %>%
    apply(1, ext) %>%
    lapply(vect) %>%
    lapply(`crs<-`, crs_str)
  message("Extensões dos tiles calculadas.")
  
  # Plotagem gráfica
  if (graph) {
    message("Plotando gráfico dos tiles...")
    plot(rst[[1]])
    invisible(lapply(blocks, plot, add = TRUE))
    message("Gráfico dos tiles plotado.")
  }
  
  nm_tiles <- paste0(outdir, "/tile_", seq_along(blocks), ".tif")
  
  if (parallel) {
    message("Configurando paralelismo com ", n_cores, " núcleos...")
    crop_mask_write <- function(v, file, r) {
      raster <- rast(r)
      vetor <- vect(v)
      
      r_crop <- crop(x = raster, y = vetor)
      r_mask <- mask(x = r_crop, mask = vetor)
      
      writeRaster(r_mask, filename = file, gdal = "COMPRESS=LZW", overwrite = TRUE)
      message("Tile salvo: ", file)
      gc()
      return(file)
    }
    
    # Configuração do paralelismo
    plan(multisession, workers = n_cores)
    rst_file <- tempfile(fileext = ".tif")
    writeRaster(rst, filename = rst_file, gdal = "COMPRESS=LZW", overwrite = TRUE)
    
    blocks_file <- lapply(blocks, function(v) {
      v_file <- tempfile(fileext = ".gpkg")
      writeVector(x = v, filename = v_file)
      v_file
    })
    
    message("Processamento dos tiles em paralelo iniciado...")
    tile_files <- future_mapply(crop_mask_write, v = blocks_file, file = nm_tiles,
                                MoreArgs = list(r = rst_file),
                                future.packages = "terra", SIMPLIFY = TRUE)
    message("Processamento dos tiles em paralelo concluído.")
  } else {
    message("Processamento dos tiles em modo sequencial iniciado...")
    crop_mask_write <- function(v, r, file) {
      
      r_crop <- crop(x = r, y = v)
      r_mask <- mask(x = r_crop, mask = v)
      
      writeRaster(r_mask, filename = file, gdal = "COMPRESS=LZW", overwrite = TRUE)
      message("Tile salvo: ", file)
      gc()
      return(file)
    }
    
    tile_files <- mapply(crop_mask_write, v = blocks, file = nm_tiles,
                         MoreArgs = list(r = rst), SIMPLIFY = TRUE)
    message("Processamento dos tiles em modo sequencial concluído.")
  }
  
  end_time <- Sys.time()
  duration <- end_time - start_time
  message("Tiles criados com sucesso no diretório: ", outdir)
  message("Tempo total para criar os tiles: ", round(duration, 4), " ", units(duration))
  
  return(tile_files)
}
