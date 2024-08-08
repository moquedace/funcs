crop_mask_project <- function(rst,
                              vct,
                              method = "cubicspline",
                              threads = TRUE,
                              output_dir,
                              crs = c("EPSG:4326", "ESRI:54052")) {
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  # Pacotes necessários
  required_packages <- c("terra", "dplyr", "janitor")
  invisible(install_and_load_packages(required_packages))
  
  # Função para validar inputs
  validate_inputs <- function(rst, vct, output_dir, crs) {
    if (!file.exists(rst)) {
      stop("O arquivo raster não existe: ", rst)
    }
    if (!file.exists(vct)) {
      stop("O arquivo vetor (vct) não existe: ", vct)
    }
    if (!is.character(crs) || length(crs) == 0) {
      stop("O parâmetro 'crs' deve ser um vetor de strings não vazio")
    }
  }
  
  validate_inputs(rst, vct, output_dir, crs)
  
  # Verificar e criar diretório de saída se necessário
  if (!file.exists(output_dir)) {
    message("O diretório de saída não existe. Criando: ", output_dir)
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Carregar o raster
  message("Carregando o raster...")
  r <- tryCatch({
    terra::rast(rst)
  }, error = function(e) {
    stop("Erro ao carregar o raster: ", e$message)
  })
  
  # Carregar o vetor
  message("Carregando o vetor...")
  v <- tryCatch({
    terra::vect(vct)
  }, error = function(e) {
    stop("Erro ao carregar o vetor: ", e$message)
  })
  
  # Processamento de CRS (primeiro reprojetar)
  for (i in seq_along(crs)) {
    message("Processando CRS: ", crs[i])
    
    current_crs <- paste0(terra::crs(r, proj = FALSE, describe = TRUE, parse = FALSE)$authority, ":",
                          terra::crs(r, proj = FALSE, describe = TRUE, parse = FALSE)$code)
    
    if (current_crs != crs[i]) {
      message("Reprojetando raster para ", crs[i])
      r_projected <- tryCatch({
        r %>% terra::project(y = crs[i], method = method, threads = threads)
      }, error = function(e) {
        stop("Erro ao reprojetar o raster: ", e$message)
      })
    } else {
      r_projected <- r
    }
    
    # Aplicar crop e mask ao raster reprojetado
    message("Aplicando recorte e máscara...")
    
    v <- project(v, y = crs[i])
    
    r_crop_mask <- tryCatch({
      r_projected %>% terra::crop(v, mask = TRUE, overwrite = TRUE)
    }, error = function(e) {
      stop("Erro ao recortar e mascarar o raster: ", e$message)
    })
    
    # Criar subdiretório de saída para cada CRS, se necessário
    outdir_run <- file.path(output_dir, janitor::make_clean_names(crs[i]))
    if (!file.exists(outdir_run)) {
      message("Criando subdiretório para CRS: ", outdir_run)
      dir.create(outdir_run, recursive = TRUE)
    }
    
    # Salvar o raster reprojetado e recortado
    outfile <- file.path(outdir_run, paste0(names(r_crop_mask), ".tif"))
    tryCatch({
      writeRaster(r_crop_mask, overwrite = TRUE, filename = outfile, gdal = "COMPRESS=LZW")
      message("Raster salvo em: ", outfile)
    }, error = function(e) {
      stop("Erro ao salvar o raster: ", e$message)
    })
  }
  
  message("Processamento concluído.")
}
