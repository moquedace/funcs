check_tiles <- function(tile_paths, cores = detectCores() - 1, 
                        outdir = "./", filerdata = "tiles_subset.rdata") {
  
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("terra", "furrr", "stringr",
                         "future", "parallel", "progressr")
  install_and_load_packages(required_packages)
  
  handlers("txtprogressbar")
  
  if (length(tile_paths) == 0) {
    stop("Erro: A lista de tiles está vazia. Forneça endereços válidos.")
  }
  
  if (!all(file.exists(tile_paths))) {
    stop("Erro: Um ou mais arquivos fornecidos não existem. Verifique os caminhos.")
  }
  
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
    message(paste("O diretório", outdir, "não existia e foi criado."))
  }
  
  plan(multisession, workers = cores)
  handlers(global = TRUE)
  
  check_tile <- function(tile_path, index, progressor = NULL) {
    tryCatch({
      if (!is.null(progressor)) progressor()
      d <- rast(tile_path)[[1]] %>% 
        as.data.frame(xy = FALSE, na.rm = TRUE)
      result <- ifelse(nrow(d) == 0, FALSE, TRUE)
      gc()
      return(result)
    }, error = function(e) {
      message(paste("Erro ao processar o tile", index, ":", e$message))
      return(NA)
    })
  }
  
  message("Iniciando processamento dos tiles...")
  tt <- Sys.time()
  
  with_progress({
    p <- progressor(along = tile_paths)
    tiles_subset <- future_map_lgl(seq_along(tile_paths), 
                                   ~check_tile(tile_paths[.x], .x, progressor = p))
  })
  
  ttf <- Sys.time() - tt
  message(paste("Processamento concluído em", round(ttf, 2), units(ttf)))
  
  save_path <- file.path(outdir, filerdata)
  save(tiles_subset, file = save_path)
  message(paste("Resultados salvos em:", save_path))
  
  plan(sequential)
  return(tiles_subset)
}
