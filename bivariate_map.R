colmat <- function(nbreaks = 3, breakstyle = "quantile",
                   upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = FALSE) {
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("tidyverse", "classInt", "ggplot2")
  install_and_load_packages(required_packages)
  
  cat("Pacotes necessários carregados.\n")
  
  # Verificações de argumentos
  if (!is.numeric(nbreaks) || nbreaks < 1 || nbreaks != round(nbreaks)) {
    stop("Erro: 'nbreaks' deve ser um número inteiro positivo.")
  }
  
  if (!is.character(breakstyle) || length(breakstyle) != 1) {
    stop("Erro: 'breakstyle' deve ser uma string.")
  }
  
  color_arguments <- c(upperleft, upperright, bottomleft, bottomright)
  if (any(!sapply(color_arguments, function(col) grepl("^#[A-Fa-f0-9]{6}$", col)))) {
    stop("Erro: As cores devem estar no formato hexadecimal (por exemplo, '#RRGGBB').")
  }
  
  if (!is.character(xlab) || length(xlab) != 1) {
    stop("Erro: 'xlab' deve ser uma string.")
  }
  
  if (!is.character(ylab) || length(ylab) != 1) {
    stop("Erro: 'ylab' deve ser uma string.")
  }
  
  if (!is.logical(plotLeg)) {
    stop("Erro: 'plotLeg' deve ser TRUE ou FALSE.")
  }
  
  if (!is.logical(saveLeg)) {
    stop("Erro: 'saveLeg' deve ser TRUE ou FALSE.")
  }
  
  cat("Todos os argumentos foram verificados e são válidos.\n")
  
  # Verifica e ajusta o estilo de intervalo de classes
  if (breakstyle == "sd") {
    warning("Estilo de intervalos 'sd' não pode ser usado.\nEle nem sempre retorna o número correto de intervalos.\nVeja classInt::classIntervals() para detalhes.\nResetando para 'quantile'",
            call. = FALSE, immediate. = FALSE)
    breakstyle <- "quantile"
  }
  
  # Início do temporizador
  start_time <- Sys.time()
  
  # Cria dados e classes para a paleta de cores
  cat("Criando dados e classes para a paleta de cores...\n")
  my.data <- seq(0, 1, .01)
  my.class <- classInt::classIntervals(my.data, n = nbreaks, style = breakstyle)
  
  # Cria paletas de cores para as bordas
  cat("Criando paletas de cores para as bordas...\n")
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  
  # Inicializa a matriz de cores
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  
  # Preenche a matriz de cores
  cat("Preenchendo a matriz de cores...\n")
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  cat("Matriz de cores preenchida.\n")
  
  # Converte a matriz de cores para um data frame
  cat("Convertendo a matriz de cores para um data frame...\n")
  col.matrix.plot <- col.matrix %>%
    as.data.frame() %>%
    mutate("Y" = row_number()) %>%
    pivot_longer(cols = -Y, names_to = "X", values_to = "HEXCode") %>%
    mutate("X" = as.integer(sub("V", "", X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(Y)) %>%
    dplyr::select(-4) %>%
    mutate("Y" = rep(seq(1, nbreaks), each = nbreaks),
           "X" = rep(seq(1, nbreaks), times = nbreaks),
           "UID" = row_number())
  cat("Matriz de cores convertida para data frame.\n")
  
  # Plota a legenda se necessário
  if (plotLeg) {
    cat("Plotando a legenda...\n")
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_tile() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 12, colour = "black", hjust = 0.5, vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~ symbol("\256"))) +
      ylab(bquote(.(ylab) ~ symbol("\256")))
    
    print(p)
    assign("BivLegend", p, pos = .GlobalEnv)
    cat("Legenda plotada e armazenada como 'BivLegend' no ambiente global.\n")
  }
  
  # Salva a legenda como PDF se necessário
  if (saveLeg) {
    cat("Salvando a legenda como PDF...\n")
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in", dpi = 300)
    cat("Legenda salva como 'bivLegend.pdf'.\n")
  }
  
  # Ajusta a matriz de cores com base nos intervalos
  cat("Ajustando a matriz de cores com base nos intervalos...\n")
  seqs <- seq(0, 100, 100 / nbreaks)
  seqs[1] <- 1
  col.matrix <- col.matrix[seqs, seqs]
  cat("Matriz de cores ajustada.\n")
  
  # Adiciona atributos à matriz de cores
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  
  # Fim do temporizador
  end_time <- Sys.time()
  total_time <- end_time - start_time
  cat("Processo concluído em", total_time, "segundos.\n")
  
  return(col.matrix)
}



bivariate_map <- function(rasterx, rastery, colourmatrix = col.matrix,
                          export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx)),
                          ncores = parallel::detectCores() - 1) {
  
  # Função para instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    invisible(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("terra", "classInt", "future.apply", "parallel")
  install_and_load_packages(required_packages)
  
  cat("Pacotes necessários carregados.\n")
  
  # Função auxiliar para converter fatores em números
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  
  # Verificações de argumentos
  if (!inherits(rasterx, "SpatRaster")) {
    stop("Erro: 'rasterx' deve ser um objeto SpatRaster.")
  }
  
  if (!inherits(rastery, "SpatRaster")) {
    stop("Erro: 'rastery' deve ser um objeto SpatRaster.")
  }
  
  if (!is.matrix(colourmatrix)) {
    stop("Erro: 'colourmatrix' deve ser uma matriz.")
  }
  
  if (!is.logical(export.colour.matrix)) {
    stop("Erro: 'export.colour.matrix' deve ser TRUE ou FALSE.")
  }
  
  if (!is.character(outname) || length(outname) != 1) {
    stop("Erro: 'outname' deve ser uma string.")
  }
  
  if (!is.numeric(ncores) || ncores < 1 || ncores != round(ncores)) {
    stop("Erro: 'ncores' deve ser um número inteiro positivo.")
  }
  
  cat("Todos os argumentos foram verificados e são válidos.\n")
  
  # Inicializa a paralelização
  options(future.globals.maxSize = 300 * 1024^3)
  future::plan(future::multicore, workers = ncores)
  cat("Paralelização inicializada com", ncores, "núcleos.\n")
  
  # Início do temporizador
  start_time <- Sys.time()
  
  # Processa o raster x
  cat("Processando o raster x...\n")
  quanx <- values(rasterx)
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- classInt::classIntervals(quanx, n = attr(colourmatrix, "nbreaks"), style = attr(colourmatrix, "breakstyle"))$brks
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(tempx, quantile <- cut(quanx, breaks = brks, labels = 2:length(brks), include.lowest = TRUE))
  quantr <- data.frame(r1[, 2])
  cat("Raster x processado.\n")
  
  # Processa o raster y
  cat("Processando o raster y...\n")
  quany <- values(rastery)
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brksy <- classInt::classIntervals(quany, n = attr(colourmatrix, "nbreaks"), style = attr(colourmatrix, "breakstyle"))$brks
  brksy[-1] <- brksy[-1] + seq_along(brksy[-1]) * .Machine$double.eps
  r2 <- within(tempy, quantile <- cut(quany, breaks = brksy, labels = 2:length(brksy), include.lowest = TRUE))
  quantr2 <- data.frame(r2[, 2])
  cat("Raster y processado.\n")
  
  # Converte a matriz de cores para valores numéricos
  cat("Convertendo a matriz de cores...\n")
  col.matrix2 <- colourmatrix
  cn <- unique(colourmatrix)
  for (i in 1:length(col.matrix2)) {
    if (is.na(col.matrix2[i])) {
      col.matrix2[i] <- 1
    } else {
      col.matrix2[i] <- which(col.matrix2[i] == cn)[1]
    }
  }
  cat("Matriz de cores convertida.\n")
  
  # Exporta a matriz de cores, se necessário
  if (export.colour.matrix) {
    cat("Exportando a matriz de cores...\n")
    exportCols <- data.frame(rasValue = as.vector(col.matrix2), HEX = as.vector(colourmatrix),
                             t(col2rgb(as.vector(colourmatrix))))
    assign(outname, exportCols, pos = .GlobalEnv)
    cat("Matriz de cores exportada como", outname, ".\n")
  }
  
  # Atribui cores aos valores raster usando paralelização
  cat("Atribuindo cores aos valores raster...\n")
  cols <- future_sapply(1:length(quantr[, 1]), function(i) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    as.numeric(col.matrix2[b, a])
  }, future.packages = c("classInt", "terra"))
  cat("Cores atribuídas.\n")
  
  # Atualiza os valores do raster e retorna
  r <- rasterx
  values(r) <- cols
  
  # Fim do temporizador
  end_time <- Sys.time()
  total_time <- end_time - start_time
  cat("Processo concluído em", total_time, "segundos.\n")
  
  return(r)
}
