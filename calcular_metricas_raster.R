calcular_metricas_raster <- function(df_bloco) {
  # Verificação de pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  # Instala e carrega pacotes necessários
  required_packages <- c("dplyr")
  install_and_load_packages(required_packages)
  
  # Verificação de entrada
  if (!is.data.frame(df_bloco)) {
    stop("O argumento 'df_bloco' deve ser um data.frame.")
  }
  
  required_columns <- c("x", "y")
  missing_columns <- setdiff(required_columns, colnames(df_bloco))
  if (length(missing_columns) > 0) {
    stop(paste("As seguintes colunas obrigatórias estão ausentes em 'df_bloco':", 
               paste(missing_columns, collapse = ", ")))
  }
  
  if (!all(sapply(df_bloco[, required_columns], is.numeric))) {
    stop("As colunas 'x' e 'y' devem conter apenas valores numéricos.")
  }
  
  # Calcula variedade
  df_bloco_variety <- df_bloco %>%
    rowwise() %>%
    mutate(variety = length(unique(c_across(-c(x, y))))) %>%
    ungroup() %>%
    select(variety)
  
  # Função para calcular moda
  calcular_modal <- function(x) {
    tab <- table(as.character(x))
    names(which.max(tab))
  }
  
  # Calcula moda e frequência da moda
  df_bloco_moda_frequencia <- df_bloco %>%
    mutate(across(-c(x, y), as.character)) %>%
    rowwise() %>%
    mutate(
      moda = calcular_modal(c_across(-c(x, y))),
      frequencia_moda = (max(table(c_across(-c(moda, x, y)))) / length(c_across(-c(moda, x, y)))) * 100
    ) %>%
    ungroup() %>%
    select(moda, frequencia_moda)
  
  # Combina os resultados
  df_bloco_completo <- bind_cols(
    df_bloco[, c("x", "y")],
    df_bloco_variety,
    df_bloco_moda_frequencia
  )
  
  return(df_bloco_completo)
}
