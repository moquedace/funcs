calcular_metricas_raster <- function(df_bloco) {
  # Função para calcular métricas de variedade, moda e frequência da moda em um dataframe

  # Função auxiliar para instalar e carregar pacotes necessários
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

  # Verificações de entrada
  if (!is.data.frame(df_bloco)) {
    stop("O argumento 'df_bloco' deve ser um data.frame.")
  }

  required_columns <- c("x", "y")
  missing_columns <- setdiff(required_columns, colnames(df_bloco))
  if (length(missing_columns) > 0) {
    stop(paste("As seguintes colunas obrigatórias estão ausentes em 'df_bloco':", paste(missing_columns, collapse = ", ")))
  }

  if (!all(sapply(df_bloco[, c("x", "y")], is.numeric))) {
    stop("As colunas 'x' e 'y' devem conter apenas valores numéricos.")
  }

  # Calcula variedade (número de valores únicos nas colunas além de x e y)
  df_bloco_variety <- df_bloco %>%
    rowwise() %>%
    summarize(
      variety = length(unique(c_across(-c(x, y)))),
      .groups = "drop"
    )

  # Calcula a moda (valor mais frequente nas colunas além de x e y)
  df_bloco_moda <- df_bloco %>%
    rowwise() %>%
    summarize(
      moda = {
        valores <- c_across(-c(x, y))
        valores <- valores[!is.na(valores)]
        if (length(valores) == 0) return(NA)
        as.character(names(which.max(table(valores))))
      },
      .groups = "drop"
    )

  # Calcula a frequência da moda (percentual de ocorrência do valor moda)
  df_bloco_frequencia <- df_bloco %>%
    rowwise() %>%
    summarize(
      frequencia_moda = {
        valores <- c_across(-c(x, y))
        valores <- valores[!is.na(valores)]
        if (length(valores) == 0) return(NA_real_)
        valor_moda <- df_bloco_moda$moda[row_number()]
        sum(valores == valor_moda) / length(valores) * 100
      },
      .groups = "drop"
    )

  # Combina todas as métricas em um único dataframe
  df_bloco_completo <- cbind(
    df_bloco[, c("x", "y")], 
    df_bloco_variety[, "variety", drop = FALSE], 
    df_bloco_moda[, "moda", drop = FALSE], 
    df_bloco_frequencia[, "frequencia_moda", drop = FALSE]
  )

  return(df_bloco_completo)
}
