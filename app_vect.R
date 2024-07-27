app_vect <- function(list_vect, name_col, fun, graph = TRUE, parallel = FALSE, workers = NULL) {
  
  # Instalar e carregar pacotes necessários
  install_and_load_packages <- function(packages) {
    installed <- rownames(installed.packages())
    to_install <- packages[!packages %in% installed]
    if (length(to_install) > 0) {
      install.packages(to_install)
    }
    suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE))
  }
  
  required_packages <- c("terra", "purrr", "future.apply")
  install_and_load_packages(required_packages)
  
  # Verificação dos argumentos
  if (!is.list(list_vect)) {
    stop("Erro: 'list_vect' deve ser uma lista de objetos SpatVector.")
  }
  if (!is.character(name_col) || length(name_col) != 1) {
    stop("Erro: 'name_col' deve ser um único nome de coluna como string.")
  }
  if (!is.function(fun) && !fun %in% c("modal", "variety")) {
    stop("Erro: 'fun' deve ser uma função válida ou um dos seguintes valores: 'modal', 'variety'.")
  }
  if (!is.logical(graph) || length(graph) != 1) {
    stop("Erro: 'graph' deve ser um valor lógico (TRUE ou FALSE).")
  }
  if (!is.logical(parallel) || length(parallel) != 1) {
    stop("Erro: 'parallel' deve ser um valor lógico (TRUE ou FALSE).")
  }
  if (!is.null(workers) && (!is.numeric(workers) || length(workers) != 1 || workers <= 0)) {
    stop("Erro: 'workers' deve ser um número inteiro positivo ou NULL.")
  }

  # Funções modal e variety
  variety <- function(x) {
    var_uni <- length(unique(x[!is.na(x)]))
    return(var_uni)
  }
  
  modal <- function(x) {
    unique_x <- unique(x)
    unique_x[which.max(tabulate(match(x, unique_x)))]
  }
  
  # Captura o tempo inicial
  start_time <- Sys.time()
  
  # Mensagem inicial
  cat("Iniciando processamento...\n")
  
  # Extrair matrizes de atributos
  attribute_matrices <- map(list_vect, function(spv) {
    if (!name_col %in% names(values(spv))) {
      stop(paste("Erro: Coluna", name_col, "não encontrada em um dos objetos SpatVector."))
    }
    values(spv)[[name_col]]
  })
  
  attribute_matrix <- do.call(cbind, attribute_matrices)
  
  # Configuração para processamento paralelo
  if (parallel) {
    plan(multisession, workers = ifelse(is.null(workers), availableCores(), workers))
    apply_fun <- future.apply::future_apply
  } else {
    apply_fun <- apply
  }
  
  # Aplicar a função apropriada
  if (fun %in% c("modal", "variety")) {
    fun_values <- apply_fun(attribute_matrix, 1, get(fun))
  } else {
    fun_values <- apply_fun(attribute_matrix, 1, fun)
  }
  
  # Criar SpatVector resultante
  fun_spatvector <- list_vect[[1]]
  values(fun_spatvector)[[name_col]] <- fun_values
  ncol_rename <- which(names(fun_spatvector) == name_col)
  new_col_name <- ifelse(fun %in% c("modal", "variety"), paste(fun, name_col, sep = "_"), name_col)
  names(fun_spatvector)[ncol_rename] <- new_col_name
  
  # Plotar se necessário
  if (graph) {
    plot(fun_spatvector, new_col_name)
  }
  
  # Mensagem de conclusão
  cat("Processamento concluído!\n")
  
  # Exibir o tempo de execução
  end_time <- Sys.time()
  cat("Tempo de execução:", round(difftime(end_time, start_time, units = "secs"), 2), "segundos.\n")
  
  return(fun_spatvector)
}
