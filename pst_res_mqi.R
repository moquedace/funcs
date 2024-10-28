pst_res_mqi <- function(data, lev = NULL, model = NULL) {
  
  # Lista de pacotes necessários
  pkg <- c("DescTools")
  
  # Verifica e instala o pacote caso necessário
  installed_packages <- pkg %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    package_to_install <- pkg[!installed_packages]
    cat("Instalando pacote(s):", package_to_install, "\n")
    install.packages(package_to_install)
    cat("Pacote(s) instalado(s).\n")
  }
  
  # Carrega o(s) pacote(s) necessário(s)
  suppressPackageStartupMessages(lapply(pkg, library, character.only = TRUE))
  
  # Extrai as colunas de observação e predição dos dados
  obs <- data[, "obs"]
  pred <- data[, "pred"]
  
  # Calcula o Coeficiente de Correlação Concordante (CCC) com intervalo de confiança de 95%
  ccc <- CCC(obs, pred, conf.level = 0.95)$rho.c$est
  
  # Calcula o Coeficiente de Determinação (R²)
  r2 <- try(cor(pred, obs, use = "pairwise.complete.obs"), silent = TRUE)^2
  
  # Calcula o Mean Absolute Error (MAE)
  mae <- mean(abs(pred - obs))
  
  # Calcula o Nash-Sutcliffe Efficiency (NSE)
  nse <- 1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))
  
  # Calcula o Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean((pred - obs)^2))
  
  # Calcula o índice de qualidade do modelo (MQI)
  mqi <- (ccc * nse) / (mae / mean(obs))
  
  # Combina os resultados em um vetor nomeado para saída
  out <- c(ccc, r2, nse, mae, rmse, mqi)
  names(out) <- c("ccc", "r2", "nse", "mae", "rmse", "mqi")
  
  # Retorna o vetor de resultados
  return(out)
}
