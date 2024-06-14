pst_res_ccc <- function(data, lev = NULL, model = NULL) {
  # Pacote necessário
  pkg <- c("DescTools")
  
  # Verifica se o pacote está instalado, caso contrário, instala
  installed_packages <- pkg %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    package_to_install <- pkg[!installed_packages]
    cat("Instalando pacote(s):", package_to_install, "\n")
    install.packages(package_to_install)
    cat("Pacote(s) instalado(s).\n")
  }
  
  # Carrega o(s) pacote(s)
  invisible(lapply(pkg, library, character.only = TRUE))
  
  # Extrai as colunas de observações e previsões dos dados
  obs <- data[, "obs"]
  pred <- data[, "pred"]
  
  # Calcula o Coeficiente de Correlação Concordante (CCC) com intervalo de confiança de 95%
  ccc <- CCC(obs, pred, conf.level = 0.95)$rho.c$est
  # Calcula o Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean((pred - obs)^2))
  # Calcula o Mean Absolute Error (MAE)
  mae <- mean(abs(pred - obs))
  # Calcula o Coeficiente de Determinação (R²)
  r2 <- try(cor(pred, obs, use = "pairwise.complete.obs"), silent = TRUE)^2
  
  # Combina os resultados em um vetor nomeado
  out <- c(ccc, r2, mae, rmse)
  names(out) <- c("ccc", "r2", "mae", "rmse")
  
  return(out)
}
