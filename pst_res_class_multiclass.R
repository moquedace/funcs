pst_res_class_multiclass <- function(data, lev = NULL, model = NULL) {
  
  # Lista de pacotes necessários
  pkg <- c("caret")
  
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
  
  
  
  
  
  obs <- data[, "obs"]
  pred <- data[, "pred"]
  
  
  
  
  mcpr <- confusionMatrix(table(pred, obs), mode = "everything")
  
  
  kappa <- mcpr[["overall"]][["Kappa"]]
  accuracy <- mcpr[["overall"]][["Accuracy"]]
  f1_score <- mean(mcpr[["byClass"]][,"F1"], na.rm = TRUE)
  sensitivity <- mean(mcpr[["byClass"]][,"Sensitivity"], na.rm = TRUE)
  specificity <- mean(mcpr[["byClass"]][,"Specificity"], na.rm = TRUE)
  pos_pred_value <- mean(mcpr[["byClass"]][,"Pos Pred Value"], na.rm = TRUE)
  neg_pred_value <- mean(mcpr[["byClass"]][,"Neg Pred Value"], na.rm = TRUE)
  precision <- mean(mcpr[["byClass"]][,"Precision"], na.rm = TRUE)
  recall <- mean(mcpr[["byClass"]][,"Recall"], na.rm = TRUE)
  prevalence <- mean(mcpr[["byClass"]][,"Prevalence"], na.rm = TRUE)
  detection_rate <- mean(mcpr[["byClass"]][,"Detection Rate"], na.rm = TRUE)
  detection_prevalence <- mean(mcpr[["byClass"]][,"Detection Prevalence"], na.rm = TRUE)
  balanced_accuracy <- mean(mcpr[["byClass"]][,"Balanced Accuracy"], na.rm = TRUE)
  
  # Combina as métricas no vetor de saída
  out <- c(kappa, accuracy, f1_score, sensitivity, specificity, 
           pos_pred_value, neg_pred_value, precision, recall, prevalence,
           detection_rate, detection_prevalence, balanced_accuracy)
  
  # Atribuição dos nomes às métricas
  names(out) <- c("kappa", "accuracy", "f1_score", "sensitivity",
                  "specificity", "pos_pred_value", "neg_pred_value",
                  "precision", "recall", "prevalence",
                  "detection_rate", "detection_prevalence",
                  "balanced_accuracy")
  
  # Retorna o vetor de resultados
  return(out)
}
