pst_res_class_multiclass <- function(data, lev = NULL, model = NULL) {

  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required by pst_res_class_multiclass.")
  }

  metric_names <- c("kappa", "accuracy", "f1_score", "sensitivity",
                    "specificity", "pos_pred_value", "neg_pred_value",
                    "precision", "recall", "prevalence",
                    "detection_rate", "detection_prevalence",
                    "balanced_accuracy")

  na_out <- stats::setNames(rep(NA_real_, length(metric_names)), metric_names)

  obs  <- data[, "obs"]
  pred <- data[, "pred"]

  all_lev <- if (!is.null(lev)) {
    lev
  } else {
    union(levels(factor(obs)), levels(factor(pred)))
  }

  obs  <- factor(as.character(obs),  levels = all_lev)
  pred <- factor(as.character(pred), levels = all_lev)

  keep <- !is.na(obs) & !is.na(pred)
  obs  <- obs[keep]
  pred <- pred[keep]

  if (length(obs) < 1 || length(all_lev) < 2) {
    return(na_out)
  }

  mcpr <- tryCatch(
    caret::confusionMatrix(table(pred, obs), mode = "everything"),
    error = function(e) NULL
  )

  if (is.null(mcpr)) {
    return(na_out)
  }

  mean_by <- function(col) {
    bc <- mcpr[["byClass"]]
    if (is.null(bc)) {
      return(NA_real_)
    }
    if (is.matrix(bc)) {
      if (!col %in% colnames(bc)) return(NA_real_)
      mean(bc[, col], na.rm = TRUE)
    } else {
      if (!col %in% names(bc)) return(NA_real_)
      as.numeric(bc[[col]])
    }
  }

  out <- c(
    kappa                = as.numeric(mcpr[["overall"]][["Kappa"]]),
    accuracy             = as.numeric(mcpr[["overall"]][["Accuracy"]]),
    f1_score             = mean_by("F1"),
    sensitivity          = mean_by("Sensitivity"),
    specificity          = mean_by("Specificity"),
    pos_pred_value       = mean_by("Pos Pred Value"),
    neg_pred_value       = mean_by("Neg Pred Value"),
    precision            = mean_by("Precision"),
    recall               = mean_by("Recall"),
    prevalence           = mean_by("Prevalence"),
    detection_rate       = mean_by("Detection Rate"),
    detection_prevalence = mean_by("Detection Prevalence"),
    balanced_accuracy    = mean_by("Balanced Accuracy")
  )

  out[!is.finite(out)] <- NA_real_

  out[metric_names]
}
