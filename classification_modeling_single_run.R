source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
)

pkg <- c(
  "caret",
  "sf",
  "stringr",
  "quantregForest",
  "readxl",
  "dplyr",
  "terra",
  "parallelly",
  "parallel",
  "doParallel",
  "DescTools",
  "tidyr",
  "purrr",
  "readr",
  "gbm",
  "tibble",
  "ggplot2",
  "randomForest",
  "kknn",
  "kernlab",
  "C50"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

path_raiz <- "//200.235.173.229/dados_processamento/cassio/R/atlas_qf"
setwd(path_raiz)

# parameterization
cut_off_mc <- 0.9
nseed <- 666
fold_results <- "results_class_sentinel_5m_single_run"
run_rfe <- TRUE

# recursive feature elimination parameters
rfe_fold <- 10
rfe_repeat <- 1
rfe_size <- seq(2, 72, 2)
rfe_tn_length <- 1
tolerance <- FALSE
tol_per <- 1

# model parameters
model_fold <- 10
model_repeat <- 10
model_tn_length <- 10
metric_otm <- "kappa"
maxim <- TRUE
class_probs <- FALSE

# funcs
source(
  "https://github.com/moquedace/funcs/blob/main/gbm_custom.R?raw=TRUE"
)

source(
  "https://github.com/moquedace/funcs/blob/main/pst_res_class_multiclass.R?raw=TRUE"
)

custom_rcaretFuncs <- caretFuncs
custom_rcaretFuncs$summary <- pst_res_class_multiclass
custom_rcaretFuncs$fit <- function(x, y, first, last, ...) {
  train(x, y, metric = "kappa", ...)
}

create_dirs <- function(base_path, sub_dirs) {
  purrr::walk(sub_dirs, ~{
    dir_path <- file.path(base_path, .x)
    
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  })
}

safe_read <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    stop(paste("error: file not found", file_path))
  }
  
  readr::read_csv2(file_path, show_col_types = FALSE, ...)
}

get_caret_method <- function(model_name) {
  if (exists(model_name, inherits = TRUE)) {
    model_object <- get(model_name, inherits = TRUE)
    
    if (is.list(model_object) && all(c("label", "library", "type") %in% names(model_object))) {
      return(model_object)
    }
  }
  
  model_name
}

map_factor_back <- function(x, factor_vars, original_names) {
  if (x %in% original_names) {
    return(x)
  }
  
  hits <- factor_vars[startsWith(x, factor_vars)]
  
  if (length(hits) == 0) {
    return(x)
  }
  
  hits[which.max(nchar(hits))]
}

# list of models
models <- c(
  "rf",
  "kknn",
  "svmRadialSigma",
  "C5.0",
  "gbm_custom"
)

# factor predictor variables
varfact <- c("class") %>%
  sort()

# dummy variables protected from correlation filtering
dummy_vars_raw <- character(0)

# k-fold settings
kfold <- FALSE
col_kfold <- "id"

# read base dataset
dfbase <- safe_read("./extract_xy/yx_class_sentinel_5m.csv")
varsy <- names(dfbase)[1]

# parallel cluster setup
cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(cores)
cl <- parallelly::autoStopCluster(cl)

i <- 1
j <- 1

for (i in seq_along(models)) {
  
  tmodel <- Sys.time()
  
  for (j in seq_along(varsy)) {
    
    tvar <- Sys.time()
    target_var <- varsy[j]
    
    cat(
      sprintf(
        "model: %s | variable: %s\n",
        models[i],
        target_var
      )
    )
    
    path_results <- file.path(path_raiz, fold_results)
    
    create_dirs(
      path_results,
      c(
        models[i],
        file.path(models[i], target_var)
      )
    )
    
    path_results <- file.path(path_results, models[i], target_var)
    
    create_dirs(
      path_results,
      c(
        "select/cor",
        "select/nzv",
        "select/rfe/metric",
        "select/rfe/select",
        "performance/csv",
        "performance/imp_pred",
        "performance/pred_obs",
        "performance/conf_mat",
        "img",
        "model"
      )
    )
    
    dfperf <- tibble::tibble(
      model = NA_character_,
      target_var = NA_character_,
      selection_method = NA_character_,
      n = NA_integer_,
      n_predictors = NA_integer_,
      kappa_full = NA_real_,
      accuracy_full = NA_real_,
      sensitivity_full = NA_real_,
      specificity_full = NA_real_,
      precision_full = NA_real_,
      recall_full = NA_real_,
      balanced_accuracy_full = NA_real_,
      f1_full = NA_real_
    )
    
    dy <- dfbase %>%
      dplyr::select(dplyr::all_of(target_var))
    
    dx <- dfbase %>%
      dplyr::select(-dplyr::all_of(varsy))
    
    vars_fact_present <- intersect(varfact, names(dx))
    vars_factor_all <- unique(c(target_var, vars_fact_present))
    dummy_vars_present <- intersect(dummy_vars_raw, names(dx))
    
    has_kfold_col_global <- !is.null(col_kfold) &&
      length(col_kfold) == 1 &&
      nzchar(col_kfold) &&
      col_kfold %in% names(dx)
    
    metadata_vars <- if (has_kfold_col_global) {
      col_kfold
    } else {
      character(0)
    }
    
    dyx_sel <- dplyr::bind_cols(dy, dx)
    
    vars_numeric_to_clean <- names(dyx_sel)[
      vapply(dyx_sel, is.numeric, logical(1))
    ] %>%
      setdiff(vars_factor_all)
    
    dyx_sel <- dyx_sel %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(vars_numeric_to_clean),
          .fns = ~ round(.x, 4)
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(vars_numeric_to_clean),
          .fns = ~ ifelse(is.nan(.x), NA_real_, .x)
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(vars_numeric_to_clean),
          .fns = ~ ifelse(!is.finite(.x) & !is.nan(.x), NA_real_, .x)
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(vars_factor_all),
          .fns = as.factor
        )
      ) %>%
      na.omit()
    
    dyx_sel[[target_var]] <- droplevels(dyx_sel[[target_var]])
    class_levels <- levels(dyx_sel[[target_var]])
    
    class_count <- table(dyx_sel[[target_var]])
    
    if (length(class_levels) < 2) {
      stop(
        paste0(
          "Classification requires at least two classes after cleaning for target: ",
          target_var
        )
      )
    }
    
    if (any(class_count < 2)) {
      stop(
        paste0(
          "Classification requires at least 2 observations per class. ",
          "The following classes have fewer than 2 observations: ",
          paste(names(class_count[class_count < 2]), collapse = ", ")
        )
      )
    }
    
    nzv_candidates <- setdiff(
      names(dyx_sel),
      c(target_var, dummy_vars_present, metadata_vars)
    )
    
    if (length(nzv_candidates) > 0) {
      nzv_vars <- caret::nearZeroVar(
        dyx_sel[, nzv_candidates, drop = FALSE],
        names = TRUE
      )
    } else {
      nzv_vars <- character(0)
    }
    
    readr::write_csv2(
      data.frame(
        nzv_removed = nzv_vars,
        stringsAsFactors = FALSE
      ),
      file.path(
        path_results,
        "select/nzv",
        paste0(target_var, "_nzv.csv")
      )
    )
    
    if (length(nzv_vars) > 0) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(-dplyr::any_of(nzv_vars))
    }
    
    cor_candidates <- setdiff(
      names(dyx_sel),
      c(target_var, dummy_vars_present, vars_factor_all, metadata_vars)
    )
    
    cor_candidates <- cor_candidates[
      vapply(dyx_sel[, cor_candidates, drop = FALSE], is.numeric, logical(1))
    ]
    
    if (length(cor_candidates) >= 2) {
      mcor <- dyx_sel %>%
        dplyr::select(dplyr::all_of(cor_candidates)) %>%
        stats::cor(
          method = "spearman",
          use = "pairwise.complete.obs"
        )
      
      fc <- caret::findCorrelation(
        mcor,
        cutoff = cut_off_mc,
        names = TRUE
      )
    } else {
      fc <- character(0)
    }
    
    readr::write_csv2(
      data.frame(
        cor_removed = fc,
        stringsAsFactors = FALSE
      ),
      file.path(
        path_results,
        "select/cor",
        paste0(target_var, "_cor.csv")
      )
    )
    
    if (length(fc) > 0) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(-dplyr::any_of(fc))
    }
    
    has_kfold_col <- !is.null(col_kfold) &&
      length(col_kfold) == 1 &&
      nzchar(col_kfold) &&
      col_kfold %in% names(dyx_sel)
    
    if (kfold) {
      if (!has_kfold_col) {
        stop(
          paste0(
            "kfold = TRUE, but grouping column was not found in dyx_sel: ",
            col_kfold
          )
        )
      }
      
      set.seed(nseed)
      gkfold_rfe <- caret::groupKFold(
        dyx_sel[[col_kfold]],
        k = rfe_fold
      )
      
      set.seed(nseed)
      gkfold_model <- caret::groupKFold(
        dyx_sel[[col_kfold]],
        k = model_fold
      )
      
      dyx_sel <- dyx_sel %>%
        dplyr::select(-dplyr::all_of(col_kfold))
      
    } else {
      gkfold_rfe <- NULL
      gkfold_model <- NULL
      
      if (has_kfold_col) {
        dyx_sel <- dyx_sel %>%
          dplyr::select(-dplyr::all_of(col_kfold))
      }
    }
    
    registerDoParallel(cl)
    
    all_predictors_after_filter <- setdiff(
      names(dyx_sel),
      target_var
    )
    
    if (length(all_predictors_after_filter) == 0) {
      stop(
        paste0(
          "No predictors available after NZV and correlation filtering for target: ",
          target_var
        )
      )
    }
    
    if (run_rfe) {
      
      cat("RFE enabled: running recursive feature elimination\n")
      
      rfe_size_current <- sort(
        unique(
          rfe_size[rfe_size <= length(all_predictors_after_filter)]
        )
      )
      
      if (length(rfe_size_current) == 0) {
        rfe_size_current <- length(all_predictors_after_filter)
      }
      
      set.seed(nseed)
      rfe_ctrl <- rfeControl(
        functions = custom_rcaretFuncs,
        method = "repeatedcv",
        repeats = rfe_repeat,
        number = rfe_fold,
        verbose = FALSE,
        index = gkfold_rfe,
        allowParallel = TRUE
      )
      
      set.seed(nseed)
      rfe_model_ctrl <- trainControl(
        method = "repeatedcv",
        number = rfe_fold,
        repeats = rfe_repeat,
        savePredictions = TRUE,
        summaryFunction = pst_res_class_multiclass,
        index = gkfold_rfe,
        classProbs = class_probs,
        allowParallel = TRUE
      )
      
      formu <- stats::reformulate(
        termlabels = all_predictors_after_filter,
        response = target_var
      )
      
      set.seed(nseed)
      rfe_fit <- caret::rfe(
        form = formu,
        data = dyx_sel,
        sizes = rfe_size_current,
        method = get_caret_method(models[i]),
        metric = metric_otm,
        maximize = maxim,
        importance = TRUE,
        trControl = rfe_model_ctrl,
        tuneLength = rfe_tn_length,
        rfeControl = rfe_ctrl
      )
      
      print(rfe_fit)
      cat("\n-----------------------------------------------------------\n")
      
      cat(
        sprintf(
          "time for rfe - model: %s | variable: %s | duration: %.2f %s\n",
          models[i],
          target_var,
          Sys.time() - tvar,
          units(Sys.time() - tvar)
        )
      )
      
      lrferes <- if (!is.null(rfe_fit$results)) {
        rfe_fit$results
      } else {
        rfe_fit$result
      }
      
      if (tolerance) {
        pick <- caret::pickSizeTolerance(
          x = lrferes,
          metric = metric_otm,
          tol = tol_per,
          maximize = maxim
        )
        
        lrfepred <- rfe_fit$optVariables[1:pick]
        
      } else {
        lrfepred <- rfe_fit$optVariables
      }
      
      if (length(varfact) > 0) {
        original_predictor_names <- names(dyx_sel)
        
        lrfepred <- unique(
          vapply(
            lrfepred,
            map_factor_back,
            FUN.VALUE = character(1),
            factor_vars = varfact,
            original_names = original_predictor_names
          )
        )
      }
      
      selection_method <- "rfe"
      
      cat(
        sprintf(
          "rfe select: %d variables\n",
          length(lrfepred)
        )
      )
      
    } else {
      
      cat("RFE disabled: using all predictors after NZV and correlation filtering\n")
      
      rfe_fit <- NULL
      lrfepred <- all_predictors_after_filter
      
      lrferes <- tibble::tibble(
        selection_method = "no_rfe_after_nzv_and_correlation",
        target_var = target_var,
        model = models[i],
        n_predictors = length(lrfepred),
        predictors = paste(lrfepred, collapse = ";")
      )
      
      selection_method <- "no_rfe"
      
      cat(
        sprintf(
          "predictors selected without RFE: %d variables\n",
          length(lrfepred)
        )
      )
    }
    
    cat("-----------------------------------------------------------\n")
    
    readr::write_csv2(
      data.frame(lrferes),
      file.path(
        path_results,
        "select/rfe/metric",
        "rfe_metrics.csv"
      )
    )
    
    readr::write_csv2(
      data.frame(
        selection_method = selection_method,
        pred_sel = lrfepred
      ),
      file.path(
        path_results,
        "select/rfe/select",
        "rfe_pred_sel.csv"
      )
    )
    
    dfselrfe <- dyx_sel %>%
      dplyr::select(
        dplyr::all_of(c(target_var, lrfepred))
      )
    
    predictor_names_selected <- setdiff(
      names(dfselrfe),
      target_var
    )
    
    if (length(predictor_names_selected) == 0) {
      stop(
        paste0(
          "No predictors available in dfselrfe for variable: ",
          target_var
        )
      )
    }
    
    set.seed(nseed)
    model_ctrl <- trainControl(
      method = "repeatedcv",
      number = model_fold,
      repeats = model_repeat,
      savePredictions = TRUE,
      index = gkfold_model,
      summaryFunction = pst_res_class_multiclass,
      classProbs = class_probs,
      allowParallel = TRUE
    )
    
    formu <- stats::reformulate(
      termlabels = predictor_names_selected,
      response = target_var
    )
    
    set.seed(nseed)
    registerDoParallel(cl)
    
    model_fit <- caret::train(
      form = formu,
      data = dfselrfe,
      metric = metric_otm,
      importance = TRUE,
      maximize = maxim,
      method = get_caret_method(models[i]),
      trControl = model_ctrl,
      tuneLength = model_tn_length
    )
    
    print(model_fit)
    cat("\n-----------------------------------------------------------\n")
    
    cat(
      sprintf(
        "%s | variable: %s | duration: %.2f %s\n",
        model_fit[["modelInfo"]][["label"]],
        target_var,
        Sys.time() - tvar,
        units(Sys.time() - tvar)
      )
    )
    
    cat("-----------------------------------------------------------\n")
    
    lmodel <- model_fit
    
    pred_data_full <- dfselrfe %>%
      dplyr::select(dplyr::all_of(predictor_names_selected))
    
    pred_obs_full <- tibble::tibble(
      pred = as.character(predict(lmodel, pred_data_full)),
      obs = as.character(dfselrfe[[target_var]]),
      data = "full"
    ) %>%
      dplyr::filter(!is.na(pred) & !is.na(obs))
    
    pred_obs_full$pred <- factor(
      pred_obs_full$pred,
      levels = class_levels
    )
    
    pred_obs_full$obs <- factor(
      pred_obs_full$obs,
      levels = class_levels
    )
    
    pr_full <- caret::getTrainPerf(lmodel)
    
    conf_matrix_full <- caret::confusionMatrix(
      data = pred_obs_full$pred,
      reference = pred_obs_full$obs,
      mode = "everything"
    )
    
    pred_imp <- caret::varImp(
      lmodel,
      scale = TRUE
    )
    
    lpredimp <- pred_imp$importance %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "predictor") %>%
      tidyr::pivot_longer(
        cols = -predictor,
        names_to = "class",
        values_to = "importance"
      ) %>%
      dplyr::mutate(
        class = gsub("^importance\\.", "", class)
      ) %>%
      dplyr::filter(!is.na(importance)) %>%
      dplyr::relocate(class, predictor)
    
    lpredimp_top <- lpredimp %>%
      dplyr::group_by(class) %>%
      dplyr::slice_max(
        order_by = importance,
        n = 15,
        with_ties = FALSE
      ) %>%
      dplyr::ungroup()
    
    g1 <- ggplot(
      lpredimp_top,
      aes(
        y = reorder(predictor, importance),
        x = importance
      )
    ) +
      geom_col() +
      facet_wrap(
        ~ class,
        scales = "free_x"
      ) +
      labs(
        title = paste0(target_var, " | top predictors"),
        x = "Importance",
        y = NULL
      )
    
    plot(g1)
    
    g2 <- ggplot(
      pred_obs_full,
      aes(
        x = obs,
        fill = pred
      )
    ) +
      geom_bar(
        position = "dodge"
      ) +
      labs(
        title = paste0(target_var, " | observed versus predicted classes"),
        x = "Observed class",
        y = "Count",
        fill = "Predicted class"
      )
    
    plot(g2)
    
    readr::write_csv2(
      lpredimp,
      file.path(
        path_results,
        "performance/imp_pred",
        "imp_pred.csv"
      )
    )
    
    readr::write_csv2(
      pred_obs_full,
      file.path(
        path_results,
        "performance/pred_obs",
        "pred_obs_full.csv"
      )
    )
    
    readr::write_csv2(
      as.data.frame(conf_matrix_full$table),
      file.path(
        path_results,
        "performance/conf_mat",
        paste0(target_var, "_conf_matrix_full.csv")
      )
    )
    
    saveRDS(
      conf_matrix_full,
      file.path(
        path_results,
        "performance/conf_mat",
        paste0(target_var, "_conf_matrix_full.rds")
      )
    )
    
    dfperf[1, ] <- list(
      model = lmodel$modelInfo$label,
      target_var = target_var,
      selection_method = selection_method,
      n = nrow(dfselrfe),
      n_predictors = length(lrfepred),
      kappa_full = as.numeric(pr_full$Trainkappa),
      accuracy_full = as.numeric(pr_full$Trainaccuracy),
      sensitivity_full = as.numeric(pr_full$Trainsensitivity),
      specificity_full = as.numeric(pr_full$Trainspecificity),
      precision_full = as.numeric(pr_full$Trainprecision),
      recall_full = as.numeric(pr_full$Trainrecall),
      balanced_accuracy_full = as.numeric(pr_full$Trainbalanced_accuracy),
      f1_full = as.numeric(pr_full$Trainf1_score)
    )
    
    readr::write_csv2(
      dfperf,
      file.path(
        path_results,
        "performance/csv",
        paste0(target_var, "_performance.csv")
      )
    )
    
    saveRDS(
      object = model_fit,
      file = file.path(
        path_results,
        "model",
        paste0(target_var, "_model_fit.rds")
      )
    )
    
    saveRDS(
      object = list(
        run_rfe = run_rfe,
        rfe_fit = rfe_fit,
        selected_predictors = lrfepred,
        selection_method = selection_method
      ),
      file = file.path(
        path_results,
        "model",
        paste0(target_var, "_rfe_fit.rds")
      )
    )
    
    saveRDS(
      object = list(
        model_fit = model_fit,
        run_rfe = run_rfe,
        rfe_fit = rfe_fit,
        selected_predictors = lrfepred,
        selection_method = selection_method,
        performance = dfperf,
        pred_obs_full = pred_obs_full,
        predictor_importance = lpredimp,
        conf_matrix_full = conf_matrix_full,
        class_probs = class_probs,
        class_levels = class_levels
      ),
      file = file.path(
        path_results,
        "model",
        paste0(target_var, "_model_bundle.rds")
      )
    )
    
    save.image(
      file = file.path(
        path_results,
        "img",
        paste0(target_var, ".rdata")
      )
    )
    
    cat(
      sprintf(
        "total time for variable %s | model: %s | duration: %.2f %s\n",
        target_var,
        models[i],
        Sys.time() - tvar,
        units(Sys.time() - tvar)
      )
    )
    
    gc()
    
  } # end for j
  
  cat(
    sprintf(
      "total time for model %s | duration: %.2f %s\n",
      models[i],
      Sys.time() - tmodel,
      units(Sys.time() - tmodel)
    )
  )
  
} # end for i
