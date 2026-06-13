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
  "C50",
  "foreach"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

path_raiz <- "//200.235.173.229/dados_processamento/cassio/R/atlas_qf"
setwd(path_raiz)

source(
  "https://github.com/moquedace/funcs/blob/main/gbm_custom.R?raw=TRUE"
)

source(
  "https://github.com/moquedace/funcs/blob/main/pst_res_class_multiclass.R?raw=TRUE"
)

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/caret_rfe_functions.R"
)

# parameterization
# =================================================================
# <-- EDIT HERE (v2): principais knobs estao nesta secao:
#   path_raiz / fold_results / row_limit / rfe_* / model_* /
#   kfold + col_kfold / metric_otm / cores / cut_off_mc
#   (a leitura do dataset e a definicao de varsy ficam mais abaixo)
# =================================================================

# NOTE (v2):
#  - E1: NZV e correlacao sao calculados no dataset COMPLETO, antes do
#        split treino/teste. Sao filtros nao-supervisionados (baixo
#        vazamento); para rigor maximo use os scripts *nested*.
#  - E3: o tuning interno do RFE (rfe_model_ctrl, index = NULL) usa
#        reamostragem nao-agrupada; so o rfeControl externo respeita os
#        grupos quando kfold = TRUE.

cut_off_mc <- 0.9
nseed <- 666
fold_results <- "results_class_sentinel_5m_single_run"

row_limit <- Inf
# Use row_limit <- 50 only for quick tests.

run_rfe <- TRUE

# <-- EDIT HERE (v2): controles de I/O e plots
save_workspace <- FALSE   # TRUE = salva if (save_workspace) save.image() a cada iteracao (lento em rede)
pause_plots    <- FALSE   # TRUE = Sys.sleep() apos cada grafico

# predictors that must always remain in the model
# use character(0) when no fixed predictors are needed

fixed_predictors <- character(0)

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

# k-fold settings
# if kfold = TRUE, col_kfold is used for grouped internal resampling
# if kfold = FALSE, col_kfold is removed from predictors if present

kfold <- FALSE
col_kfold <- "id"

# parallel parameters

use_parallel <- TRUE
allow_parallel_rfe <- TRUE
allow_parallel_model <- TRUE

cores <- max(   # <-- EDIT HERE: nucleos do cluster paralelo
  1,
  parallel::detectCores() - 1
)

parallel_packages <- c(
  "caret",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "readr",
  "stringr",
  "randomForest",
  "kknn",
  "kernlab",
  "C50",
  "gbm",
  "DescTools",
  "foreach"
)

# caret functions

custom_rcaretFuncs <- make_ccaret_funcs(
  metric_value = metric_otm,
  summary_function = pst_res_class_multiclass
)

# support functions

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

# read base dataset

dfbase <- safe_read_csv2(
  "./extract_xy/yx_class_sentinel_5m.csv"
)

if (is.finite(row_limit) && nrow(dfbase) > row_limit) {
  dfbase <- dfbase %>%
    dplyr::slice_head(n = row_limit)
}

varsy <- names(dfbase)[1]

# parallel cluster setup

parallel_export_objects <- c(
  "metric_otm",
  "pst_res_class_multiclass",
  "custom_rcaretFuncs",
  "gbm_custom"
)

cl <- setup_parallel_backend(
  use_parallel = use_parallel,
  cores = cores,
  packages = parallel_packages,
  objects_to_export = parallel_export_objects
)

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
      dplyr::select(
        dplyr::all_of(target_var)
      )
    
    dx <- dfbase %>%
      dplyr::select(
        -dplyr::all_of(varsy)
      )
    
    dyx_sel <- dplyr::bind_cols(dy, dx)
    
    has_kfold_col <- length(col_kfold) == 1 &&
      nzchar(col_kfold) &&
      col_kfold %in% names(dyx_sel)
    
    kfold_col_present <- if (has_kfold_col) {
      col_kfold
    } else {
      character(0)
    }
    
    if (kfold && !has_kfold_col) {
      stop(
        paste0(
          "kfold = TRUE, but grouping column was not found in dyx_sel: ",
          col_kfold
        )
      )
    }
    
    vars_fact_present <- intersect(varfact, names(dyx_sel)) %>%
      setdiff(kfold_col_present)
    
    vars_factor_all <- unique(
      c(target_var, vars_fact_present)
    ) %>%
      setdiff(kfold_col_present)
    
    dummy_vars_present <- intersect(dummy_vars_raw, names(dyx_sel))
    
    fixed_predictors_present <- intersect(fixed_predictors, names(dyx_sel)) %>%
      setdiff(kfold_col_present)
    
    fixed_predictors_missing <- setdiff(fixed_predictors, names(dyx_sel))
    
    if (length(fixed_predictors_missing) > 0) {
      warning(
        paste0(
          "Some fixed predictors were not found in dyx_sel and will be ignored: ",
          paste(fixed_predictors_missing, collapse = ", ")
        )
      )
    }
    
    if (length(intersect(fixed_predictors, kfold_col_present)) > 0) {
      warning(
        paste0(
          "The kfold column cannot be used as a fixed predictor and will be ignored as predictor: ",
          paste(kfold_col_present, collapse = ", ")
        )
      )
    }
    
    vars_numeric_to_clean <- names(dyx_sel)[
      vapply(dyx_sel, is.numeric, logical(1))
    ] %>%
      setdiff(c(vars_factor_all, kfold_col_present))
    
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
      tidyr::drop_na()
    
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
      c(
        target_var,
        dummy_vars_present,
        fixed_predictors_present,
        kfold_col_present
      )
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
      file = file.path(
        path_results,
        "select/nzv",
        paste0(target_var, "_nzv.csv")
      )
    )
    
    if (length(nzv_vars) > 0) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::any_of(nzv_vars)
        )
    }
    
    cor_candidates <- setdiff(
      names(dyx_sel),
      c(
        target_var,
        dummy_vars_present,
        vars_factor_all,
        fixed_predictors_present,
        kfold_col_present
      )
    )
    
    cor_candidates <- cor_candidates[
      vapply(dyx_sel[, cor_candidates, drop = FALSE], is.numeric, logical(1))
    ]
    
    if (length(cor_candidates) >= 2) {
      
      mcor <- dyx_sel %>%
        dplyr::select(
          dplyr::all_of(cor_candidates)
        ) %>%
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
      file = file.path(
        path_results,
        "select/cor",
        paste0(target_var, "_cor.csv")
      )
    )
    
    if (length(fc) > 0) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::any_of(fc)
        )
    }
    
    if (kfold) {
      
      n_groups <- dplyr::n_distinct(
        dyx_sel[[col_kfold]]
      )
      
      rfe_fold_use <- min(
        rfe_fold,
        n_groups
      )
      
      model_fold_use <- min(
        model_fold,
        n_groups
      )
      
      set.seed(nseed)
      
      gkfold_rfe <- make_repeated_stratified_group_kfold(
        data = dyx_sel,
        group_var = col_kfold,
        target_var = target_var,
        k = rfe_fold_use,
        repeats = rfe_repeat,
        seed = nseed
      )

      gkfold_rfe <- validate_resampling_index(
        index = gkfold_rfe,
        n_rows = nrow(dyx_sel),
        target = dyx_sel[[target_var]],
        class_levels = class_levels,
        require_all_classes = TRUE
      )
      
      set.seed(nseed)
      
      gkfold_model <- make_repeated_stratified_group_kfold(
        data = dyx_sel,
        group_var = col_kfold,
        target_var = target_var,
        k = model_fold_use,
        repeats = model_repeat,
        seed = nseed
      )

      gkfold_model <- validate_resampling_index(
        index = gkfold_model,
        n_rows = nrow(dyx_sel),
        target = dyx_sel[[target_var]],
        class_levels = class_levels,
        require_all_classes = TRUE
      )
      
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::all_of(col_kfold)
        )
      
    } else {
      
      gkfold_rfe <- NULL
      gkfold_model <- NULL
      
      rfe_fold_use <- rfe_fold
      model_fold_use <- model_fold
      
      if (has_kfold_col) {
        dyx_sel <- dyx_sel %>%
          dplyr::select(
            -dplyr::all_of(col_kfold)
          )
      }
      
    } # end if kfold
    
    fixed_predictors_present <- intersect(fixed_predictors_present, names(dyx_sel))
    
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
    
    rfe_candidate_predictors <- setdiff(
      all_predictors_after_filter,
      fixed_predictors_present
    )
    
    if (run_rfe && length(rfe_candidate_predictors) > 0) {
      
      cat("RFE enabled: running recursive feature elimination\n")
      
      rfe_data <- dyx_sel %>%
        dplyr::select(
          dplyr::all_of(c(target_var, rfe_candidate_predictors))
        )
      
      rfe_size_current <- make_valid_rfe_sizes(
        rfe_size = rfe_size,
        n_predictors = length(rfe_candidate_predictors)
      )
      
      set.seed(nseed)
      
      rfe_ctrl <- make_rfe_control(
        method = "repeatedcv",
        number = rfe_fold_use,
        repeats = rfe_repeat,
        index = gkfold_rfe,
        functions_object = custom_rcaretFuncs,
        verbose = FALSE,
        allow_parallel = allow_parallel_rfe
      )
      
      set.seed(nseed)
      
      rfe_model_ctrl <- make_train_control(
        method = "repeatedcv",
        number = rfe_fold_use,
        repeats = rfe_repeat,
        index = NULL,
        summary_function = pst_res_class_multiclass,
        save_predictions = TRUE,
        allow_parallel = FALSE
      )
      
      rfe_model_ctrl$classProbs <- class_probs
      
      formu <- stats::reformulate(
        termlabels = rfe_candidate_predictors,
        response = target_var
      )
      
      set.seed(nseed)
      
      rfe_fit <- caret::rfe(
        form = formu,
        data = rfe_data,
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
        
        lrfepred <- head(rfe_fit$optVariables, pick)
        
      } else {
        
        lrfepred <- rfe_fit$optVariables
        
      } # end if tolerance
      
      if (length(varfact) > 0) {
        
        original_predictor_names <- names(rfe_data)
        
        lrfepred <- unique(
          vapply(
            lrfepred,
            map_factor_back,
            FUN.VALUE = character(1),
            factor_vars = varfact,
            original_names = original_predictor_names
          )
        )
        
      } # end if varfact
      
      rfe_selected_predictors <- lrfepred
      
      lrfepred <- unique(
        c(fixed_predictors_present, rfe_selected_predictors)
      )
      
      selection_method <- ifelse(
        length(fixed_predictors_present) > 0,
        "rfe_plus_fixed",
        "rfe"
      )
      
      cat(
        sprintf(
          "rfe select: %d variables\n",
          length(lrfepred)
        )
      )
      
    } else if (run_rfe && length(rfe_candidate_predictors) == 0) {
      
      cat("RFE enabled, but no candidate predictors are available after filters. Using fixed predictors only\n")
      
      rfe_fit <- NULL
      
      lrfepred <- fixed_predictors_present
      
      lrferes <- tibble::tibble(
        selection_method = "fixed_only_no_rfe_candidates",
        target_var = target_var,
        model = models[i],
        n_predictors = length(lrfepred),
        predictors = paste(lrfepred, collapse = ";")
      )
      
      selection_method <- "fixed_only"
      
      cat(
        sprintf(
          "fixed predictors selected: %d variables\n",
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
      
    } # end if run_rfe
    
    cat("-----------------------------------------------------------\n")
    
    if (length(lrfepred) == 0) {
      stop(
        paste0(
          "No predictors were selected for target: ",
          target_var
        )
      )
    }
    
    readr::write_csv2(
      data.frame(lrferes),
      file = file.path(
        path_results,
        "select/rfe/metric",
        "rfe_metrics.csv"
      )
    )
    
    readr::write_csv2(
      data.frame(
        selection_method = selection_method,
        pred_sel = lrfepred,
        is_fixed_predictor = lrfepred %in% fixed_predictors_present
      ),
      file = file.path(
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
    
    model_ctrl <- make_train_control(
      method = "repeatedcv",
      number = model_fold_use,
      repeats = model_repeat,
      index = gkfold_model,
      summary_function = pst_res_class_multiclass,
      save_predictions = TRUE,
      allow_parallel = allow_parallel_model
    )
    
    model_ctrl$classProbs <- class_probs
    
    formu <- stats::reformulate(
      termlabels = predictor_names_selected,
      response = target_var
    )
    
    set.seed(nseed)
    
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
      dplyr::select(
        dplyr::all_of(predictor_names_selected)
      )
    
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
      file = file.path(
        path_results,
        "performance/imp_pred",
        "imp_pred.csv"
      )
    )
    
    readr::write_csv2(
      pred_obs_full,
      file = file.path(
        path_results,
        "performance/pred_obs",
        "pred_obs_full.csv"
      )
    )
    
    readr::write_csv2(
      as.data.frame(conf_matrix_full$table),
      file = file.path(
        path_results,
        "performance/conf_mat",
        paste0(target_var, "_conf_matrix_full.csv")
      )
    )
    
    saveRDS(
      object = conf_matrix_full,
      file = file.path(
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
      file = file.path(
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
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
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
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
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
    
    if (save_workspace) save.image(
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

close_parallel_backend(cl)
