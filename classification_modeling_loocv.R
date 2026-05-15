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
  "ggplot2"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

path_raiz <- "//200.235.173.229/dados_processamento/cassio/R/atlas_qf"
setwd(path_raiz)

# parameterization
cut_off_mc <- 0.9
fold_results <- "results_nested_class_sentinel_5m"

run_rfe <- TRUE

# predictors that must always remain in the model
# use character(0) when no fixed predictors are needed
fixed_predictors <- character(0)

# auxiliary grouping column
# in this backup version, this column is only excluded from predictors
# it does not change the nested LOOCV structure
kfold <- FALSE
col_kfold <- character(0)

# recursive feature elimination parameters
rfe_size <- seq(2, 72, 2)
rfe_tn_length <- 1
tolerance <- FALSE
tol_per <- 1

# model parameters
model_tn_length <- 10
metric_otm <- "kappa"
maxim <- TRUE
class_probs <- FALSE

# functions
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

# list of models
models <- c("rf", "kknn", "svmRadialSigma", "C5.0", "gbm_custom")

# factor variables
varfact <- c("class") %>%
  sort()

# dummy variables
dummy_vars_raw <- character(0)

# read base dataset
dfbase <- safe_read("./extract_xy/yx_class_sentinel_5m.csv")

varsy <- names(dfbase)[1]

# parallel cluster setup
cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(cores)
cl <- parallelly::autoStopCluster(cl)

for (i in seq_along(models)) {
  
  tmodel <- Sys.time()
  
  for (j in seq_along(varsy)) {
    
    tvar <- Sys.time()
    target_var <- varsy[j]
    
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
      na.omit()
    
    if (has_kfold_col) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::all_of(kfold_col_present)
        )
    }
    
    fixed_predictors_present <- intersect(fixed_predictors_present, names(dyx_sel))
    
    dyx_sel[[target_var]] <- droplevels(dyx_sel[[target_var]])
    
    class_levels <- levels(dyx_sel[[target_var]])
    
    class_count <- table(dyx_sel[[target_var]])
    
    if (any(class_count < 3)) {
      stop(
        paste0(
          "Nested LOOCV classification requires at least 3 observations per class. ",
          "The following classes have fewer than 3 observations: ",
          paste(names(class_count[class_count < 3]), collapse = ", ")
        )
      )
    }
    
    vars_fact_present <- intersect(vars_fact_present, names(dyx_sel))
    
    vars_factor_all <- unique(
      c(target_var, vars_fact_present)
    )
    
    dummy_vars_present <- intersect(dummy_vars_present, names(dyx_sel))
    
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
    
    fixed_predictors_present <- intersect(fixed_predictors_present, names(dyx_sel))
    
    n_samples <- nrow(dyx_sel)
    
    pred_obs_test <- data.frame(
      pred = rep(NA_character_, n_samples),
      obs = rep(NA_character_, n_samples),
      stringsAsFactors = FALSE
    )
    
    dfperf_train <- tibble::tibble(
      model = rep(NA_character_, n_samples),
      target_var = rep(NA_character_, n_samples),
      n_train = rep(NA_integer_, n_samples),
      kappa_train = rep(NA_real_, n_samples),
      accuracy_train = rep(NA_real_, n_samples),
      sensitivity_train = rep(NA_real_, n_samples),
      specificity_train = rep(NA_real_, n_samples),
      precision_train = rep(NA_real_, n_samples),
      recall_train = rep(NA_real_, n_samples),
      balanced_accuracy_train = rep(NA_real_, n_samples),
      f1_train = rep(NA_real_, n_samples)
    )
    
    dfperf_test <- tibble::tibble(
      model = NA_character_,
      target_var = NA_character_,
      n_test = NA_integer_,
      kappa_test = NA_real_,
      accuracy_test = NA_real_,
      sensitivity_test = NA_real_,
      specificity_test = NA_real_,
      precision_test = NA_real_,
      recall_test = NA_real_,
      balanced_accuracy_test = NA_real_,
      f1_test = NA_real_
    )
    
    set.seed(666)
    nseed <- sample(1:100000, n_samples)
    
    lmodel <- vector("list", n_samples)
    lpredimp <- vector("list", n_samples)
    lrfepred <- vector("list", n_samples)
    lrferes <- vector("list", n_samples)
    lconf_matrix_train <- vector("list", n_samples)
    pred_obs_list <- vector("list", n_samples)
    
    model_list_progress_file <- file.path(
      path_results,
      "model",
      paste0(target_var, "_model_fit_list_progress.rds")
    )
    
    model_list_full_file <- file.path(
      path_results,
      "model",
      paste0(target_var, "_model_fit_list_full.rds")
    )
    
    pred_obs_progress_file <- file.path(
      path_results,
      "performance/pred_obs",
      paste0(target_var, "_pred_obs_progress.csv")
    )
    
    model_progress_file <- file.path(
      path_results,
      "model",
      paste0(target_var, "_model_bundle_progress.rds")
    )
    
    start_run <- 1
    
    if (file.exists(model_progress_file)) {
      
      progress_obj <- readRDS(model_progress_file)
      
      progress_is_compatible <- TRUE
      
      if (is.null(progress_obj$run_rfe)) {
        progress_is_compatible <- FALSE
      } else {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$run_rfe, run_rfe)
      }
      
      if (!is.null(progress_obj$fixed_predictors)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$fixed_predictors, fixed_predictors)
      }
      
      if (!is.null(progress_obj$kfold)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$kfold, kfold)
      }
      
      if (!is.null(progress_obj$col_kfold)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$col_kfold, col_kfold)
      }
      
      if (!is.null(progress_obj$target_var)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$target_var, target_var)
      }
      
      if (!is.null(progress_obj$model_name)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$model_name, models[i])
      }
      
      if (!is.null(progress_obj$n_samples)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$n_samples, n_samples)
      }
      
      if (!is.null(progress_obj$class_probs)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$class_probs, class_probs)
      }
      
      if (!is.null(progress_obj$dfperf_train)) {
        progress_is_compatible <- progress_is_compatible &&
          nrow(progress_obj$dfperf_train) == n_samples
      }
      
      if (!is.null(progress_obj$pred_obs_test)) {
        progress_is_compatible <- progress_is_compatible &&
          nrow(progress_obj$pred_obs_test) == n_samples
      }
      
      if (!is.null(progress_obj$pred_obs_list)) {
        progress_is_compatible <- progress_is_compatible &&
          length(progress_obj$pred_obs_list) == n_samples
      }
      
      if (progress_is_compatible) {
        
        if (!is.null(progress_obj$lmodel)) {
          lmodel <- progress_obj$lmodel
        }
        
        if (!is.null(progress_obj$lpredimp)) {
          lpredimp <- progress_obj$lpredimp
        }
        
        if (!is.null(progress_obj$lrfepred)) {
          lrfepred <- progress_obj$lrfepred
        }
        
        if (!is.null(progress_obj$lrferes)) {
          lrferes <- progress_obj$lrferes
        }
        
        if (!is.null(progress_obj$lconf_matrix_train)) {
          lconf_matrix_train <- progress_obj$lconf_matrix_train
        }
        
        if (!is.null(progress_obj$dfperf_train)) {
          dfperf_train <- progress_obj$dfperf_train
        }
        
        if (!is.null(progress_obj$dfperf_test)) {
          dfperf_test <- progress_obj$dfperf_test
        }
        
        if (!is.null(progress_obj$pred_obs_test)) {
          pred_obs_test <- progress_obj$pred_obs_test
        }
        
        if (!is.null(progress_obj$pred_obs_list)) {
          pred_obs_list <- progress_obj$pred_obs_list
        }
        
        completed_runs <- which(!is.na(dfperf_train$model))
        
        if (length(completed_runs) > 0) {
          start_run <- max(completed_runs) + 1
        }
        
        cat(
          sprintf(
            "resuming from run %d/%d | model: %s | variable: %s\n",
            start_run,
            n_samples,
            models[i],
            target_var
          )
        )
        
      } else {
        
        cat(
          sprintf(
            "existing progress is incompatible and will be ignored | model: %s | variable: %s\n",
            models[i],
            target_var
          )
        )
        
      } # end if progress_is_compatible
      
    } # end if progress file
    
    if (start_run > n_samples) {
      
      cat(
        sprintf(
          "all runs already completed for variable %s | model: %s\n",
          target_var,
          models[i]
        )
      )
      
    } else {
      
      for (n in start_run:n_samples) {
        
        trun <- Sys.time()
        
        cat(
          sprintf(
            "run %d/%d | model: %s | variable: %s\n",
            n,
            n_samples,
            models[i],
            target_var
          )
        )
        
        train <- droplevels(dyx_sel[-n, , drop = FALSE])
        test <- dyx_sel[n, , drop = FALSE]
        
        test[[target_var]] <- factor(
          as.character(test[[target_var]]),
          levels = class_levels
        )
        
        registerDoParallel(cl)
        
        all_predictors_after_filter <- setdiff(
          names(train),
          target_var
        )
        
        if (length(all_predictors_after_filter) == 0) {
          stop(
            paste0(
              "No predictors available after NZV and correlation filtering for variable: ",
              target_var,
              " | run: ",
              n
            )
          )
        }
        
        fixed_predictors_run <- intersect(
          fixed_predictors_present,
          names(train)
        )
        
        rfe_candidate_predictors <- setdiff(
          all_predictors_after_filter,
          fixed_predictors_run
        )
        
        if (run_rfe && length(rfe_candidate_predictors) > 0) {
          
          cat("RFE enabled: running recursive feature elimination\n")
          
          rfe_data <- train %>%
            dplyr::select(
              dplyr::all_of(c(target_var, rfe_candidate_predictors))
            )
          
          rfe_size_valid <- sort(
            unique(
              rfe_size[rfe_size <= length(rfe_candidate_predictors)]
            )
          )
          
          if (length(rfe_size_valid) == 0) {
            rfe_size_valid <- length(rfe_candidate_predictors)
          }
          
          set.seed(nseed[n])
          rfe_ctrl <- rfeControl(
            functions = custom_rcaretFuncs,
            method = "LOOCV",
            verbose = FALSE,
            allowParallel = TRUE
          )
          
          set.seed(nseed[n])
          rfe_model_ctrl <- trainControl(
            method = "LOOCV",
            savePredictions = TRUE,
            summaryFunction = pst_res_class_multiclass,
            classProbs = class_probs,
            allowParallel = TRUE
          )
          
          formu <- stats::reformulate(
            termlabels = rfe_candidate_predictors,
            response = target_var
          )
          
          set.seed(nseed[n])
          rfe_fit <- rfe(
            form = formu,
            data = rfe_data,
            sizes = rfe_size_valid,
            method = models[i],
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
              "time for rfe - model: %s | variable: %s | run: %d | duration: %.2f %s\n",
              models[i],
              target_var,
              n,
              Sys.time() - trun,
              units(Sys.time() - trun)
            )
          )
          
          lrferes[[n]] <- if (!is.null(rfe_fit$results)) {
            rfe_fit$results
          } else {
            rfe_fit$result
          }
          
          if (tolerance) {
            
            pick <- caret::pickSizeTolerance(
              x = lrferes[[n]],
              metric = metric_otm,
              tol = tol_per,
              maximize = maxim
            )
            
            lrfepred[[n]] <- rfe_fit$optVariables[1:pick]
            
          } else {
            
            lrfepred[[n]] <- rfe_fit$optVariables
            
          } # end if tolerance
          
          cat(
            sprintf(
              "rfe select: %d variables\n",
              length(lrfepred[[n]])
            )
          )
          
          if (length(varfact) > 0) {
            
            original_predictor_names <- names(rfe_data)
            
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
            
            lrfepred[[n]] <- unique(
              vapply(
                lrfepred[[n]],
                map_factor_back,
                FUN.VALUE = character(1),
                factor_vars = varfact,
                original_names = original_predictor_names
              )
            )
            
          } # end if varfact
          
          rfe_selected_predictors <- lrfepred[[n]]
          
          lrfepred[[n]] <- unique(
            c(fixed_predictors_run, rfe_selected_predictors)
          )
          
          selection_method <- ifelse(
            length(fixed_predictors_run) > 0,
            "rfe_plus_fixed",
            "rfe"
          )
          
        } else if (run_rfe && length(rfe_candidate_predictors) == 0) {
          
          cat("RFE enabled, but no candidate predictors are available after filters. Using fixed predictors only\n")
          
          rfe_fit <- NULL
          
          lrfepred[[n]] <- fixed_predictors_run
          
          lrferes[[n]] <- tibble::tibble(
            selection_method = "fixed_only_no_rfe_candidates",
            target_var = target_var,
            model = models[i],
            run = n,
            n_predictors = length(lrfepred[[n]]),
            predictors = paste(lrfepred[[n]], collapse = ";")
          )
          
          selection_method <- "fixed_only"
          
          cat(
            sprintf(
              "fixed predictors selected: %d variables\n",
              length(lrfepred[[n]])
            )
          )
          
        } else {
          
          cat("RFE disabled: using all predictors after NZV and correlation filtering\n")
          
          rfe_fit <- NULL
          
          lrfepred[[n]] <- all_predictors_after_filter
          
          lrferes[[n]] <- tibble::tibble(
            selection_method = "no_rfe_after_nzv_and_correlation",
            target_var = target_var,
            model = models[i],
            run = n,
            n_predictors = length(lrfepred[[n]]),
            predictors = paste(lrfepred[[n]], collapse = ";")
          )
          
          selection_method <- "no_rfe"
          
          cat(
            sprintf(
              "predictors selected without RFE: %d variables\n",
              length(lrfepred[[n]])
            )
          )
          
        } # end if run_rfe
        
        cat("-----------------------------------------------------------\n")
        
        if (length(lrfepred[[n]]) == 0) {
          stop(
            paste0(
              "No predictors were selected for variable: ",
              target_var,
              " | run: ",
              n
            )
          )
        }
        
        readr::write_csv2(
          data.frame(lrferes[[n]]),
          file = file.path(
            path_results,
            "select/rfe/metric",
            paste0("rfe_metrics_", n, ".csv")
          )
        )
        
        readr::write_csv2(
          data.frame(
            selection_method = selection_method,
            pred_sel = lrfepred[[n]],
            is_fixed_predictor = lrfepred[[n]] %in% fixed_predictors_run
          ),
          file = file.path(
            path_results,
            "select/rfe/select",
            paste0("rfe_pred_sel_", n, ".csv")
          )
        )
        
        dfselrfe <- train %>%
          dplyr::select(
            dplyr::all_of(c(target_var, lrfepred[[n]]))
          )
        
        test_sel <- test %>%
          dplyr::select(
            dplyr::all_of(c(target_var, lrfepred[[n]]))
          )
        
        predictor_names_selected <- setdiff(names(dfselrfe), target_var)
        
        if (length(predictor_names_selected) == 0) {
          stop(
            paste0(
              "No predictors available in dfselrfe for variable: ",
              target_var,
              " | run: ",
              n
            )
          )
        }
        
        set.seed(nseed[n])
        model_ctrl <- trainControl(
          method = "LOOCV",
          savePredictions = TRUE,
          summaryFunction = pst_res_class_multiclass,
          classProbs = class_probs,
          allowParallel = TRUE
        )
        
        formu <- stats::reformulate(
          termlabels = predictor_names_selected,
          response = target_var
        )
        
        set.seed(nseed[n])
        registerDoParallel(cl)
        
        model_fit <- train(
          form = formu,
          data = dfselrfe,
          metric = metric_otm,
          importance = TRUE,
          maximize = maxim,
          method = models[i],
          trControl = model_ctrl,
          tuneLength = model_tn_length
        )
        
        print(model_fit)
        cat("\n-----------------------------------------------------------\n")
        
        cat(
          sprintf(
            "%s | variable: %s | run: %d | duration: %.2f %s\n",
            model_fit[["modelInfo"]][["label"]],
            target_var,
            n,
            Sys.time() - trun,
            units(Sys.time() - trun)
          )
        )
        
        cat("-----------------------------------------------------------\n")
        
        lmodel[[n]] <- model_fit
        
        pred_obs_train <- data.frame(
          pred = as.character(predict(lmodel[[n]], dfselrfe)),
          obs = as.character(dfselrfe[[target_var]]),
          data = "train",
          stringsAsFactors = FALSE
        ) %>%
          na.omit()
        
        pred_obs_train$pred <- factor(
          pred_obs_train$pred,
          levels = class_levels
        )
        
        pred_obs_train$obs <- factor(
          pred_obs_train$obs,
          levels = class_levels
        )
        
        pr_train <- getTrainPerf(lmodel[[n]])
        
        lconf_matrix_train[[n]] <- caret::confusionMatrix(
          data = pred_obs_train$pred,
          reference = pred_obs_train$obs,
          mode = "everything"
        )
        
        pred_test_current <- as.character(
          predict(lmodel[[n]], test_sel)[1]
        )
        
        obs_test_current <- as.character(
          test_sel[[target_var]][1]
        )
        
        pred_obs_test$pred[n] <- pred_test_current
        pred_obs_test$obs[n] <- obs_test_current
        
        pred_obs_test_current <- data.frame(
          pred = pred_test_current,
          obs = obs_test_current,
          data = "test",
          stringsAsFactors = FALSE
        )
        
        pred_imp <- caret::varImp(
          object = lmodel[[n]],
          scale = TRUE
        )
        
        lpredimp[[n]] <- pred_imp %>%
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
        
        lpredimp_top <- lpredimp[[n]] %>%
          dplyr::group_by(class) %>%
          dplyr::slice_max(
            order_by = importance,
            n = 15,
            with_ties = FALSE
          ) %>%
          dplyr::ungroup()
        
        gg <- ggplot(
          lpredimp_top,
          aes(
            y = reorder(predictor, importance),
            x = importance
          )
        ) +
          geom_col() +
          facet_wrap(~class, scales = "free_x")
        
        print(gg)
        
        pred_obs_fulldata <- dplyr::bind_rows(
          pred_obs_train,
          pred_obs_test_current
        ) %>%
          dplyr::mutate(
            data = factor(data, levels = c("train", "test")),
            run = n
          )
        
        pred_obs_list[[n]] <- pred_obs_fulldata
        
        readr::write_csv2(
          lpredimp[[n]],
          file = file.path(
            path_results,
            "performance/imp_pred",
            paste0("imp_pred_", n, ".csv")
          )
        )
        
        dfperf_train[n, ] <- list(
          model = lmodel[[n]]$modelInfo$label,
          target_var = target_var,
          n_train = nrow(dfselrfe),
          kappa_train = as.numeric(pr_train$Trainkappa),
          accuracy_train = as.numeric(pr_train$Trainaccuracy),
          sensitivity_train = as.numeric(pr_train$Trainsensitivity),
          specificity_train = as.numeric(pr_train$Trainspecificity),
          precision_train = as.numeric(pr_train$Trainprecision),
          recall_train = as.numeric(pr_train$Trainrecall),
          balanced_accuracy_train = as.numeric(pr_train$Trainbalanced_accuracy),
          f1_train = as.numeric(pr_train$Trainf1_score)
        )
        
        readr::write_csv2(
          dfperf_train,
          file = file.path(
            path_results,
            "performance/csv",
            paste0(target_var, "_performance_train.csv")
          )
        )
        
        pred_obs_progress <- pred_obs_test %>%
          dplyr::filter(!is.na(obs) & !is.na(pred))
        
        readr::write_csv2(
          pred_obs_progress,
          pred_obs_progress_file
        )
        
        saveRDS(
          object = lmodel,
          file = model_list_progress_file
        )
        
        saveRDS(
          object = list(
            lmodel = lmodel,
            lpredimp = lpredimp,
            lrfepred = lrfepred,
            lrferes = lrferes,
            lconf_matrix_train = lconf_matrix_train,
            dfperf_train = dfperf_train,
            dfperf_test = dfperf_test,
            pred_obs_test = pred_obs_test,
            pred_obs_list = pred_obs_list,
            run_rfe = run_rfe,
            fixed_predictors = fixed_predictors,
            fixed_predictors_present = fixed_predictors_present,
            kfold = kfold,
            col_kfold = col_kfold,
            last_completed_run = n,
            target_var = target_var,
            model_name = models[i],
            n_samples = n_samples,
            class_probs = class_probs,
            class_levels = class_levels
          ),
          file = model_progress_file
        )
        
        save.image(
          file.path(
            path_results,
            "img",
            paste0(target_var, ".rdata")
          )
        )
        
        gc()
        
      } # end for n
      
    } # end if start_run
    
    model_label_final <- if (length(lmodel) > 0 && any(!vapply(lmodel, is.null, logical(1)))) {
      lmodel[[which(!vapply(lmodel, is.null, logical(1)))[1]]]$modelInfo$label
    } else {
      models[i]
    }
    
    pred_obs_test_complete <- pred_obs_test %>%
      dplyr::filter(!is.na(obs) & !is.na(pred))
    
    if (nrow(pred_obs_test_complete) == 0) {
      stop(
        paste0(
          "No completed outer predictions were available for variable: ",
          target_var,
          " | model: ",
          models[i]
        )
      )
    }
    
    pred_obs_test_complete$pred <- factor(
      pred_obs_test_complete$pred,
      levels = class_levels
    )
    
    pred_obs_test_complete$obs <- factor(
      pred_obs_test_complete$obs,
      levels = class_levels
    )
    
    pr_test <- pst_res_class_multiclass(pred_obs_test_complete)
    
    conf_matrix_test <- caret::confusionMatrix(
      data = pred_obs_test_complete$pred,
      reference = pred_obs_test_complete$obs,
      mode = "everything"
    )
    
    dfperf_test[1, ] <- list(
      model = model_label_final,
      target_var = target_var,
      n_test = nrow(pred_obs_test_complete),
      kappa_test = as.numeric(pr_test["kappa"]),
      accuracy_test = as.numeric(pr_test["accuracy"]),
      sensitivity_test = as.numeric(pr_test["sensitivity"]),
      specificity_test = as.numeric(pr_test["specificity"]),
      precision_test = as.numeric(pr_test["precision"]),
      recall_test = as.numeric(pr_test["recall"]),
      balanced_accuracy_test = as.numeric(pr_test["balanced_accuracy"]),
      f1_test = as.numeric(pr_test["f1_score"])
    )
    
    readr::write_csv2(
      dfperf_test,
      file = file.path(
        path_results,
        "performance/csv",
        paste0(target_var, "_performance_test.csv")
      )
    )
    
    readr::write_csv2(
      pred_obs_test_complete,
      file = file.path(
        path_results,
        "performance/pred_obs",
        paste0(target_var, "_pred_obs_test.csv")
      )
    )
    
    saveRDS(
      object = lconf_matrix_train,
      file = file.path(
        path_results,
        "performance/conf_mat",
        paste0(target_var, "_conf_matrix_train_list.rds")
      )
    )
    
    saveRDS(
      object = conf_matrix_test,
      file = file.path(
        path_results,
        "performance/conf_mat",
        paste0(target_var, "_conf_matrix_test.rds")
      )
    )
    
    lrfepred_non_null <- lrfepred[!vapply(lrfepred, is.null, logical(1))]
    
    if (length(lrfepred_non_null) > 0) {
      
      n_obs <- sapply(lrfepred_non_null, length)
      seq_max <- seq_len(max(n_obs))
      
      rfe_pred_full <- as.data.frame(
        sapply(lrfepred_non_null, "[", i = seq_max)
      )
      
      readr::write_csv2(
        rfe_pred_full,
        file = file.path(
          path_results,
          "select/rfe/select",
          "select_full.csv"
        )
      )
      
    } # end if lrfepred_non_null
    
    lrferes_non_null <- lrferes[!vapply(lrferes, is.null, logical(1))]
    
    if (length(lrferes_non_null) > 0) {
      
      run_ids_lrferes <- which(!vapply(lrferes, is.null, logical(1)))
      
      rfe_res_full <- purrr::map2_dfr(
        lrferes_non_null,
        run_ids_lrferes,
        function(x, run_id) {
          dplyr::mutate(
            as.data.frame(x),
            rep = run_id
          )
        }
      ) %>%
        dplyr::relocate(rep)
      
      readr::write_csv2(
        rfe_res_full,
        file = file.path(
          path_results,
          "select/rfe/metric",
          "rfe_metrics_full.csv"
        )
      )
      
    } # end if lrferes_non_null
    
    lpredimp_non_null <- lpredimp[!vapply(lpredimp, is.null, logical(1))]
    
    if (length(lpredimp_non_null) > 0) {
      
      run_ids_lpredimp <- which(!vapply(lpredimp, is.null, logical(1)))
      
      pred_imp_full <- purrr::map2_dfr(
        lpredimp_non_null,
        run_ids_lpredimp,
        function(x, run_id) {
          dplyr::mutate(
            x,
            rep = run_id
          )
        }
      ) %>%
        dplyr::relocate(rep) %>%
        `rownames<-`(NULL)
      
      readr::write_csv2(
        pred_imp_full,
        file = file.path(
          path_results,
          "performance/imp_pred",
          "imp_pred_full.csv"
        )
      )
      
    } # end if lpredimp_non_null
    
    pred_obs_list_non_null <- pred_obs_list[!vapply(pred_obs_list, is.null, logical(1))]
    
    if (length(pred_obs_list_non_null) > 0) {
      
      pred_obs_full <- dplyr::bind_rows(pred_obs_list_non_null)
      
      readr::write_csv2(
        pred_obs_full,
        file = file.path(
          path_results,
          "performance/pred_obs",
          "pred_obs_full.csv"
        )
      )
      
    } else {
      
      pred_obs_full <- data.frame()
      
    } # end if pred_obs_list_non_null
    
    saveRDS(
      object = lmodel,
      file = model_list_full_file
    )
    
    saveRDS(
      object = list(
        lmodel = lmodel,
        lpredimp = lpredimp,
        lrfepred = lrfepred,
        lrferes = lrferes,
        lconf_matrix_train = lconf_matrix_train,
        conf_matrix_test = conf_matrix_test,
        dfperf_train = dfperf_train,
        dfperf_test = dfperf_test,
        pred_obs_test = pred_obs_test_complete,
        pred_obs_list = pred_obs_list,
        pred_obs_full = pred_obs_full,
        run_rfe = run_rfe,
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
        target_var = target_var,
        model_name = models[i],
        n_samples = n_samples,
        class_probs = class_probs,
        class_levels = class_levels
      ),
      file = file.path(
        path_results,
        "model",
        paste0(target_var, "_model_bundle_full.rds")
      )
    )
    
    cat(
      sprintf(
        "total time for variable %s | model: %s | duration: %.2f %s\n",
        target_var,
        model_label_final,
        Sys.time() - tvar,
        units(Sys.time() - tvar)
      )
    )
    
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
