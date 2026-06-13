source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/utils/install_load_pkg.R"
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
  "tibble",
  "ggplot2",
  "foreach"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

path_raiz <- "//200.235.173.96/dados_processamento/cassio/R/atlas_qf"
setwd(path_raiz)

# source functions

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/prediction/pst_res_mqi.R"
)

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/modeling/helpers/caret_rfe_functions.R"
)

# parameterization

base_seed <- 666
cut_off_mc <- 0.95
fold_results <- "results_nested_sentinel_ecos_5m_final_adapt"

row_limit <- 20
# Use row_limit <- Inf for the full run.

run_rfe <- TRUE

# predictors that must always remain in the model
# use character(0) when no fixed predictors are needed

fixed_predictors <- character(0)

# target variable filter
# TRUE keeps only target values >= 0
# FALSE allows negative target values

filter_target_non_negative <- TRUE

# grouped validation settings
# FALSE runs nested LOOCV by sample
# TRUE runs nested leave-one-group-out using col_kfold

kfold <- FALSE
col_kfold <- character(0)

# recursive feature elimination parameters

rfe_size <- 2
# Use rfe_size <- seq(2, 41, 2) for the full RFE grid.

rfe_tn_length <- 1

tolerance <- FALSE
tol_per <- 2

# model parameters

model_tn_length <- 10

metric_otm <- "mqi"
maxim <- TRUE

# parallel parameters

use_parallel <- TRUE
allow_parallel_rfe <- TRUE
allow_parallel_model <- TRUE

cores <- 15
# Use cores <- max(1, parallel::detectCores() - 1) if preferred.

parallel_packages <- c(
  "caret",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "readr",
  "stringr",
  "quantregForest",
  "DescTools",
  "foreach"
)

# caret functions

custom_rcaretFuncs <- make_rcaret_funcs(
  metric_value = metric_otm,
  summary_function = pst_res_mqi
)

qrf_mean <- make_qrf_mean_model()

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

make_leave_one_group_out_index <- function(group) {
  
  group <- as.factor(group)
  group <- droplevels(group)
  
  groups <- levels(group)
  
  if (length(groups) < 2) {
    stop("At least two groups are required for internal leave-one-group-out validation.")
  }
  
  index_list <- lapply(
    groups,
    function(current_group) {
      which(group != current_group)
    }
  )
  
  names(index_list) <- paste0("Group_", seq_along(groups))
  
  index_list
}

# list of models

models <- c("qrf")

# factor variables

varfact <- character(0)
# Use varfact <- c("id") %>% sort() when needed.

# dummy variables

dummy_vars_raw <- character(0)

# read base dataset

dfbase <- safe_read_csv2(
  "./extract_xy/ecos_5m_final.csv"
)

if (is.finite(row_limit) && nrow(dfbase) > row_limit) {
  dfbase <- dfbase %>%
    dplyr::slice_head(n = row_limit)
}

varsy <- names(dfbase)[1:3]

# parallel cluster setup

parallel_export_objects <- c(
  "metric_otm",
  "pst_res_mqi",
  "custom_rcaretFuncs",
  "qrf_mean"
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
    
    if (filter_target_non_negative) {
      dyx_sel <- dyx_sel %>%
        dplyr::filter(
          .data[[target_var]] >= 0
        )
    }
    
    check_regression_target(
      data = dyx_sel,
      target_var = target_var,
      stage = "after_target_filter"
    )
    
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
          "The grouping column cannot be used as a fixed predictor and will be ignored as predictor: ",
          paste(kfold_col_present, collapse = ", ")
        )
      )
    }
    
    vars_numeric_to_clean <- names(dyx_sel)[
      vapply(dyx_sel, is.numeric, logical(1))
    ] %>%
      setdiff(c(vars_fact_present, kfold_col_present))
    
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
          .cols = dplyr::all_of(vars_fact_present),
          .fns = as.factor
        )
      ) %>%
      tidyr::drop_na()
    
    check_regression_target(
      data = dyx_sel,
      target_var = target_var,
      stage = "after_numeric_cleaning"
    )
    
    if (!kfold && has_kfold_col) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::all_of(kfold_col_present)
        )
      
      kfold_col_present <- character(0)
    }
    
    fixed_predictors_present <- intersect(fixed_predictors_present, names(dyx_sel))
    
    validation_mode <- if (kfold) {
      "nested_leave_one_group_out"
    } else {
      "nested_leave_one_sample_out"
    }
    
    outer_units <- if (kfold) {
      unique(dyx_sel[[col_kfold]])
    } else {
      seq_len(nrow(dyx_sel))
    }
    
    n_units <- length(outer_units)
    
    if (!kfold && n_units < 3) {
      stop("At least three samples are required for nested LOOCV.")
    }
    
    if (kfold && n_units < 3) {
      stop("At least three groups are required for nested leave-one-group-out validation.")
    }
    
    dfperf_train <- tibble::tibble(
      outer_unit = seq_len(n_units),
      test_group = rep(NA_character_, n_units),
      model = rep(NA_character_, n_units),
      target_var = rep(NA_character_, n_units),
      n_train = rep(NA_integer_, n_units),
      ccc_train = rep(NA_real_, n_units),
      r2_train = rep(NA_real_, n_units),
      nse_train = rep(NA_real_, n_units),
      mae_train = rep(NA_real_, n_units),
      rmse_train = rep(NA_real_, n_units),
      mqi_train = rep(NA_real_, n_units),
      mae_null_train = rep(NA_real_, n_units),
      rmse_null_train = rep(NA_real_, n_units)
    )
    
    dfperf_test <- tibble::tibble(
      model = NA_character_,
      target_var = NA_character_,
      validation_mode = validation_mode,
      n_test = NA_integer_,
      ccc_test = NA_real_,
      r2_test = NA_real_,
      nse_test = NA_real_,
      mae_test = NA_real_,
      rmse_test = NA_real_,
      mqi_test = NA_real_,
      mae_null_test = NA_real_,
      rmse_null_test = NA_real_
    )
    
    set.seed(base_seed)
    run_seeds <- sample(1:100000, n_units)
    
    lmodel <- vector("list", n_units)
    lpredimp <- vector("list", n_units)
    lrfepred <- vector("list", n_units)
    lrferes <- vector("list", n_units)
    lnzv <- vector("list", n_units)
    lcor <- vector("list", n_units)
    pred_obs_list <- vector("list", n_units)
    pred_obs_test_list <- vector("list", n_units)
    
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
      
      if (!is.null(progress_obj$validation_mode)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$validation_mode, validation_mode)
      }
      
      if (!is.null(progress_obj$run_rfe)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$run_rfe, run_rfe)
      }
      
      if (!is.null(progress_obj$filter_target_non_negative)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$filter_target_non_negative, filter_target_non_negative)
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
      
      if (!is.null(progress_obj$metric_otm)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$metric_otm, metric_otm)
      }
      
      if (!is.null(progress_obj$target_var)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$target_var, target_var)
      }
      
      if (!is.null(progress_obj$model_name)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$model_name, models[i])
      }
      
      if (!is.null(progress_obj$n_units)) {
        progress_is_compatible <- progress_is_compatible &&
          identical(progress_obj$n_units, n_units)
      }
      
      if (!is.null(progress_obj$dfperf_train)) {
        progress_is_compatible <- progress_is_compatible &&
          nrow(progress_obj$dfperf_train) == n_units
      } else {
        progress_is_compatible <- FALSE
      }
      
      if (!is.null(progress_obj$pred_obs_list)) {
        progress_is_compatible <- progress_is_compatible &&
          length(progress_obj$pred_obs_list) == n_units
      } else {
        progress_is_compatible <- FALSE
      }
      
      if (!is.null(progress_obj$pred_obs_test_list)) {
        progress_is_compatible <- progress_is_compatible &&
          length(progress_obj$pred_obs_test_list) == n_units
      } else {
        progress_is_compatible <- FALSE
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
        
        if (!is.null(progress_obj$lnzv)) {
          lnzv <- progress_obj$lnzv
        }
        
        if (!is.null(progress_obj$lcor)) {
          lcor <- progress_obj$lcor
        }
        
        if (!is.null(progress_obj$dfperf_train)) {
          dfperf_train <- progress_obj$dfperf_train
        }
        
        if (!is.null(progress_obj$dfperf_test)) {
          dfperf_test <- progress_obj$dfperf_test
        }
        
        if (!is.null(progress_obj$pred_obs_list)) {
          pred_obs_list <- progress_obj$pred_obs_list
        }
        
        if (!is.null(progress_obj$pred_obs_test_list)) {
          pred_obs_test_list <- progress_obj$pred_obs_test_list
        }
        
        completed_runs <- which(!is.na(dfperf_train$model))
        
        if (length(completed_runs) > 0) {
          start_run <- max(completed_runs) + 1
        }
        
        cat(
          sprintf(
            "resuming from run %d/%d | model: %s | variable: %s | mode: %s\n",
            start_run,
            n_units,
            models[i],
            target_var,
            validation_mode
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
    
    if (start_run > n_units) {
      
      cat(
        sprintf(
          "all runs already completed for variable %s | model: %s | mode: %s\n",
          target_var,
          models[i],
          validation_mode
        )
      )
      
    } else {
      
      for (n in start_run:n_units) {
        
        trun <- Sys.time()
        
        cat(
          sprintf(
            "run %d/%d | model: %s | variable: %s | mode: %s\n",
            n,
            n_units,
            models[i],
            target_var,
            validation_mode
          )
        )
        
        if (kfold) {
          
          test_group <- outer_units[n]
          
          train_raw <- dyx_sel %>%
            dplyr::filter(
              !.data[[col_kfold]] %in% test_group
            )
          
          test_raw <- dyx_sel %>%
            dplyr::filter(
              .data[[col_kfold]] %in% test_group
            )
          
          inner_index <- make_leave_one_group_out_index(
            train_raw[[col_kfold]]
          )
          
          inner_method <- "cv"
          inner_number <- length(inner_index)
          
        } else {
          
          test_group <- NA_character_
          
          train_raw <- dyx_sel[-n, , drop = FALSE]
          test_raw <- dyx_sel[n, , drop = FALSE]
          
          inner_index <- NULL
          inner_method <- "LOOCV"
          inner_number <- nrow(train_raw)
          
        } # end if kfold
        
        check_regression_target(
          data = train_raw,
          target_var = target_var,
          stage = paste0("train_raw_run_", n)
        )
        
        check_regression_target(
          data = test_raw,
          target_var = target_var,
          stage = paste0("test_raw_run_", n)
        )
        
        nzv_candidates <- setdiff(
          names(train_raw),
          c(
            target_var,
            dummy_vars_present,
            fixed_predictors_present,
            kfold_col_present
          )
        )
        
        if (length(nzv_candidates) > 0) {
          nzv_vars <- caret::nearZeroVar(
            train_raw[, nzv_candidates, drop = FALSE],
            names = TRUE
          )
        } else {
          nzv_vars <- character(0)
        }
        
        lnzv[[n]] <- tibble::tibble(
          outer_unit = n,
          test_group = as.character(test_group),
          nzv_removed = nzv_vars
        )
        
        readr::write_csv2(
          lnzv[[n]],
          file = file.path(
            path_results,
            "select/nzv",
            paste0(target_var, "_nzv_", n, ".csv")
          )
        )
        
        if (length(nzv_vars) > 0) {
          train_raw <- train_raw %>%
            dplyr::select(
              -dplyr::any_of(nzv_vars)
            )
          
          test_raw <- test_raw %>%
            dplyr::select(
              -dplyr::any_of(nzv_vars)
            )
        }
        
        vars_fact_run <- intersect(vars_fact_present, names(train_raw)) %>%
          setdiff(kfold_col_present)
        
        dummy_vars_run <- intersect(dummy_vars_present, names(train_raw))
        
        fixed_predictors_run <- intersect(fixed_predictors_present, names(train_raw)) %>%
          setdiff(kfold_col_present)
        
        cor_candidates <- setdiff(
          names(train_raw),
          c(
            target_var,
            dummy_vars_run,
            vars_fact_run,
            fixed_predictors_run,
            kfold_col_present
          )
        )
        
        cor_candidates <- cor_candidates[
          vapply(train_raw[, cor_candidates, drop = FALSE], is.numeric, logical(1))
        ]
        
        if (length(cor_candidates) >= 2) {
          
          mcor <- train_raw %>%
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
        
        lcor[[n]] <- tibble::tibble(
          outer_unit = n,
          test_group = as.character(test_group),
          cor_removed = fc
        )
        
        readr::write_csv2(
          lcor[[n]],
          file = file.path(
            path_results,
            "select/cor",
            paste0(target_var, "_cor_", n, ".csv")
          )
        )
        
        if (length(fc) > 0) {
          train_raw <- train_raw %>%
            dplyr::select(
              -dplyr::any_of(fc)
            )
          
          test_raw <- test_raw %>%
            dplyr::select(
              -dplyr::any_of(fc)
            )
        }
        
        if (kfold) {
          
          train <- train_raw %>%
            dplyr::select(
              -dplyr::all_of(col_kfold)
            )
          
          test <- test_raw %>%
            dplyr::select(
              -dplyr::all_of(col_kfold)
            )
          
        } else {
          
          train <- train_raw
          test <- test_raw
          
        } # end if kfold remove group column
        
        all_predictors_after_filter <- setdiff(
          names(train),
          target_var
        )
        
        if (length(all_predictors_after_filter) == 0) {
          stop(
            paste0(
              "No predictors available after filters for variable: ",
              target_var,
              " | run: ",
              n
            )
          )
        }
        
        fixed_predictors_run <- intersect(
          fixed_predictors_run,
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
          
          rfe_size_valid <- make_valid_rfe_sizes(
            rfe_size = rfe_size,
            n_predictors = length(rfe_candidate_predictors)
          )
          
          set.seed(run_seeds[n])
          
          rfe_ctrl <- make_rfe_control(
            method = inner_method,
            number = inner_number,
            repeats = NULL,
            index = inner_index,
            functions_object = custom_rcaretFuncs,
            verbose = FALSE,
            allow_parallel = allow_parallel_rfe
          )
          
          set.seed(run_seeds[n])
          
          rfe_model_ctrl <- make_train_control(
            method = inner_method,
            number = inner_number,
            repeats = NULL,
            index = NULL,
            summary_function = pst_res_mqi,
            save_predictions = TRUE,
            allow_parallel = FALSE
          )
          
          formu <- stats::reformulate(
            termlabels = rfe_candidate_predictors,
            response = target_var
          )
          
          set.seed(run_seeds[n])
          
          rfe_fit <- caret::rfe(
            form = formu,
            data = rfe_data,
            sizes = rfe_size_valid,
            method = get_caret_method(models[i]),
            metric = metric_otm,
            maximize = maxim,
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
            
            cat(
              sprintf(
                "rfe select: %d variables\n",
                length(lrfepred[[n]])
              )
            )
            
          } else {
            
            lrfepred[[n]] <- rfe_fit$optVariables
            
            cat(
              sprintf(
                "rfe select: %d variables\n",
                length(lrfepred[[n]])
              )
            )
            
          } # end if tolerance
          
          if (length(varfact) > 0) {
            
            original_predictor_names <- names(rfe_data)
            
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
            outer_unit = n,
            test_group = as.character(test_group),
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
          
          cat("RFE disabled: using all predictors after filters\n")
          
          rfe_fit <- NULL
          
          lrfepred[[n]] <- all_predictors_after_filter
          
          lrferes[[n]] <- tibble::tibble(
            selection_method = "no_rfe_after_nzv_and_correlation",
            target_var = target_var,
            model = models[i],
            outer_unit = n,
            test_group = as.character(test_group),
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
        
        predictor_names_selected <- setdiff(
          names(dfselrfe),
          target_var
        )
        
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
        
        set.seed(run_seeds[n])
        
        model_ctrl <- make_train_control(
          method = inner_method,
          number = inner_number,
          repeats = NULL,
          index = inner_index,
          summary_function = pst_res_mqi,
          save_predictions = TRUE,
          allow_parallel = allow_parallel_model
        )
        
        formu <- stats::reformulate(
          termlabels = predictor_names_selected,
          response = target_var
        )
        
        set.seed(run_seeds[n])
        
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
        
        train_mean <- mean(
          dfselrfe[[target_var]],
          na.rm = TRUE
        )
        
        pred_obs_null_train <- data.frame(
          pred = rep(train_mean, nrow(dfselrfe)),
          obs = dfselrfe[[target_var]]
        )
        
        pr_train <- caret::getTrainPerf(lmodel[[n]])
        
        pr_null_train <- pst_res_mqi(
          pred_obs_null_train
        )
        
        pred_obs_test_current <- tibble::tibble(
          outer_unit = n,
          test_group = as.character(test_group),
          obs = test_sel[[target_var]],
          pred = as.numeric(predict(lmodel[[n]], test_sel)),
          null_pred = train_mean,
          data = "test"
        )
        
        pred_obs_test_list[[n]] <- pred_obs_test_current
        
        pred_imp <- caret::varImp(
          object = lmodel[[n]],
          scale = TRUE
        )
        
        lpredimp[[n]] <- pred_imp$importance %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "predictor")
        
        importance_col <- names(lpredimp[[n]])[
          vapply(lpredimp[[n]], is.numeric, logical(1))
        ][1]
        
        if (length(importance_col) == 0 || is.na(importance_col)) {
          stop("No numeric importance column was returned by varImp.")
        }
        
        lpredimp[[n]] <- lpredimp[[n]] %>%
          dplyr::mutate(
            importance = .data[[importance_col]]
          ) %>%
          dplyr::select(
            predictor,
            importance
          ) %>%
          dplyr::filter(!is.na(importance)) %>%
          dplyr::arrange(
            dplyr::desc(importance)
          )
        
        lpredimp_top <- lpredimp[[n]] %>%
          dplyr::slice_head(n = 15) %>%
          dplyr::arrange(importance)
        
        g1 <- ggplot2::ggplot(
          lpredimp_top,
          ggplot2::aes(
            y = factor(predictor, levels = predictor),
            x = importance
          )
        ) +
          ggplot2::geom_col() +
          ggplot2::labs(
            title = paste0(target_var, " | top predictors"),
            x = "Importance",
            y = NULL
          )
        
        plot(g1)
        
        Sys.sleep(2)
        
        pred_obs_train_current <- tibble::tibble(
          outer_unit = n,
          test_group = as.character(test_group),
          obs = dfselrfe[[target_var]],
          pred = as.numeric(predict(lmodel[[n]], dfselrfe)),
          null_pred = train_mean,
          data = "train"
        )
        
        pred_obs_fulldata <- dplyr::bind_rows(
          pred_obs_train_current,
          pred_obs_test_current
        ) %>%
          dplyr::mutate(
            data = factor(data, levels = c("train", "test"))
          )
        
        pred_obs_list[[n]] <- pred_obs_fulldata
        
        g2 <- ggplot2::ggplot(
          pred_obs_fulldata,
          ggplot2::aes(
            x = obs,
            y = pred
          )
        ) +
          ggplot2::geom_abline(
            col = "red",
            linewidth = 1
          ) +
          ggplot2::geom_point(
            alpha = 0.25
          ) +
          ggplot2::labs(
            title = target_var,
            x = "Observed",
            y = "Predicted"
          ) +
          ggplot2::facet_wrap(
            ~ data,
            scales = "free"
          )
        
        plot(g2)
        
        readr::write_csv2(
          lpredimp[[n]],
          file = file.path(
            path_results,
            "performance/imp_pred",
            paste0("imp_pred_", n, ".csv")
          )
        )
        
        dfperf_train[n, ] <- list(
          outer_unit = n,
          test_group = as.character(test_group),
          model = lmodel[[n]]$modelInfo$label,
          target_var = target_var,
          n_train = nrow(dfselrfe),
          ccc_train = as.numeric(pr_train$Trainccc),
          r2_train = as.numeric(pr_train$Trainr2),
          nse_train = as.numeric(pr_train$Trainnse),
          mae_train = as.numeric(pr_train$Trainmae),
          rmse_train = as.numeric(pr_train$Trainrmse),
          mqi_train = as.numeric(pr_train$Trainmqi),
          mae_null_train = as.numeric(pr_null_train["mae"]),
          rmse_null_train = as.numeric(pr_null_train["rmse"])
        )
        
        readr::write_csv2(
          dfperf_train,
          file = file.path(
            path_results,
            "performance/csv",
            paste0(target_var, "_performance_train.csv")
          )
        )
        
        pred_obs_progress <- dplyr::bind_rows(pred_obs_test_list) %>%
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
            lnzv = lnzv,
            lcor = lcor,
            dfperf_train = dfperf_train,
            dfperf_test = dfperf_test,
            pred_obs_list = pred_obs_list,
            pred_obs_test_list = pred_obs_test_list,
            run_rfe = run_rfe,
            filter_target_non_negative = filter_target_non_negative,
            fixed_predictors = fixed_predictors,
            fixed_predictors_present = fixed_predictors_present,
            kfold = kfold,
            col_kfold = col_kfold,
            validation_mode = validation_mode,
            last_completed_run = n,
            target_var = target_var,
            model_name = models[i],
            n_units = n_units,
            metric_otm = metric_otm
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
    
    pred_obs_test_complete <- dplyr::bind_rows(pred_obs_test_list) %>%
      dplyr::filter(!is.na(obs) & !is.na(pred)) %>%
      data.frame()
    
    if (nrow(pred_obs_test_complete) == 0) {
      stop(
        paste0(
          "No external predictions were available for variable: ",
          target_var
        )
      )
    }
    
    pr_test <- pst_res_mqi(
      pred_obs_test_complete %>%
        dplyr::select(
          pred,
          obs
        )
    )
    
    pred_obs_null_test <- data.frame(
      pred = pred_obs_test_complete$null_pred,
      obs = pred_obs_test_complete$obs
    )
    
    pr_null_test <- pst_res_mqi(
      pred_obs_null_test
    )
    
    dfperf_test[1, ] <- list(
      model = model_label_final,
      target_var = target_var,
      validation_mode = validation_mode,
      n_test = nrow(pred_obs_test_complete),
      ccc_test = as.numeric(pr_test["ccc"]),
      r2_test = as.numeric(pr_test["r2"]),
      nse_test = as.numeric(pr_test["nse"]),
      mae_test = as.numeric(pr_test["mae"]),
      rmse_test = as.numeric(pr_test["rmse"]),
      mqi_test = as.numeric(pr_test["mqi"]),
      mae_null_test = as.numeric(pr_null_test["mae"]),
      rmse_null_test = as.numeric(pr_null_test["rmse"])
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
      file.path(
        path_results,
        "performance/pred_obs",
        paste0(target_var, "_pred_obs_test.csv")
      )
    )
    
    lnzv_non_null <- lnzv[
      !vapply(lnzv, is.null, logical(1))
    ]
    
    if (length(lnzv_non_null) > 0) {
      
      nzv_full <- dplyr::bind_rows(lnzv_non_null)
      
      readr::write_csv2(
        nzv_full,
        file = file.path(
          path_results,
          "select/nzv",
          paste0(target_var, "_nzv_full.csv")
        )
      )
      
    } # end if lnzv_non_null
    
    lcor_non_null <- lcor[
      !vapply(lcor, is.null, logical(1))
    ]
    
    if (length(lcor_non_null) > 0) {
      
      cor_full <- dplyr::bind_rows(lcor_non_null)
      
      readr::write_csv2(
        cor_full,
        file = file.path(
          path_results,
          "select/cor",
          paste0(target_var, "_cor_full.csv")
        )
      )
      
    } # end if lcor_non_null
    
    lrfepred_non_null <- lrfepred[
      !vapply(lrfepred, is.null, logical(1))
    ]
    
    if (length(lrfepred_non_null) > 0) {
      
      n_obs <- sapply(lrfepred_non_null, length)
      seq_max <- seq_len(max(n_obs))
      
      rfe_pred_full <- as.data.frame(
        lapply(lrfepred_non_null, "[", i = seq_max)
      )
      
      names(rfe_pred_full) <- paste0("outer_unit_", seq_along(lrfepred_non_null))
      
      readr::write_csv2(
        rfe_pred_full,
        file = file.path(
          path_results,
          "select/rfe/select",
          "select_full.csv"
        )
      )
      
    } # end if lrfepred_non_null
    
    lrferes_non_null <- lrferes[
      !vapply(lrferes, is.null, logical(1))
    ]
    
    if (length(lrferes_non_null) > 0) {
      
      rfe_res_full <- purrr::imap_dfr(
        lrferes_non_null,
        function(x, run_id) {
          as.data.frame(x) %>%
            dplyr::mutate(
              outer_unit = as.integer(run_id),
              .before = 1
            )
        }
      )
      
      readr::write_csv2(
        rfe_res_full,
        file = file.path(
          path_results,
          "select/rfe/metric",
          "rfe_metrics_full.csv"
        )
      )
      
    } # end if lrferes_non_null
    
    lpredimp_non_null <- lpredimp[
      !vapply(lpredimp, is.null, logical(1))
    ]
    
    if (length(lpredimp_non_null) > 0) {
      
      pred_imp_full <- purrr::imap_dfr(
        lpredimp_non_null,
        function(x, run_id) {
          x %>%
            dplyr::mutate(
              outer_unit = as.integer(run_id),
              .before = 1
            )
        }
      ) %>%
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
    
    pred_obs_list_non_null <- pred_obs_list[
      !vapply(pred_obs_list, is.null, logical(1))
    ]
    
    if (length(pred_obs_list_non_null) > 0) {
      
      pred_obs_full <- dplyr::bind_rows(pred_obs_list_non_null)
      
      readr::write_csv2(
        pred_obs_full,
        file.path(
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
        lnzv = lnzv,
        lcor = lcor,
        dfperf_train = dfperf_train,
        dfperf_test = dfperf_test,
        pred_obs_test = pred_obs_test_complete,
        pred_obs_list = pred_obs_list,
        pred_obs_full = pred_obs_full,
        run_rfe = run_rfe,
        filter_target_non_negative = filter_target_non_negative,
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
        validation_mode = validation_mode,
        target_var = target_var,
        model_name = models[i],
        n_units = n_units,
        metric_otm = metric_otm
      ),
      file = file.path(
        path_results,
        "model",
        paste0(target_var, "_model_bundle_full.rds")
      )
    )
    
    cat(
      sprintf(
        "total time for variable %s | model: %s | mode: %s | duration: %.2f %s\n",
        target_var,
        model_label_final,
        validation_mode,
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

close_parallel_backend(cl)
