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
  "gbm",
  "tibble",
  "ggplot2",
  "foreach",
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

# source functions

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/modeling/helpers/gbm_custom.R"
)

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/prediction/pst_res_class_multiclass.R"
)

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/modeling/helpers/caret_rfe_functions.R"
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

nruns <- 100
nseed <- 666
cut_off_mc <- 0.9
perc_train <- 0.75
fold_results <- "results_class_sentinel_5m"

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
# if kfold = TRUE, col_kfold is used for grouped train/test split and grouped internal resampling
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
  "gbm",
  "randomForest",
  "kknn",
  "kernlab",
  "C50",
  "DescTools",
  "foreach"
)

# caret functions

custom_rcaretFuncs <- make_ccaret_funcs(
  metric_value = metric_otm,
  summary_function = pst_res_class_multiclass
)

# support functions

check_class_support <- function(data, target_var, class_levels, context) {
  
  class_count <- table(
    factor(
      as.character(data[[target_var]]),
      levels = class_levels
    )
  )
  
  if (any(class_count == 0)) {
    stop(
      paste0(
        context,
        " does not contain all classes. Missing classes: ",
        paste(names(class_count[class_count == 0]), collapse = ", ")
      )
    )
  }
  
  invisible(class_count)
}

make_group_class_table <- function(data, group_var, target_var) {
  
  group_class <- data %>%
    dplyr::distinct(
      .data[[group_var]],
      .data[[target_var]]
    ) %>%
    dplyr::group_by(
      .data[[group_var]]
    ) %>%
    dplyr::summarise(
      n_classes = dplyr::n_distinct(.data[[target_var]]),
      group_class = dplyr::first(.data[[target_var]]),
      .groups = "drop"
    )
  
  if (any(group_class$n_classes > 1)) {
    
    bad_groups <- group_class %>%
      dplyr::filter(n_classes > 1) %>%
      dplyr::pull(.data[[group_var]])
    
    stop(
      paste0(
        "Some groups contain more than one class, which prevents grouped stratified splitting: ",
        paste(bad_groups, collapse = ", ")
      )
    )
  }
  
  group_class
}

make_repeated_stratified_group_kfold <- function(data, group_var, target_var, k, repeats, seed) {
  
  group_class <- make_group_class_table(
    data = data,
    group_var = group_var,
    target_var = target_var
  )
  
  class_levels <- levels(data[[target_var]])
  
  if (is.null(class_levels)) {
    class_levels <- sort(unique(as.character(data[[target_var]])))
  }
  
  group_class <- group_class %>%
    dplyr::mutate(
      group_class = factor(
        as.character(group_class),
        levels = class_levels
      )
    )
  
  n_groups_total <- nrow(group_class)
  
  if (n_groups_total < 2) {
    stop("At least two groups are required to create grouped folds.")
  }
  
  k_use <- min(k, n_groups_total)
  
  if (k_use < k) {
    warning(
      paste0(
        "The requested number of folds was larger than the number of groups. ",
        "Using k = ",
        k_use,
        " instead."
      )
    )
  }
  
  group_class_count <- table(group_class$group_class)
  
  if (any(group_class_count < 2)) {
    stop(
      paste0(
        "Grouped resampling requires at least two groups per class. Problematic classes: ",
        paste(names(group_class_count[group_class_count < 2]), collapse = ", ")
      )
    )
  }
  
  index_list <- list()
  
  for (rep_id in seq_len(repeats)) {
    
    set.seed(seed + rep_id - 1)
    
    fold_validation_groups <- vector("list", k_use)
    
    for (fold_id in seq_len(k_use)) {
      fold_validation_groups[[fold_id]] <- character(0)
    } # end for fold_id
    
    for (current_class in class_levels) {
      
      class_groups <- group_class %>%
        dplyr::filter(group_class == current_class) %>%
        dplyr::pull(.data[[group_var]]) %>%
        as.character()
      
      class_groups <- sample(class_groups, length(class_groups))
      
      fold_id_class <- rep(seq_len(k_use), length.out = length(class_groups))
      
      for (fold_id in seq_len(k_use)) {
        fold_validation_groups[[fold_id]] <- unique(
          c(
            fold_validation_groups[[fold_id]],
            class_groups[fold_id_class == fold_id]
          )
        )
      } # end for fold_id
      
    } # end for current_class
    
    for (fold_id in seq_len(k_use)) {
      
      validation_groups <- fold_validation_groups[[fold_id]]
      
      train_index <- which(
        !as.character(data[[group_var]]) %in% validation_groups
      )
      
      train_classes <- table(
        factor(
          as.character(data[[target_var]][train_index]),
          levels = class_levels
        )
      )
      
      if (any(train_classes == 0)) {
        stop(
          paste0(
            "A grouped internal fold removed all observations from at least one class. ",
            "Problematic fold: ",
            fold_id,
            " | repetition: ",
            rep_id
          )
        )
      }
      
      index_name <- sprintf("Fold%02d.Rep%02d", fold_id, rep_id)
      index_list[[index_name]] <- train_index
      
    } # end for fold_id
    
  } # end for rep_id
  
  index_list
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

# factor variables

varfact <- c("class") %>%
  sort()

# dummy variables

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
      model = rep(NA_character_, nruns),
      target_var = rep(NA_character_, nruns),
      n_train = rep(NA_integer_, nruns),
      kappa_train = rep(NA_real_, nruns),
      accuracy_train = rep(NA_real_, nruns),
      sensitivity_train = rep(NA_real_, nruns),
      specificity_train = rep(NA_real_, nruns),
      precision_train = rep(NA_real_, nruns),
      recall_train = rep(NA_real_, nruns),
      balanced_accuracy_train = rep(NA_real_, nruns),
      f1_train = rep(NA_real_, nruns),
      n_test = rep(NA_integer_, nruns),
      kappa_test = rep(NA_real_, nruns),
      accuracy_test = rep(NA_real_, nruns),
      sensitivity_test = rep(NA_real_, nruns),
      specificity_test = rep(NA_real_, nruns),
      precision_test = rep(NA_real_, nruns),
      recall_test = rep(NA_real_, nruns),
      balanced_accuracy_test = rep(NA_real_, nruns),
      f1_test = rep(NA_real_, nruns)
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
    
    if (!kfold && has_kfold_col) {
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
          "Classification modeling requires at least 3 observations per class. ",
          "The following classes have fewer than 3 observations: ",
          paste(names(class_count[class_count < 3]), collapse = ", ")
        )
      )
    }
    
    if (kfold) {
      
      group_class_global <- make_group_class_table(
        data = dyx_sel,
        group_var = col_kfold,
        target_var = target_var
      )
      
      group_class_count <- table(
        factor(
          as.character(group_class_global$group_class),
          levels = class_levels
        )
      )
      
      if (any(group_class_count < 2)) {
        stop(
          paste0(
            "Grouped classification requires at least 2 groups per class. ",
            "The following classes have fewer than 2 groups: ",
            paste(names(group_class_count[group_class_count < 2]), collapse = ", ")
          )
        )
      }
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
    
    set.seed(nseed)
    run_seeds <- sample(1:100000, nruns)
    
    lmodel <- vector("list", nruns)
    lpredimp <- vector("list", nruns)
    lrfepred <- vector("list", nruns)
    lrferes <- vector("list", nruns)
    lconf_matrix_train <- vector("list", nruns)
    lconf_matrix_test <- vector("list", nruns)
    pred_obs_list <- vector("list", nruns)
    
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
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$fixed_predictors, fixed_predictors)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$kfold, kfold)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$col_kfold, col_kfold)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$target_var, target_var)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$model_name, models[i])
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$nruns, nruns)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$class_probs, class_probs)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$metric_otm, metric_otm)
      
      if (!is.null(progress_obj$dfperf)) {
        progress_is_compatible <- progress_is_compatible &&
          nrow(progress_obj$dfperf) == nruns
      } else {
        progress_is_compatible <- FALSE
      }
      
      if (!is.null(progress_obj$pred_obs_list)) {
        progress_is_compatible <- progress_is_compatible &&
          length(progress_obj$pred_obs_list) == nruns
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
        
        if (!is.null(progress_obj$lconf_matrix_train)) {
          lconf_matrix_train <- progress_obj$lconf_matrix_train
        }
        
        if (!is.null(progress_obj$lconf_matrix_test)) {
          lconf_matrix_test <- progress_obj$lconf_matrix_test
        }
        
        if (!is.null(progress_obj$dfperf)) {
          dfperf <- progress_obj$dfperf
        }
        
        if (!is.null(progress_obj$pred_obs_list)) {
          pred_obs_list <- progress_obj$pred_obs_list
        }
        
        completed_runs <- which(!is.na(dfperf$model))
        
        if (length(completed_runs) > 0) {
          start_run <- max(completed_runs) + 1
        }
        
        cat(
          sprintf(
            "resuming from run %d/%d | model: %s | variable: %s\n",
            start_run,
            nruns,
            models[i],
            target_var
          )
        )
        
      } else {
        
        stop(
          sprintf(
            "Existing progress file is incompatible with current settings (model: %s | variable: %s). Delete it or change fold_results before continuing: %s",
            models[i],
            target_var,
            model_progress_file
          )
        )
        
      } # end if progress_is_compatible
      
    } # end if progress file
    
    if (start_run > nruns) {
      
      cat(
        sprintf(
          "all runs already completed for variable %s | model: %s\n",
          target_var,
          models[i]
        )
      )
      
    } else {
      
      for (n in start_run:nruns) {
        
        trun <- Sys.time()
        
        cat(
          sprintf(
            "run %d/%d | model: %s | variable: %s\n",
            n,
            nruns,
            models[i],
            target_var
          )
        )
        
        if (kfold) {
          
          group_class_run <- make_group_class_table(
            data = dyx_sel,
            group_var = col_kfold,
            target_var = target_var
          )
          
          set.seed(run_seeds[n])
          
          vf <- caret::createDataPartition(
            group_class_run$group_class,
            p = perc_train,
            list = FALSE
          )
          
          train_groups <- group_class_run[vf, ] %>%
            dplyr::pull(
              dplyr::all_of(col_kfold)
            )
          
          train_raw <- dyx_sel %>%
            dplyr::filter(
              .data[[col_kfold]] %in% train_groups
            )
          
          test_raw <- dyx_sel %>%
            dplyr::filter(
              !.data[[col_kfold]] %in% train_groups
            )
          
          check_class_support(
            data = train_raw,
            target_var = target_var,
            class_levels = class_levels,
            context = paste0("Training set in run ", n)
          )
          
          check_class_support(
            data = test_raw,
            target_var = target_var,
            class_levels = class_levels,
            context = paste0("Test set in run ", n)
          )
          
          rfe_fold_use <- min(
            rfe_fold,
            dplyr::n_distinct(train_raw[[col_kfold]])
          )
          
          model_fold_use <- min(
            model_fold,
            dplyr::n_distinct(train_raw[[col_kfold]])
          )
          
          set.seed(run_seeds[n])
          
          gkfold_rfe <- make_repeated_stratified_group_kfold(
            data = train_raw,
            group_var = col_kfold,
            target_var = target_var,
            k = rfe_fold_use,
            repeats = rfe_repeat,
            seed = run_seeds[n]
          )

          gkfold_rfe <- validate_resampling_index(
            index = gkfold_rfe,
            n_rows = nrow(train_raw),
            target = train_raw[[target_var]],
            class_levels = class_levels,
            require_all_classes = TRUE
          )
          
          set.seed(run_seeds[n])
          
          gkfold_model <- make_repeated_stratified_group_kfold(
            data = train_raw,
            group_var = col_kfold,
            target_var = target_var,
            k = model_fold_use,
            repeats = model_repeat,
            seed = run_seeds[n]
          )

          gkfold_model <- validate_resampling_index(
            index = gkfold_model,
            n_rows = nrow(train_raw),
            target = train_raw[[target_var]],
            class_levels = class_levels,
            require_all_classes = TRUE
          )
          
          train <- train_raw %>%
            dplyr::select(
              -dplyr::all_of(col_kfold)
            )
          
          test <- test_raw %>%
            dplyr::select(
              -dplyr::all_of(col_kfold)
            )
          
        } else {
          
          set.seed(run_seeds[n])
          
          vf <- caret::createDataPartition(
            dyx_sel[[target_var]],
            p = perc_train,
            list = FALSE
          )
          
          train <- dyx_sel[vf, , drop = FALSE]
          test <- dyx_sel[-vf, , drop = FALSE]
          
          gkfold_rfe <- NULL
          gkfold_model <- NULL
          
          rfe_fold_use <- min(rfe_fold, nrow(train))
          model_fold_use <- min(model_fold, nrow(train))
          
        } # end if kfold
        
        train[[target_var]] <- factor(
          as.character(train[[target_var]]),
          levels = class_levels
        )
        
        test[[target_var]] <- factor(
          as.character(test[[target_var]]),
          levels = class_levels
        )
        
        check_class_support(
          data = train,
          target_var = target_var,
          class_levels = class_levels,
          context = paste0("Training set in run ", n)
        )
        
        check_class_support(
          data = test,
          target_var = target_var,
          class_levels = class_levels,
          context = paste0("Test set in run ", n)
        )
        
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
          
          rfe_size_valid <- make_valid_rfe_sizes(
            rfe_size = rfe_size,
            n_predictors = length(rfe_candidate_predictors)
          )
          
          set.seed(run_seeds[n])
          
          rfe_ctrl <- make_rfe_control(
            method = "repeatedcv",
            number = rfe_fold_use,
            repeats = rfe_repeat,
            index = gkfold_rfe,
            functions_object = custom_rcaretFuncs,
            verbose = FALSE,
            allow_parallel = allow_parallel_rfe
          )
          
          set.seed(run_seeds[n])
          
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
          
          set.seed(run_seeds[n])
          
          rfe_fit <- caret::rfe(
            form = formu,
            data = rfe_data,
            sizes = rfe_size_valid,
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
            
            lrfepred[[n]] <- head(rfe_fit$optVariables, pick)
            
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
        
        set.seed(run_seeds[n])
        
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
        
        pred_obs_train <- data.frame(
          pred = as.character(predict(lmodel[[n]], dfselrfe)),
          obs = as.character(dfselrfe[[target_var]]),
          data = "train",
          stringsAsFactors = FALSE
        ) %>%
          tidyr::drop_na()
        
        pred_obs_test <- data.frame(
          pred = as.character(predict(lmodel[[n]], test_sel)),
          obs = as.character(test_sel[[target_var]]),
          data = "test",
          stringsAsFactors = FALSE
        ) %>%
          tidyr::drop_na()
        
        pred_obs_train$pred <- factor(
          pred_obs_train$pred,
          levels = class_levels
        )
        
        pred_obs_train$obs <- factor(
          pred_obs_train$obs,
          levels = class_levels
        )
        
        pred_obs_test$pred <- factor(
          pred_obs_test$pred,
          levels = class_levels
        )
        
        pred_obs_test$obs <- factor(
          pred_obs_test$obs,
          levels = class_levels
        )
        
        pr_train <- caret::getTrainPerf(lmodel[[n]])
        
        pr_test <- pst_res_class_multiclass(
          pred_obs_test
        )
        
        lconf_matrix_train[[n]] <- caret::confusionMatrix(
          data = pred_obs_train$pred,
          reference = pred_obs_train$obs,
          mode = "everything"
        )
        
        lconf_matrix_test[[n]] <- caret::confusionMatrix(
          data = pred_obs_test$pred,
          reference = pred_obs_test$obs,
          mode = "everything"
        )
        
        pred_imp <- caret::varImp(
          object = lmodel[[n]],
          scale = TRUE
        )
        
        lpredimp[[n]] <- pred_imp$importance %>%
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
        
        gg <- ggplot2::ggplot(
          lpredimp_top,
          ggplot2::aes(
            y = reorder(predictor, importance),
            x = importance
          )
        ) +
          ggplot2::geom_col() +
          ggplot2::facet_wrap(~class, scales = "free_x")
        
        print(gg)
        
        pred_obs_fulldata <- dplyr::bind_rows(
          pred_obs_train,
          pred_obs_test
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
        
        dfperf[n, ] <- list(
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
          f1_train = as.numeric(pr_train$Trainf1_score),
          n_test = nrow(test_sel),
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
          dfperf,
          file = file.path(
            path_results,
            "performance/csv",
            paste0(target_var, "_performance.csv")
          )
        )
        
        pred_obs_progress <- pred_obs_list[
          !vapply(pred_obs_list, is.null, logical(1))
        ]
        
        if (length(pred_obs_progress) > 0) {
          
          pred_obs_progress_df <- dplyr::bind_rows(pred_obs_progress)
          
          readr::write_csv2(
            pred_obs_progress_df,
            pred_obs_progress_file
          )
        }
        
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
            lconf_matrix_test = lconf_matrix_test,
            dfperf = dfperf,
            pred_obs_list = pred_obs_list,
            run_rfe = run_rfe,
            fixed_predictors = fixed_predictors,
            fixed_predictors_present = fixed_predictors_present,
            kfold = kfold,
            col_kfold = col_kfold,
            last_completed_run = n,
            target_var = target_var,
            model_name = models[i],
            nruns = nruns,
            metric_otm = metric_otm,
            class_probs = class_probs,
            class_levels = class_levels
          ),
          file = model_progress_file
        )
        
        if (save_workspace) save.image(
          file.path(
            path_results,
            "img",
            paste0(target_var, ".rdata")
          )
        )
        
        gc()
        
      } # end for n
      
    } # end if start_run
    
    lrfepred_non_null <- lrfepred[
      !vapply(lrfepred, is.null, logical(1))
    ]
    
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
    
    lrferes_non_null <- lrferes[
      !vapply(lrferes, is.null, logical(1))
    ]
    
    if (length(lrferes_non_null) > 0) {
      
      run_ids_lrferes <- which(
        !vapply(lrferes, is.null, logical(1))
      )
      
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
    
    lpredimp_non_null <- lpredimp[
      !vapply(lpredimp, is.null, logical(1))
    ]
    
    if (length(lpredimp_non_null) > 0) {
      
      run_ids_lpredimp <- which(
        !vapply(lpredimp, is.null, logical(1))
      )
      
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
        lconf_matrix_train = lconf_matrix_train,
        lconf_matrix_test = lconf_matrix_test,
        dfperf = dfperf,
        pred_obs_list = pred_obs_list,
        pred_obs_full = pred_obs_full,
        run_rfe = run_rfe,
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
        target_var = target_var,
        model_name = models[i],
        nruns = nruns,
        metric_otm = metric_otm,
        class_probs = class_probs,
        class_levels = class_levels
      ),
      file = file.path(
        path_results,
        "model",
        paste0(target_var, "_model_bundle_full.rds")
      )
    )
    
    model_label_final <- if (length(lmodel) > 0 && any(!vapply(lmodel, is.null, logical(1)))) {
      lmodel[[which(!vapply(lmodel, is.null, logical(1)))[1]]]$modelInfo$label
    } else {
      models[i]
    }
    
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

close_parallel_backend(cl)
