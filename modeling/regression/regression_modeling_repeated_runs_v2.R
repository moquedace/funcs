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

path_raiz <- "D:/usuario_armazenamento/cassio/R/doutorado/fungi"
setwd(path_raiz)

# source functions

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/prediction/pst_res_mqi.R"
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

nruns <- 100   # <-- EDIT HERE: numero de rodadas (use 3 para teste rapido)
nseed <- 666
cut_off_mc <- 0.95
perc_train <- 0.8
fold_results <- "results_performance"

row_limit <- Inf   # <-- EDIT HERE: use um numero pequeno (ex.: 300) so para teste rapido

run_rfe <- TRUE

# <-- EDIT HERE (v2): controles de I/O e plots
save_workspace <- FALSE   # TRUE = salva if (save_workspace) save.image() a cada iteracao (lento em rede)
pause_plots    <- FALSE   # TRUE = Sys.sleep() apos cada grafico

# predictors that must always remain in the model
# use character(0) when no fixed predictors are needed

fixed_predictors <- character(0)

# target variable filter
# TRUE keeps only target values >= 0
# FALSE allows negative target values

filter_target_non_negative <- TRUE

# recursive feature elimination parameters

rfe_fold <- 10
rfe_repeat <- 1
rfe_size <- 2:60   # <-- EDIT HERE: grade completa (use rfe_size <- 2 para teste rapido)

rfe_tn_length <- 1

tolerance <- FALSE
tol_per <- 2

# model parameters

model_fold <- 10
model_repeat <- 10
model_tn_length <- 10

metric_otm <- "mqi"
maxim <- TRUE

# k-fold settings
# if kfold = TRUE, col_kfold is used for grouped train/test split and grouped internal resampling
# if kfold = FALSE, col_kfold is removed from predictors if present

kfold <- FALSE
col_kfold <- "id"

# parallel parameters

use_parallel <- TRUE
allow_parallel_rfe <- TRUE
allow_parallel_model <- TRUE

cores <- 15   # <-- EDIT HERE: nucleos do cluster paralelo
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

# qrf_mean removido (v2): "qrf" nativo prediz a mediana; qrf_mean nunca era referenciado

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

models <- c("qrf")

# factor variables

varfact <- c("id") %>%
  sort()

# dummy variables

dummy_vars_raw <- unlist(
  list(
    change_lulc = c(
      "change_lulc_2000_2020_built_up_gain",
      "change_lulc_2000_2020_crop_gain",
      "change_lulc_2000_2020_crop_loss",
      "change_lulc_2000_2020_high_short_veg",
      "change_lulc_2000_2020_low_short_veg",
      "change_lulc_2000_2020_mid_short_veg",
      "change_lulc_2000_2020_ocean",
      "change_lulc_2000_2020_other_change",
      "change_lulc_2000_2020_stable_built_up",
      "change_lulc_2000_2020_stable_cropland",
      "change_lulc_2000_2020_stable_natural",
      "change_lulc_2000_2020_tree_loss",
      "change_lulc_2000_2020_trees_11_to_20_m",
      "change_lulc_2000_2020_trees_20_m",
      "change_lulc_2000_2020_trees_3_to_10_m",
      "change_lulc_2000_2020_water",
      "change_lulc_2000_2020_water_change"
    ),
    wetlands = c(
      "floodplains",
      "non_floodplain_wetlands",
      "wetlands"
    ),
    geology = c(
      "geology_acid_plutonic_rocks",
      "geology_acid_volcanic_rocks",
      "geology_basic_plutonic_rocks",
      "geology_basic_volcanic_rocks",
      "geology_carbonate_sedimentary_rocks",
      "geology_evaporites",
      "geology_ice_and_glaciers",
      "geology_intermediate_plutonic_r",
      "geology_intermediate_volcani_r",
      "geology_metamorphic_rocks",
      "geology_mixed_sedimentary_rocks",
      "geology_pyroclastics",
      "geology_siliciclastic_sedimentary_rocks",
      "geology_unconsolidated_sediments",
      "geology_water_bodies"
    ),
    habitat = c(
      "global_terrestrial_habitat_artificial_terrestrial",
      "global_terrestrial_habitat_deep_ocean_floor",
      "global_terrestrial_habitat_desert",
      "global_terrestrial_habitat_forest",
      "global_terrestrial_habitat_grassland",
      "global_terrestrial_habitat_marine_intertidal",
      "global_terrestrial_habitat_marine_neritic",
      "global_terrestrial_habitat_rocky_areas",
      "global_terrestrial_habitat_savanna",
      "global_terrestrial_habitat_shrubland",
      "global_terrestrial_habitat_wetlands_inland"
    ),
    soil_class_fao = c(
      "soil_class_fao_acrisols",
      "soil_class_fao_alisols",
      "soil_class_fao_andosols",
      "soil_class_fao_anthrosols",
      "soil_class_fao_arenosols",
      "soil_class_fao_calcisols",
      "soil_class_fao_cambisols",
      "soil_class_fao_chernozems",
      "soil_class_fao_cryosols",
      "soil_class_fao_ferralsols",
      "soil_class_fao_fluvisols",
      "soil_class_fao_glaciers",
      "soil_class_fao_gleysols",
      "soil_class_fao_gypsisols",
      "soil_class_fao_histosols",
      "soil_class_fao_islands",
      "soil_class_fao_kastanozems",
      "soil_class_fao_leptosols",
      "soil_class_fao_lixisols",
      "soil_class_fao_luvisols",
      "soil_class_fao_nitisols",
      "soil_class_fao_no_data",
      "soil_class_fao_open_water",
      "soil_class_fao_phaeozems",
      "soil_class_fao_planosols",
      "soil_class_fao_plinthosols",
      "soil_class_fao_podzols",
      "soil_class_fao_regosols",
      "soil_class_fao_retisols",
      "soil_class_fao_solonchaks",
      "soil_class_fao_solonetz",
      "soil_class_fao_stagnosols",
      "soil_class_fao_technosols",
      "soil_class_fao_umbrisols",
      "soil_class_fao_vertisols"
    )
  ),
  use.names = FALSE
)

# read base dataset

dfbase <- safe_read_csv2(
  "./extract_xy/xy_mixed.csv"
) %>%
  dplyr::select(
    -forest_height
  )

if (is.finite(row_limit) && nrow(dfbase) > row_limit) {
  dfbase <- dfbase %>%
    dplyr::slice_head(n = row_limit)
}

varsy <- names(dfbase)[2:3]

# parallel cluster setup

parallel_export_objects <- c(
  "metric_otm",
  "pst_res_mqi",
  "custom_rcaretFuncs"
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
    
    dfperf <- tibble::tibble(
      model = rep(NA_character_, nruns),
      target_var = rep(NA_character_, nruns),
      n_train = rep(NA_integer_, nruns),
      ccc_train = rep(NA_real_, nruns),
      r2_train = rep(NA_real_, nruns),
      nse_train = rep(NA_real_, nruns),
      mae_train = rep(NA_real_, nruns),
      rmse_train = rep(NA_real_, nruns),
      mqi_train = rep(NA_real_, nruns),
      n_test = rep(NA_integer_, nruns),
      ccc_test = rep(NA_real_, nruns),
      r2_test = rep(NA_real_, nruns),
      nse_test = rep(NA_real_, nruns),
      mae_test = rep(NA_real_, nruns),
      rmse_test = rep(NA_real_, nruns),
      mqi_test = rep(NA_real_, nruns),
      mae_null_train = rep(NA_real_, nruns),
      rmse_null_train = rep(NA_real_, nruns),
      mae_null_test = rep(NA_real_, nruns),
      rmse_null_test = rep(NA_real_, nruns)
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
    
    if (!kfold && has_kfold_col) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::all_of(kfold_col_present)
        )
      
      kfold_col_present <- character(0)
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
          "The kfold column cannot be used as a fixed predictor and will be ignored as predictor: ",
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
        vars_fact_present,
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
    
    check_regression_target(
      data = dyx_sel,
      target_var = target_var,
      stage = "after_nzv_and_correlation"
    )
    
    fixed_predictors_present <- intersect(fixed_predictors_present, names(dyx_sel))
    
    set.seed(nseed)
    run_seeds <- sample(1:100000, nruns)
    
    lmodel <- vector("list", nruns)
    lpredimp <- vector("list", nruns)
    lrfepred <- vector("list", nruns)
    lrferes <- vector("list", nruns)
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
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$run_rfe, run_rfe)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$filter_target_non_negative, filter_target_non_negative)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$fixed_predictors, fixed_predictors)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$kfold, kfold)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$col_kfold, col_kfold)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$metric_otm, metric_otm)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$target_var, target_var)
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$model_name, models[i])
      
      progress_is_compatible <- progress_is_compatible &&
        identical(progress_obj$nruns, nruns)
      
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
          paste0(
            "Existing progress file is incompatible with current settings. ",
            "Delete it or change fold_results before continuing: ",
            model_progress_file
          )
        )
        
      } # end if progress_is_compatible
      
    } # end if progress file
    
    if (start_run > nruns) {
      
      cat(
        sprintf(
          "All runs already completed for variable %s | model: %s\n",
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
          
          group_summary <- dyx_sel %>%
            dplyr::select(
              dplyr::all_of(c(col_kfold, target_var))
            ) %>%
            dplyr::group_by(
              dplyr::across(
                dplyr::all_of(col_kfold)
              )
            ) %>%
            dplyr::summarise(
              target_mean = mean(.data[[target_var]], na.rm = TRUE),
              .groups = "drop"
            )
          
          set.seed(run_seeds[n])
          
          vf <- caret::createDataPartition(
            group_summary$target_mean,
            p = perc_train,
            list = FALSE
          )
          
          train_groups <- group_summary[vf, ] %>%
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
          
          rfe_fold_use <- min(
            rfe_fold,
            dplyr::n_distinct(train_raw[[col_kfold]])
          )
          
          model_fold_use <- min(
            model_fold,
            dplyr::n_distinct(train_raw[[col_kfold]])
          )
          
          set.seed(run_seeds[n])
          
          gkfold_rfe <- make_repeated_group_folds(
            group = train_raw[[col_kfold]],
            k = rfe_fold_use,
            repeats = rfe_repeat,
            seed = run_seeds[n]
          )

          gkfold_rfe <- validate_resampling_index(
            index = gkfold_rfe,
            n_rows = nrow(train_raw)
          )
          
          set.seed(run_seeds[n])
          
          gkfold_model <- make_repeated_group_folds(
            group = train_raw[[col_kfold]],
            k = model_fold_use,
            repeats = model_repeat,
            seed = run_seeds[n]
          )

          gkfold_model <- validate_resampling_index(
            index = gkfold_model,
            n_rows = nrow(train_raw)
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
          
          rfe_fold_use <- min(
            rfe_fold,
            nrow(train)
          )
          
          model_fold_use <- min(
            model_fold,
            nrow(train)
          )
          
        } # end if kfold
        
        check_regression_target(
          data = train,
          target_var = target_var,
          stage = paste0("train_run_", n)
        )
        
        check_regression_target(
          data = test,
          target_var = target_var,
          stage = paste0("test_run_", n)
        )
        
        all_predictors_after_filter <- setdiff(
          names(train),
          target_var
        )
        
        if (length(all_predictors_after_filter) == 0) {
          stop(
            paste0(
              "No predictors available after NZV and correlation filtering for target: ",
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
            
            lrfepred[[n]] <- head(rfe_fit$optVariables, pick)
            
            cat(
              sprintf(
                "rfe select: %d variables\n",
                pick
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
              "No predictors were selected for target: ",
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
              "No predictors available in dfselrfe for target: ",
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
        
        pred_obs_test <- data.frame(
          pred = predict(lmodel[[n]], test_sel),
          obs = test_sel[[target_var]],
          data = "test"
        ) %>%
          tidyr::drop_na()
        
        train_mean <- mean(
          dfselrfe[[target_var]],
          na.rm = TRUE
        )
        
        pred_obs_null_train <- data.frame(
          pred = rep(train_mean, nrow(dfselrfe)),
          obs = dfselrfe[[target_var]]
        )
        
        pred_obs_null_test <- data.frame(
          pred = rep(train_mean, nrow(test_sel)),
          obs = test_sel[[target_var]]
        )
        
        pr_train <- caret::getTrainPerf(lmodel[[n]])
        pr_test <- pst_res_mqi(pred_obs_test)
        
        pr_null_train <- pst_res_mqi(pred_obs_null_train)
        pr_null_test <- pst_res_mqi(pred_obs_null_test)
        
        pred_imp <- caret::varImp(
          object = lmodel[[n]],
          scale = FALSE
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
        
        if (pause_plots) Sys.sleep(2)
        
        pred_obs_fulldata <- tibble::tibble(
          obs = dfselrfe[[target_var]],
          pred = as.numeric(predict(lmodel[[n]], dfselrfe))
        ) %>%
          dplyr::mutate(data = "train") %>%
          dplyr::bind_rows(
            pred_obs_test %>%
              dplyr::select(
                obs,
                pred,
                data
              )
          ) %>%
          dplyr::mutate(
            data = factor(data, levels = c("train", "test")),
            run = n
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
        
        dfperf[n, ] <- list(
          model = lmodel[[n]]$modelInfo$label,
          target_var = target_var,
          n_train = nrow(dfselrfe),
          ccc_train = pr_train$Trainccc,
          r2_train = pr_train$Trainr2,
          nse_train = pr_train$Trainnse,
          mae_train = pr_train$Trainmae,
          rmse_train = pr_train$Trainrmse,
          mqi_train = pr_train$Trainmqi,
          n_test = nrow(test_sel),
          ccc_test = pr_test["ccc"],
          r2_test = pr_test["r2"],
          nse_test = pr_test["nse"],
          mae_test = pr_test["mae"],
          rmse_test = pr_test["rmse"],
          mqi_test = pr_test["mqi"],
          mae_null_train = pr_null_train["mae"],
          rmse_null_train = pr_null_train["rmse"],
          mae_null_test = pr_null_test["mae"],
          rmse_null_test = pr_null_test["rmse"]
        )
        
        readr::write_csv2(
          dfperf,
          file = file.path(
            path_results,
            "performance/csv",
            paste0(target_var, "_performance.csv")
          )
        )
        
        if (save_workspace) save.image(
          file.path(
            path_results,
            "img",
            paste0(target_var, ".rdata")
          )
        )
        
        pred_obs_progress <- dplyr::bind_rows(
          pred_obs_list[
            !vapply(pred_obs_list, is.null, logical(1))
          ]
        )
        
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
            dfperf = dfperf,
            pred_obs_list = pred_obs_list,
            run_rfe = run_rfe,
            filter_target_non_negative = filter_target_non_negative,
            fixed_predictors = fixed_predictors,
            fixed_predictors_present = fixed_predictors_present,
            kfold = kfold,
            col_kfold = col_kfold,
            selection_method = selection_method,
            last_completed_run = n,
            target_var = target_var,
            model_name = models[i],
            nruns = nruns,
            metric_otm = metric_otm
          ),
          file = model_progress_file
        )
        
        gc()
        
      } # end for n
      
    } # end if start_run
    
    lrfepred_non_null <- lrfepred[
      !vapply(lrfepred, is.null, logical(1))
    ]
    
    if (length(lrfepred_non_null) > 0) {
      
      n_obs <- lengths(lrfepred_non_null)
      
      if (length(n_obs) > 0 && max(n_obs) > 0) {
        seq_max <- seq_len(max(n_obs))
        rfe_pred_full <- as.data.frame(
          lapply(lrfepred_non_null, "[", i = seq_max)
        )
        names(rfe_pred_full) <- paste0("run_", seq_along(lrfepred_non_null))
      } else {
        rfe_pred_full <- tibble::tibble()
      }
      
    } else {
      
      rfe_pred_full <- tibble::tibble()
      
    } # end if lrfepred_non_null
    
    readr::write_csv2(
      rfe_pred_full,
      file = file.path(
        path_results,
        "select/rfe/select",
        "select_full.csv"
      )
    )
    
    lrferes_non_null <- lrferes[
      !vapply(lrferes, is.null, logical(1))
    ]
    
    run_ids_lrferes <- which(!vapply(lrferes, is.null, logical(1)))

    rfe_res_full <- purrr::map2_dfr(
      lrferes_non_null,
      run_ids_lrferes,
      function(x, run_id) {
        if (is.null(x)) {
          return(tibble::tibble())
        }
        
        x %>%
          dplyr::mutate(
            run = as.integer(run_id),
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
    
    lpredimp_non_null <- lpredimp[
      !vapply(lpredimp, is.null, logical(1))
    ]
    
    run_ids_lpredimp <- which(!vapply(lpredimp, is.null, logical(1)))

    pred_imp_full <- purrr::map2_dfr(
      lpredimp_non_null,
      run_ids_lpredimp,
      function(x, run_id) {
        if (is.null(x)) {
          return(tibble::tibble())
        }
        
        x %>%
          dplyr::mutate(
            run = as.integer(run_id),
            .before = 1
          )
      }
    )
    
    readr::write_csv2(
      pred_imp_full,
      file = file.path(
        path_results,
        "performance/imp_pred",
        "imp_pred_full.csv"
      )
    )
    
    pred_obs_list_non_null <- pred_obs_list[
      !vapply(pred_obs_list, is.null, logical(1))
    ]
    
    if (length(pred_obs_list_non_null) > 0) {
      pred_obs_full <- dplyr::bind_rows(pred_obs_list_non_null)
    } else {
      pred_obs_full <- tibble::tibble()
    }
    
    readr::write_csv2(
      pred_obs_full,
      file.path(
        path_results,
        "performance/pred_obs",
        "pred_obs_full.csv"
      )
    )
    
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
        dfperf = dfperf,
        pred_obs_list = pred_obs_list,
        pred_obs_full = pred_obs_full,
        run_rfe = run_rfe,
        filter_target_non_negative = filter_target_non_negative,
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
        target_var = target_var,
        model_name = models[i],
        nruns = nruns,
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
        "total time for variable %s | model: %s | duration: %.2f %s\n",
        target_var,
        models[i],
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
