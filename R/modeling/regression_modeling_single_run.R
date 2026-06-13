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
  "tibble",
  "ggplot2",
  "foreach",
  "xgboost"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

path_raiz <- "D:/usuario_armazenamento/cassio/R/poc_maoc_global_soil_carbon"
setwd(path_raiz)

# source functions

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/pst_res_mqi.R"
)

source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/caret_rfe_functions.R"
)

# parameterization

cut_off_mc <- 0.9
nseed <- 666
fold_results <- "results_global"

row_limit <- Inf
# Use row_limit <- 50 only for quick tests.

run_rfe <- TRUE

# predictors that must always remain in the model
# use character(0) when no fixed predictors are needed

fixed_predictors <- character(0)

# target variable filter
# TRUE keeps only target values >= 0
# FALSE allows negative target values

filter_target_non_negative <- TRUE

# recursive feature elimination parameters

rfe_fold <- 10
rfe_repeat <- 3
rfe_size <- 2
# Use rfe_size <- 2:60 for the full RFE grid.

rfe_tn_length <- 4

tolerance <- FALSE
tol_per <- 2

# model parameters

model_fold <- 10
model_repeat <- 10
model_tn_length <- 10

metric_otm <- "mqi"
maxim <- TRUE

# k-fold settings

kfold <- FALSE
col_kfold <- character(0)

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
  "foreach",
  "xgboost"
)

# caret functions

custom_rcaretFuncs <- make_rcaret_funcs(
  metric_value = metric_otm,
  summary_function = pst_res_mqi
)

qrf_mean <- make_qrf_mean_model()

# support functions

safe_read_csv <- function(file_path, ...) {
  
  if (!file.exists(file_path)) {
    stop(
      paste0(
        "File not found: ",
        file_path
      )
    )
  }
  
  readr::read_csv(
    file_path,
    show_col_types = FALSE,
    ...
  )
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
  "qrf",
  "xgbTree"
)

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

dfbase <- safe_read_csv(
  "./extract_xy/xy_mixed.csv"
) %>%
  dplyr::select(
    -c(1:2)
  )

if (is.finite(row_limit) && nrow(dfbase) > row_limit) {
  dfbase <- dfbase %>%
    dplyr::slice_head(n = row_limit)
}

varsy <- names(dfbase)[18:1]

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
  
  for (j in seq_along(varsy)[3:18]) {
    
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
        "img",
        "model"
      )
    )
    
    dfperf <- tibble::tibble(
      model = NA_character_,
      target_var = NA_character_,
      n = NA_integer_,
      ccc_full = NA_real_,
      r2_full = NA_real_,
      nse_full = NA_real_,
      mae_full = NA_real_,
      rmse_full = NA_real_,
      mqi_full = NA_real_,
      mae_null_full = NA_real_,
      rmse_null_full = NA_real_
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
          "The kfold column cannot be used as a fixed predictor and will be ignored as predictor: ",
          paste(kfold_col_present, collapse = ", ")
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
      
      rfe_index <- make_repeated_group_folds(
        group = dyx_sel[[col_kfold]],
        k = rfe_fold_use,
        repeats = rfe_repeat,
        seed = nseed
      )
      
      set.seed(nseed)
      
      model_index <- make_repeated_group_folds(
        group = dyx_sel[[col_kfold]],
        k = model_fold_use,
        repeats = model_repeat,
        seed = nseed
      )
      
      dyx_sel <- dyx_sel %>%
        dplyr::select(
          -dplyr::all_of(kfold_col_present)
        )
      
    } else {
      
      rfe_index <- NULL
      model_index <- NULL
      
      rfe_fold_use <- rfe_fold
      model_fold_use <- model_fold
      
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
      
      rfe_size_valid <- make_valid_rfe_sizes(
        rfe_size = rfe_size,
        n_predictors = length(rfe_candidate_predictors)
      )
      
      set.seed(nseed)
      
      rfe_ctrl <- make_rfe_control(
        method = "repeatedcv",
        number = rfe_fold_use,
        repeats = rfe_repeat,
        index = rfe_index,
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
        summary_function = pst_res_mqi,
        save_predictions = TRUE,
        allow_parallel = FALSE
      )
      
      formu <- stats::reformulate(
        termlabels = rfe_candidate_predictors,
        response = target_var
      )
      
      set.seed(nseed)
      
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
        
        cat(
          sprintf(
            "rfe select: %d variables\n",
            pick
          )
        )
        
      } else {
        
        lrfepred <- rfe_fit$optVariables
        
        cat(
          sprintf(
            "rfe select: %d variables\n",
            length(lrfepred)
          )
        )
        
      } # end if tolerance
      
      cat("-----------------------------------------------------------\n")
      
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
      
      cat("-----------------------------------------------------------\n")
      
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
      
      cat("-----------------------------------------------------------\n")
      
    } # end if run_rfe
    
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
          "No predictors available in dfselrfe for target: ",
          target_var
        )
      )
    }
    
    set.seed(nseed)
    
    model_ctrl <- make_train_control(
      method = "repeatedcv",
      number = model_fold_use,
      repeats = model_repeat,
      index = model_index,
      summary_function = pst_res_mqi,
      save_predictions = TRUE,
      allow_parallel = allow_parallel_model
    )
    
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
    
    pred_obs_null_full <- data.frame(
      pred = rep(
        mean(dfselrfe[[target_var]], na.rm = TRUE),
        nrow(dfselrfe)
      ),
      obs = dfselrfe[[target_var]]
    )
    
    pred_obs_full <- data.frame(
      pred = predict(lmodel, dfselrfe),
      obs = dfselrfe[[target_var]]
    ) %>%
      tidyr::drop_na()
    
    pr_full <- caret::getTrainPerf(lmodel)
    
    pr_null_full <- pst_res_mqi(
      pred_obs_null_full
    )
    
    pred_imp <- caret::varImp(
      object = lmodel,
      scale = FALSE
    )
    
    lpredimp <- pred_imp$importance %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "predictor")
    
    importance_col <- names(lpredimp)[
      vapply(lpredimp, is.numeric, logical(1))
    ][1]
    
    if (length(importance_col) == 0 || is.na(importance_col)) {
      stop("No numeric importance column was returned by varImp.")
    }
    
    lpredimp <- lpredimp %>%
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
    
    lpredimp_top <- lpredimp %>%
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
    
    g2 <- ggplot2::ggplot(
      pred_obs_full,
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
    
    dfperf[1, ] <- list(
      model = lmodel$modelInfo$label,
      target_var = target_var,
      n = nrow(dfselrfe),
      ccc_full = pr_full$Trainccc,
      r2_full = pr_full$Trainr2,
      nse_full = pr_full$Trainnse,
      mae_full = pr_full$Trainmae,
      rmse_full = pr_full$Trainrmse,
      mqi_full = pr_full$Trainmqi,
      mae_null_full = pr_null_full["mae"],
      rmse_null_full = pr_null_full["rmse"]
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
        filter_target_non_negative = filter_target_non_negative,
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
        filter_target_non_negative = filter_target_non_negative,
        fixed_predictors = fixed_predictors,
        fixed_predictors_present = fixed_predictors_present,
        kfold = kfold,
        col_kfold = col_kfold,
        rfe_fit = rfe_fit,
        selected_predictors = lrfepred,
        selection_method = selection_method,
        performance = dfperf
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

close_parallel_backend(cl)
