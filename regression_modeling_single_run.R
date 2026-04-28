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
  "ggplot2"
)

install_load_pkg(pkg)

rm(list = ls())
gc()

path_raiz <- "D:/usuario_armazenamento/cassio/R/poc_maoc_global_soil_carbon"
setwd(path_raiz)



# parameterization
cut_off_mc <- 0.9
nseed <- 666
fold_results <- "results_global"

run_rfe <- TRUE

# recursive feature elimination (RFE) parameters
rfe_fold <- 10
rfe_repeat <- 3
rfe_size <- 2#2:60
rfe_tn_length <- 4
tolerance <- FALSE
tol_per <- 2

# model parameters
model_fold <- 10
model_repeat <- 10
model_tn_length <- 10
metric_otm <- "mqi"
maxim <- T


# funcs
source(
  "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/pst_res_mqi.R"
)


custom_rcaretFuncs <- caretFuncs
custom_rcaretFuncs$summary <- pst_res_mqi
custom_rcaretFuncs$fit <- function (x, y, first, last, ...) {
  train(x, y, metric = "mqi", ...)
}


create_dirs <- function(base_path, sub_dirs) {
  purrr::walk(sub_dirs, ~{
    dir_path <- file.path(base_path, .x)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  })
}


safe_read <- function(file_path, ...) {
  if (!file.exists(file_path)) {
    stop(paste("error: file not found", file_path))
  }
  readr::read_csv(file_path, show_col_types = FALSE, ...)
}


qrf_mean <- getModelInfo("qrf")$qrf
qrf_mean$predict <- function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, what = mean)
  if (is.matrix(out)) out <- out[, 1]
  out
}


# list of models
models <- c("qrf", "xgbTree")

# factor variables
varfact <- c("id") %>% sort()

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
  ), use.names = FALSE
)

# k-fold settings
kfold <- F
col_kfold <- character(0)

# read base dataset
dfbase <- safe_read("./extract_xy/xy_mixed.csv") %>% 
  select(
    -c(1:2)
  )
varsy <- names(dfbase)[18:1]

# parallel cluster setup
cores <- 15#max(1, detectCores() - 1)
cl <- parallel::makeCluster(cores)
cl <- parallelly::autoStopCluster(cl)



i = 1
j = 3
n = 1

for (i in seq_along(models)) {
  
  tmodel <- Sys.time()
  
  
  for (j in seq_along(varsy)[3:18]) {
    
    tvar <- Sys.time()
    target_var <- varsy[j]
    cat(
      sprintf(
        "model: %s | variable: %s\n",
        models[i], target_var
      )
    )
    
    
    path_results <- file.path(path_raiz, fold_results)
    
    create_dirs(path_results, c(models[i], file.path(models[i], target_var)))
    
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
      dplyr::select(dplyr::all_of(target_var))
    
    dx <- dfbase %>%
      dplyr::select(-dplyr::all_of(varsy))
    
    vars_fact_present <- intersect(varfact, names(dx))
    dummy_vars_present <- intersect(dummy_vars_raw, names(dx))
    
    dyx_sel <- dplyr::bind_cols(dy, dx) %>%
      dplyr::filter(.data[[target_var]] >= 0)
    
    vars_numeric_to_clean <- names(dyx_sel)[
      vapply(dyx_sel, is.numeric, logical(1))
    ] %>%
      setdiff(vars_fact_present)
    
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
      na.omit()
    
    nzv_candidates <- setdiff(
      names(dyx_sel),
      c(target_var, dummy_vars_present)
    )
    
    if (length(nzv_candidates) > 0) {
      nzv_vars <- caret::nearZeroVar(
        dyx_sel[, nzv_candidates, drop = FALSE],
        names = TRUE
      )
    } else {
      nzv_vars <- character(0)
    }
    
    write.csv2(
      data.frame(
        nzv_removed = nzv_vars,
        stringsAsFactors = FALSE
      ),
      file = file.path(
        path_results,
        "select/nzv",
        paste0(target_var, "_nzv.csv")
      ),
      row.names = FALSE
    )
    
    if (length(nzv_vars) > 0) {
      dyx_sel <- dyx_sel %>%
        dplyr::select(-dplyr::any_of(nzv_vars))
    }
    
    cor_candidates <- setdiff(
      names(dyx_sel),
      c(target_var, dummy_vars_present, vars_fact_present)
    )
    
    cor_candidates <- cor_candidates[
      vapply(dyx_sel[, cor_candidates, drop = FALSE], is.numeric, logical(1))
    ]
    
    if (length(cor_candidates) >= 2) {
      mcor <- dyx_sel %>%
        dplyr::select(dplyr::all_of(cor_candidates)) %>%
        cor(
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
    
    write.csv2(
      data.frame(
        cor_removed = fc,
        stringsAsFactors = FALSE
      ),
      file = file.path(
        path_results,
        "select/cor",
        paste0(target_var, "_cor.csv")
      ),
      row.names = FALSE
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
      gkfold <- caret::groupKFold(
        dyx_sel[[col_kfold]],
        k = rfe_fold
      )
      
      dyx_sel <- dyx_sel %>%
        dplyr::select(-dplyr::all_of(col_kfold))
      
    } else {
      if (has_kfold_col) {
        dyx_sel <- dyx_sel %>%
          dplyr::select(-dplyr::all_of(col_kfold))
      }
    }
    
    
    
    registerDoParallel(cl)
    
    formu <- as.formula(paste(target_var, "~ ."))
    
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
      
      set.seed(nseed)
      rfe_ctrl <- rfeControl(
        method = "repeatedcv", 
        repeats = rfe_repeat,
        number = rfe_fold,
        verbose = FALSE,
        index = if (kfold) gkfold else NULL,
        functions = custom_rcaretFuncs
      )
      
      set.seed(nseed)
      rfe_model_ctrl <- trainControl(
        method = "repeatedcv", 
        number = rfe_fold,
        repeats = rfe_repeat,
        savePredictions = TRUE,
        summaryFunction = pst_res_mqi
      )
      
      set.seed(nseed)
      rfe_fit <- rfe(
        form = formu,
        data = dyx_sel,
        sizes = rfe_size,
        method = if (models[i] == "qrf_mean") get(models[i]) else models[i],
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
          models[i], target_var, Sys.time() - tvar, units(Sys.time() - tvar)
        )
      )
      
      lrferes <- rfe_fit$result
      
      if (tolerance) {
        pick <- caret::pickSizeTolerance(
          x = lrferes,
          metric = metric_otm,
          tol = tol_per,
          maximize = maxim
        )
        
        lrfepred <- rfe_fit$optVariables[1:pick]
        
        cat(sprintf("rfe select: %d variables\n", pick))
        
      } else {
        
        lrfepred <- rfe_fit$optVariables
        
        cat(sprintf("rfe select: %d variables\n", length(lrfepred)))
      }
      
      cat("-----------------------------------------------------------\n")
      
      if (length(varfact) > 0) {
        
        original_predictor_names <- names(dyx_sel)
        
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
      
      cat(sprintf("predictors selected without RFE: %d variables\n", length(lrfepred)))
      cat("-----------------------------------------------------------\n")
    }
    
    write.csv2(
      data.frame(lrferes),
      file = file.path(
        path_results,
        "select/rfe/metric",
        paste0("rfe_metrics.csv")
      ),
      row.names = FALSE
    )
    
    write.csv2(
      data.frame(
        selection_method = selection_method,
        pred_sel = lrfepred
      ),
      file = file.path(
        path_results,
        "select/rfe/select",
        paste0("rfe_pred_sel.csv")
      ),
      row.names = FALSE
    )
    
    dfselrfe <- dyx_sel %>%
      dplyr::select(
        dplyr::all_of(c(target_var, lrfepred))
      )
    
    
    
    set.seed(nseed)
    model_ctrl <- trainControl(
      method = "repeatedcv", 
      number = model_fold,
      repeats = model_repeat,
      savePredictions = T,
      index = if (kfold) gkfold else NULL,
      summaryFunction = pst_res_mqi
    )
    
    formu <- as.formula(paste(target_var, "~ ."))
    
    set.seed(nseed)
    registerDoParallel(cl)
    model_fit <- train(
      form = formu,
      data = dfselrfe,
      metric = metric_otm,
      importance = T,
      maximize = maxim,
      method = if (models[i] == "qrf_mean") get(models[i]) else models[i], 
      trControl = model_ctrl,
      tuneLength = model_tn_length
    )
    
    print(model_fit)
    cat("\n-----------------------------------------------------------\n")
    
    cat(
      sprintf(
        "%s | variable: %s | duration: %.2f %s\n",
        model_fit[["modelInfo"]][["label"]], target_var,
        Sys.time() - tvar,
        units(Sys.time() - tvar)
      )
    )
    
    cat("-----------------------------------------------------------\n")
    
    lmodel <- model_fit
    
    pred_obs_null_full <- data.frame(
      pred = rep(mean(dfselrfe[[target_var]]), nrow(dfselrfe)),
      obs = dfselrfe[[target_var]])
    
    pred_obs_full <- data.frame(pred = predict(lmodel, dfselrfe), 
                                obs = dfselrfe[[target_var]])  %>% 
      na.omit()
    
    pr_full <- getTrainPerf(lmodel)
    
    pr_null_full = pst_res_mqi(pred_obs_null_full)
    
    
    
    pred_imp <- caret::varImp(
      object = lmodel$finalModel, scale = FALSE
    )
    
    lpredimp <- pred_imp %>%
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
      dplyr::select(predictor, importance) %>%
      dplyr::filter(!is.na(importance)) %>%
      dplyr::arrange(dplyr::desc(importance))
    
    lpredimp_top <- lpredimp %>%
      dplyr::slice_head(n = 15) %>%
      dplyr::arrange(importance)
    
    g1 <- ggplot(
      lpredimp_top,
      aes(
        y = factor(predictor, levels = predictor),
        x = importance
      )
    ) +
      geom_col() +
      labs(
        title = paste0(target_var, " | top predictors"),
        x = "Importance",
        y = NULL
      )
    
    plot(g1)
    
    Sys.sleep(2)
    
    g2 <- ggplot(pred_obs_full, aes(x = obs, y = pred)) +
      geom_abline(col = "red", lwd = 2) +
      geom_point(alpha = 0.25) +
      labs(
        title = target_var,
        x = "Observed",
        y = "Predicted"
      )
    
    plot(g2)
    
    write.csv2(
      lpredimp,
      file.path(
        path_results,
        "performance/imp_pred",
        "imp_pred.csv"
      ),
      row.names = FALSE
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
    
    
    write.csv2(
      dfperf, row.names = F,
      file.path(
        path_results, "performance/csv",
        paste0(target_var, "_performance.csv")))
    
    
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
    
    
    
  }
  
  
  cat(
    sprintf(
      "total time for model %s | duration: %.2f %s\n",
      models[i],
      Sys.time() - tmodel,
      units(Sys.time() - tmodel)
    )
  )
  
  
  
  
}








