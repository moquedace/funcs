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

path_raiz <- "D:/usuario_armazenamento/cassio/R/doutorado/fungi"
setwd(path_raiz)


# parameterization
nruns <- 3
cut_off_mc <- 0.95
perc_train <- 0.8
fold_results <- "results_performance"

run_rfe <- TRUE

# recursive feature elimination (rfe) parameters
rfe_fold <- 10
rfe_repeat <- 1
rfe_size <- 2#2:60
rfe_tn_length <- 1
tolerance <- FALSE
tol_per <- 2

# model parameters
model_fold <- 10
model_repeat <- 10
model_tn_length <- 10
metric_otm <- "mqi"
maxim <- TRUE


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
  readr::read_csv2(file_path, show_col_types = FALSE, ...)
}


qrf_mean <- getModelInfo("qrf")$qrf
qrf_mean$predict <- function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, what = mean)
  if (is.matrix(out)) out <- out[, 1]
  out
}


# list of models
models <- c("qrf")

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
kfold <- FALSE
col_kfold <- "id"

# read base dataset
dfbase <- safe_read("./extract_xy/xy_mixed.csv") %>% 
  select(
    -forest_height
  ) %>% 
  .[1:300, ]

varsy <- names(dfbase)[2:3]

# parallel cluster setup
cores <- 15#max(1, detectCores() - 1)
cl <- parallel::makeCluster(cores)
cl <- parallelly::autoStopCluster(cl)



i = 1
j = 1
n = 1

for (i in seq_along(models)) {
  
  tmodel <- Sys.time()
  
  
  for (j in seq_along(varsy)) {
    
    tvar <- Sys.time()
    target_var <- varsy[j]
    
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
    
    
    
    set.seed(666)
    nseed <- sample(1:100000, nruns)
    
    
    lmodel <- list()
    lpredimp <- list()
    lrfepred <- list()
    lrferes <- list()
    
    
    pred_obs_list <- list()
    
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
      
      if (!is.null(progress_obj$run_rfe)) {
        if (!identical(progress_obj$run_rfe, run_rfe)) {
          stop(
            paste0(
              "Existing progress file was created with run_rfe = ",
              progress_obj$run_rfe,
              ", but current script has run_rfe = ",
              run_rfe,
              ". Delete the progress file or change fold_results before continuing: ",
              model_progress_file
            )
          )
        }
      }
      
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
    }
    
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
            n, nruns, models[i], target_var
          )
        )
        
        
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
          
          dcp <- dyx_sel %>%
            dplyr::select(dplyr::all_of(c(varfact, target_var))) %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(col_kfold))) %>%
            dplyr::summarise(
              dplyr::across(dplyr::all_of(target_var), mean),
              .groups = "drop"
            )
          
          set.seed(nseed[n])
          vf <- caret::createDataPartition(
            dcp[[target_var]],
            p = perc_train,
            list = FALSE
          )
          
          selrow <- dcp[vf, ] %>%
            dplyr::select(-dplyr::all_of(target_var))
          
          train <- dyx_sel %>%
            dplyr::filter(.data[[col_kfold]] %in% selrow[[col_kfold]])
          
          test <- dyx_sel %>%
            dplyr::anti_join(selrow, by = col_kfold)
          
          set.seed(nseed[n])
          gkfold <- caret::groupKFold(
            train[[col_kfold]],
            k = rfe_fold
          )
          
          train <- train %>%
            dplyr::select(-dplyr::all_of(col_kfold))
          
          test <- test %>%
            dplyr::select(-dplyr::all_of(col_kfold))
          
        } else {
          set.seed(nseed[n])
          vf <- caret::createDataPartition(
            dyx_sel[[target_var]],
            p = perc_train,
            list = FALSE
          )
          
          train <- dyx_sel[vf, ]
          test <- dyx_sel[-vf, ]
          
          if (has_kfold_col) {
            train <- train %>%
              dplyr::select(-dplyr::all_of(col_kfold))
            
            test <- test %>%
              dplyr::select(-dplyr::all_of(col_kfold))
          }
        }
        
        
        
        registerDoParallel(cl)
        
        formu <- as.formula(paste(target_var, "~ ."))
        
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
        
        if (run_rfe) {
          
          cat("RFE enabled: running recursive feature elimination\n")
          
          set.seed(nseed[n])
          rfe_ctrl <- rfeControl(
            method = "repeatedcv", 
            repeats = rfe_repeat,
            number = rfe_fold,
            verbose = FALSE,
            index = if (kfold) gkfold else NULL,
            functions = custom_rcaretFuncs
          )
          
          set.seed(nseed[n])
          rfe_model_ctrl <- trainControl(
            method = "repeatedcv", 
            number = rfe_fold,
            repeats = rfe_repeat,
            savePredictions = TRUE,
            summaryFunction = pst_res_mqi
          )
          
          set.seed(nseed[n])
          rfe_fit <- rfe(
            form = formu,
            data = train,
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
              "time for rfe - model: %s | variable: %s | run: %d | duration: %.2f %s\n",
              models[i],
              target_var,
              n,
              Sys.time() - trun,
              units(Sys.time() - trun)
            )
          )
          
          lrferes[[n]] <- rfe_fit$result
          
          if (tolerance) {
            
            pick <- caret::pickSizeTolerance(
              x = lrferes[[n]],
              metric = metric_otm,
              tol = tol_per,
              maximize = maxim
            )
            
            lrfepred[[n]] <- rfe_fit$optVariables[1:pick]
            
            cat(sprintf("rfe select: %d variables\n", pick))
            
          } else {
            
            lrfepred[[n]] <- rfe_fit$optVariables
            
            cat(sprintf("rfe select: %d variables\n", length(lrfepred[[n]])))
          }
          
          if (length(varfact) > 0) {
            
            original_predictor_names <- names(train)
            
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
          }
          
          selection_method <- "rfe"
          
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
          
          cat(sprintf("predictors selected without RFE: %d variables\n", length(lrfepred[[n]])))
        }
        
        cat("-----------------------------------------------------------\n")
        
        write.csv2(
          data.frame(lrferes[[n]]),
          file = file.path(
            path_results,
            "select/rfe/metric",
            paste0("rfe_metrics_", n, ".csv")
          ),
          row.names = FALSE
        )
        
        write.csv2(
          data.frame(
            selection_method = selection_method,
            pred_sel = lrfepred[[n]]
          ),
          file = file.path(
            path_results,
            "select/rfe/select", 
            paste0("rfe_pred_sel_", n, ".csv")
          ),
          row.names = FALSE
        )
        
        dfselrfe <- train %>%
          dplyr::select(
            dplyr::all_of(c(target_var, lrfepred[[n]]))
          )
        
        
        set.seed(nseed[n])
        model_ctrl <- trainControl(
          method = "repeatedcv", 
          number = model_fold,
          repeats = model_repeat,
          savePredictions = T,
          index = if (kfold) gkfold else NULL,
          summaryFunction = pst_res_mqi
        )
        
        formu <- as.formula(paste(target_var, "~ ."))
        
        set.seed(nseed[n])
        registerDoParallel(cl)
        model_fit <- train(
          form = formu,
          data = dfselrfe,
          metric = metric_otm,
          maximize = maxim,
          method = if (models[i] == "qrf_mean") get(models[i]) else models[i], 
          trControl = model_ctrl,
          tuneLength = model_tn_length
        )
        
        print(model_fit)
        cat("\n-----------------------------------------------------------\n")
        
        cat(
          sprintf(
            "%s | variable: %s | run: %d | duration: %.2f %s\n",
            model_fit[["modelInfo"]][["label"]], target_var, n,
            Sys.time() - trun,
            units(Sys.time() - trun)
          )
        )
        
        cat("-----------------------------------------------------------\n")
        
        lmodel[[n]] <- model_fit
        
        
        
        pred_obs_test <- data.frame(pred = predict(lmodel[[n]], test), 
                                    obs = test[[target_var]],
                                    data = "test")  %>% 
          na.omit()
        
        
        
        pred_obs_null_train <- data.frame(
          pred = rep(mean(train[[target_var]]), nrow(train)),
          obs = train[[target_var]])
        
        pred_obs_null_test <- data.frame(
          pred = rep(mean(test[[target_var]]), nrow(test)),
          obs = test[[target_var]])
        
        
        
        # metrics train and test
        pr_train <- getTrainPerf(lmodel[[n]])
        pr_test <- pst_res_mqi(pred_obs_test)
        
        
        # metrics null
        pr_null_train = pst_res_mqi(pred_obs_null_train)
        pr_null_test = pst_res_mqi(pred_obs_null_test)
        
        
        pred_imp <- caret::varImp(
          object = lmodel[[n]]$finalModel,
          scale = FALSE
        )
        
        lpredimp[[n]] <- pred_imp %>%
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
          dplyr::select(predictor, importance) %>%
          dplyr::filter(!is.na(importance)) %>%
          dplyr::arrange(dplyr::desc(importance))
        
        lpredimp_top <- lpredimp[[n]] %>%
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
        
        pred_obs_fulldata <- tibble::tibble(
          obs = dfselrfe[[target_var]],
          pred = as.numeric(predict(lmodel[[n]], dfselrfe))
        ) %>%
          dplyr::mutate(data = "train") %>%
          dplyr::bind_rows(
            pred_obs_test %>%
              dplyr::select(obs, pred, data)
          ) %>%
          dplyr::mutate(
            data = factor(data, levels = c("train", "test")),
            run = n
          )
        
        pred_obs_list[[n]] <- pred_obs_fulldata
        
        g2 <- ggplot(pred_obs_fulldata, aes(x = obs, y = pred)) +
          geom_abline(col = "red", lwd = 2) +
          geom_point(alpha = 0.25) +
          labs(
            title = target_var,
            x = "Observed",
            y = "Predicted"
          ) +
          facet_wrap(~data, scales = "free")
        
        plot(g2)
        
        write.csv2(
          lpredimp[[n]],
          file.path(
            path_results,
            "performance/imp_pred",
            paste0("imp_pred_", n, ".csv")
          ),
          row.names = FALSE
        )
        
        
        dfperf[n, ] <- list(
          model = lmodel[[n]]$modelInfo$label,
          
          target_var = target_var,
          
          n_train = nrow(train),
          ccc_train = pr_train$Trainccc,
          r2_train = pr_train$Trainr2,
          nse_train = pr_train$Trainnse,
          mae_train = pr_train$Trainmae,
          rmse_train = pr_train$Trainrmse,
          mqi_train = pr_train$Trainmqi,
          
          n_test = nrow(test),
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
        
        
        write.csv2(
          dfperf, row.names = F,
          file.path(
            path_results, "performance/csv",
            paste0(target_var, "_performance.csv")))
        
        
        save.image(file.path(path_results, "img", paste0(target_var, ".rdata")))
        
        pred_obs_progress <- dplyr::bind_rows(pred_obs_list)
        
        readr::write_csv(
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
            selection_method = selection_method,
            last_completed_run = n,
            target_var = target_var,
            model_name = models[i]
          ),
          file = model_progress_file
        )
        
        
      } # end for n
      
      
    } # end if start_run else
    
    # save select rfe -------------------------------------------------
    n_obs <- sapply(lrfepred, length)
    seq.max <- seq_len(max(n_obs))
    
    rfe_pred_full <- as.data.frame(sapply(lrfepred, "[", i = seq.max))
    
    
    write.csv2(
      rfe_pred_full, row.names = F,
      file = file.path(
        path_results, "select/rfe/select", "select_full.csv"))
    
    
    
    # save geral rfe ----------------------------------------------- 
    rfe_res_full <- map_dfr(lrferes, ~ .x) %>% 
      mutate(rep = rep(1:nruns, times = sapply(lrferes, nrow))) %>% 
      relocate(rep)
    
    write.csv2(
      rfe_res_full, row.names = F,
      file = file.path(
        path_results, "select/rfe/metric", "rfe_metrics_full.csv"))
    
    
    # save predict importance -------------------------------------
    pred_imp_full <- map_dfr(lpredimp, ~ .x) %>% 
      mutate(rep = rep(1:nruns, times = sapply(lpredimp, nrow))) %>% 
      relocate(rep) %>% 
      `rownames<-`(NULL)
    
    
    write.csv2(
      pred_imp_full, row.names = F,
      file = file.path(
        path_results, "performance/imp_pred", "imp_pred_full.csv"))
    
    
    
    
    
    # save pred_obs_full ------------------------------------------------------
    pred_obs_full <- dplyr::bind_rows(pred_obs_list)
    
    readr::write_csv(
      pred_obs_full,
      file.path(
        path_results,
        "performance/pred_obs",
        "pred_obs_full.csv"
      )
    )
    
    
    # save model_list_full_file -----------------------------------------------
    saveRDS(
      object = lmodel,
      file = model_list_full_file
    )
    
    
    # save model_bundle_full.rds ----------------------------------------------
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
        target_var = target_var,
        model_name = models[i]
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
