# 00_caret_rfe_functions.R

validate_metric_config <- function(metric_value, summary_function) {
  
  test_data <- data.frame(
    obs = c(1, 2, 3, 4),
    pred = c(1.1, 1.9, 3.2, 3.8)
  )
  
  metric_output <- summary_function(test_data)
  
  if (!is.numeric(metric_output)) {
    stop("The summary function must return a numeric named vector.")
  }
  
  if (is.null(names(metric_output))) {
    stop("The summary function must return named metrics.")
  }
  
  if (!metric_value %in% names(metric_output)) {
    stop(
      paste0(
        "The selected metric '",
        metric_value,
        "' was not returned by the summary function. Available metrics: ",
        paste(names(metric_output), collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}


make_rcaret_funcs <- function(metric_value, summary_function) {
  
  force(metric_value)
  force(summary_function)
  
  validate_metric_config(
    metric_value = metric_value,
    summary_function = summary_function
  )
  
  funcs <- caret::caretFuncs
  
  funcs$summary <- summary_function
  
  funcs$fit <- function(x, y, first, last, ...) {
    
    if (!is.numeric(y)) {
      stop(
        paste0(
          "RFE received a non-numeric response inside fit. Class: ",
          paste(class(y), collapse = ", ")
        )
      )
    }
    
    dots <- list(...)
    
    # Avoid duplicated or inherited metric arguments from caret internals.
    dots$metric <- NULL
    
    args <- c(
      list(
        x = x,
        y = y,
        metric = metric_value
      ),
      dots
    )
    
    do.call(
      what = caret::train,
      args = args
    )
  }
  
  funcs$pred <- function(object, x) {
    
    pred <- predict(object, x)
    
    if (is.matrix(pred)) {
      pred <- pred[, 1]
    }
    
    if (is.data.frame(pred)) {
      pred <- pred[[1]]
    }
    
    if (!is.numeric(pred)) {
      
      pred_numeric <- suppressWarnings(
        as.numeric(pred)
      )
      
      if (any(is.na(pred_numeric) & !is.na(pred))) {
        stop(
          paste0(
            "RFE prediction could not be safely converted to numeric. Class: ",
            paste(class(pred), collapse = ", ")
          )
        )
      }
      
      pred <- pred_numeric
    }
    
    pred
  }
  
  funcs
}


create_dirs <- function(base_path, sub_dirs) {
  
  purrr::walk(
    sub_dirs,
    function(sub_dir) {
      
      dir_path <- file.path(base_path, sub_dir)
      
      if (!dir.exists(dir_path)) {
        dir.create(
          dir_path,
          recursive = TRUE,
          showWarnings = FALSE
        )
      }
    }
  )
}


safe_read_csv2 <- function(file_path, ...) {
  
  if (!file.exists(file_path)) {
    stop(
      paste0(
        "File not found: ",
        file_path
      )
    )
  }
  
  readr::read_csv2(
    file_path,
    show_col_types = FALSE,
    ...
  )
}


make_repeated_group_folds <- function(group, k, repeats, seed) {
  
  if (length(group) == 0) {
    stop("The grouping vector is empty.")
  }
  
  n_groups <- dplyr::n_distinct(group)
  
  if (n_groups < 2) {
    stop("At least two groups are required to create group folds.")
  }
  
  k_use <- min(k, n_groups)
  
  fold_list <- purrr::map(
    seq_len(repeats),
    function(rep_id) {
      
      set.seed(seed + rep_id - 1)
      
      folds <- caret::groupKFold(
        group = group,
        k = k_use
      )
      
      names(folds) <- paste0(
        "Repeat",
        rep_id,
        "_",
        names(folds)
      )
      
      folds
    }
  )
  
  unlist(
    fold_list,
    recursive = FALSE
  )
}


make_valid_rfe_sizes <- function(rfe_size, n_predictors) {
  
  if (n_predictors < 1) {
    stop("No predictors are available for RFE.")
  }
  
  sizes <- rfe_size[
    rfe_size <= n_predictors
  ]
  
  sizes <- sort(
    unique(
      c(
        sizes,
        n_predictors
      )
    )
  )
  
  sizes <- sizes[
    sizes > 0
  ]
  
  if (length(sizes) == 0) {
    stop("No valid RFE sizes were available after predictor filtering.")
  }
  
  sizes
}


setup_parallel_backend <- function(use_parallel, cores, packages, objects_to_export = character(0)) {
  
  if (!use_parallel) {
    foreach::registerDoSEQ()
    return(NULL)
  }
  
  cores_use <- max(
    1,
    min(
      cores,
      parallel::detectCores()
    )
  )
  
  cl <- parallel::makeCluster(cores_use)
  
  parallel::clusterCall(
    cl,
    function(packages) {
      invisible(
        lapply(
          packages,
          require,
          character.only = TRUE
        )
      )
      NULL
    },
    packages
  )
  
  objects_to_export <- objects_to_export[
    vapply(
      objects_to_export,
      exists,
      logical(1),
      envir = .GlobalEnv
    )
  ]
  
  if (length(objects_to_export) > 0) {
    parallel::clusterExport(
      cl,
      varlist = objects_to_export,
      envir = .GlobalEnv
    )
  }
  
  doParallel::registerDoParallel(cl)
  
  cl
}


close_parallel_backend <- function(cl) {
  
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  
  foreach::registerDoSEQ()
  
  invisible(TRUE)
}


make_qrf_mean_model <- function() {
  
  qrf_mean <- caret::getModelInfo("qrf")$qrf
  
  qrf_mean$predict <- function(modelFit, newdata, submodels = NULL) {
    
    out <- predict(
      modelFit,
      newdata,
      what = mean
    )
    
    if (is.matrix(out)) {
      out <- out[, 1]
    }
    
    out
  }
  
  qrf_mean
}


check_regression_target <- function(data, target_var, stage = "data_check") {
  
  if (!target_var %in% names(data)) {
    stop(
      paste0(
        "Target variable was not found: ",
        target_var,
        " | stage: ",
        stage
      )
    )
  }
  
  y <- data[[target_var]]
  
  if (!is.numeric(y)) {
    stop(
      paste0(
        "Target variable is not numeric: ",
        target_var,
        " | class: ",
        paste(class(y), collapse = ", "),
        " | stage: ",
        stage
      )
    )
  }
  
  if (sum(is.finite(y), na.rm = TRUE) < 3) {
    stop(
      paste0(
        "Target variable has fewer than 3 finite values: ",
        target_var,
        " | stage: ",
        stage
      )
    )
  }
  
  if (dplyr::n_distinct(y, na.rm = TRUE) < 2) {
    stop(
      paste0(
        "Target variable has fewer than 2 unique values: ",
        target_var,
        " | stage: ",
        stage
      )
    )
  }
  
  invisible(TRUE)
}


make_column_diagnostic <- function(data, stage = "data_check") {
  
  tibble::tibble(
    stage = stage,
    column = names(data),
    class = purrr::map_chr(
      data,
      ~ paste(class(.x), collapse = ", ")
    ),
    type = purrr::map_chr(
      data,
      typeof
    ),
    n_na = purrr::map_int(
      data,
      ~ sum(is.na(.x))
    ),
    n_nan = purrr::map_int(
      data,
      ~ if (is.numeric(.x)) sum(is.nan(.x)) else 0L
    ),
    n_inf = purrr::map_int(
      data,
      ~ if (is.numeric(.x)) sum(is.infinite(.x)) else 0L
    ),
    n_unique = purrr::map_int(
      data,
      ~ dplyr::n_distinct(.x, na.rm = TRUE)
    )
  )
}


check_no_invalid_numeric <- function(data, stage = "data_check") {
  
  diagnostic <- make_column_diagnostic(
    data = data,
    stage = stage
  )
  
  bad_columns <- diagnostic %>%
    dplyr::filter(
      n_na > 0 |
        n_nan > 0 |
        n_inf > 0
    )
  
  if (nrow(bad_columns) > 0) {
    stop(
      paste0(
        "Invalid values found at stage ",
        stage,
        " in columns: ",
        paste(bad_columns$column, collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}


make_train_control <- function(method, number, repeats = NULL, index = NULL,
                               summary_function, save_predictions = TRUE,
                               allow_parallel = FALSE) {
  
  if (identical(method, "repeatedcv")) {
    
    caret::trainControl(
      method = method,
      number = number,
      repeats = repeats,
      index = index,
      savePredictions = save_predictions,
      summaryFunction = summary_function,
      allowParallel = allow_parallel
    )
    
  } else {
    
    caret::trainControl(
      method = method,
      number = number,
      index = index,
      savePredictions = save_predictions,
      summaryFunction = summary_function,
      allowParallel = allow_parallel
    )
  }
}


make_rfe_control <- function(method, number, repeats = NULL, index = NULL,
                             functions_object, verbose = FALSE,
                             allow_parallel = FALSE) {
  
  if (identical(method, "repeatedcv")) {
    
    caret::rfeControl(
      method = method,
      number = number,
      repeats = repeats,
      index = index,
      functions = functions_object,
      verbose = verbose,
      allowParallel = allow_parallel
    )
    
  } else {
    
    caret::rfeControl(
      method = method,
      number = number,
      index = index,
      functions = functions_object,
      verbose = verbose,
      allowParallel = allow_parallel
    )
  }
}

      
validate_class_metric_config <- function(metric_value, summary_function) {
  
  test_data <- data.frame(
    obs = factor(
      c("class_a", "class_a", "class_b", "class_b", "class_c", "class_c"),
      levels = c("class_a", "class_b", "class_c")
    ),
    pred = factor(
      c("class_a", "class_b", "class_b", "class_b", "class_c", "class_a"),
      levels = c("class_a", "class_b", "class_c")
    )
  )
  
  metric_output <- summary_function(test_data)
  
  if (!is.numeric(metric_output)) {
    stop("The classification summary function must return a numeric named vector.")
  }
  
  if (is.null(names(metric_output))) {
    stop("The classification summary function must return named metrics.")
  }
  
  if (!metric_value %in% names(metric_output)) {
    stop(
      paste0(
        "The selected metric '",
        metric_value,
        "' was not returned by the classification summary function. Available metrics: ",
        paste(names(metric_output), collapse = ", ")
      )
    )
  }
  
  invisible(TRUE)
}


make_ccaret_funcs <- function(metric_value, summary_function) {
  
  force(metric_value)
  force(summary_function)
  
  validate_class_metric_config(
    metric_value = metric_value,
    summary_function = summary_function
  )
  
  funcs <- caret::caretFuncs
  
  funcs$summary <- summary_function
  
  funcs$fit <- function(x, y, first, last, ...) {
    
    if (!is.factor(y)) {
      stop(
        paste0(
          "RFE received a non-factor response inside classification fit. Class: ",
          paste(class(y), collapse = ", ")
        )
      )
    }
    
    dots <- list(...)
    
    dots$metric <- NULL
    
    args <- c(
      list(
        x = x,
        y = y,
        metric = metric_value
      ),
      dots
    )
    
    do.call(
      what = caret::train,
      args = args
    )
  }
  
  funcs$pred <- function(object, x) {
    
    pred <- predict(object, x)
    
    if (is.matrix(pred)) {
      pred <- pred[, 1]
    }
    
    if (is.data.frame(pred)) {
      pred <- pred[[1]]
    }

    pred
  }

  funcs
}


# ---------------------------------------------------------------------
# validate_resampling_index
# ---------------------------------------------------------------------
# Structural guard (Layer A) against degenerate resampling folds.
# Receives the `index` list of TRAINING indices (one entry per resample),
# as produced by make_repeated_group_folds / groupKFold, and discards
# resamples that:
#   - have fewer than `min_train_n` training rows;
#   - leave fewer than `min_holdout_n` rows for validation
#     (includes the catastrophic case of empty holdout: length(idx) == n_rows);
#   - contain invalid indices (NA, out of 1:n_rows);
#   - (classification) do not contain all classes in TRAINING when
#     `require_all_classes = TRUE` and `target`/`class_levels` are provided.
#
# If `index` is NULL (internally generated resampling by caret, i.e.
# random CV or LOOCV), returns NULL unchanged — in that path, protection
# comes from Layer B (hardened metric functions).

validate_resampling_index <- function(index,
                                       n_rows,
                                       min_train_n = 2,
                                       min_holdout_n = 2,
                                       target = NULL,
                                       class_levels = NULL,
                                       require_all_classes = FALSE) {

  if (is.null(index)) {
    return(NULL)
  }

  index_valid <- purrr::keep(
    index,
    function(idx) {

      idx <- unique(idx)

      structural_ok <- length(idx) >= min_train_n &&
        all(!is.na(idx)) &&
        all(idx >= 1) &&
        all(idx <= n_rows) &&
        (n_rows - length(idx)) >= min_holdout_n

      if (!structural_ok) {
        return(FALSE)
      }

      if (require_all_classes &&
          !is.null(target) &&
          !is.null(class_levels)) {

        train_classes <- unique(as.character(target[idx]))

        if (!all(class_levels %in% train_classes)) {
          return(FALSE)
        }
      }

      TRUE
    }
  )

  n_removed <- length(index) - length(index_valid)

  if (n_removed > 0) {
    warning(
      sprintf(
        "validate_resampling_index: removed %d invalid/degenerate resampling fold(s) out of %d.",
        n_removed,
        length(index)
      )
    )
  }

  if (length(index_valid) == 0) {
    stop(
      "validate_resampling_index: all resampling folds were invalid. Reduce the number of folds (rfe_fold/model_fold) or check the grouping/target variable."
    )
  }

  index_valid
}


# ---------------------------------------------------------------------
# make_group_class_table
# ---------------------------------------------------------------------
# Validates that each group belongs to exactly ONE class (prerequisite
# for grouped AND stratified splitting). Returns a table of group + class.

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


# ---------------------------------------------------------------------
# make_repeated_stratified_group_kfold
# ---------------------------------------------------------------------
# Creates TRAINING indices for grouped resampling (no group leakage between
# train and validation) AND class-stratified (each fold tries to preserve
# the class proportion). Ensures no training fold is missing any class.
# Used by classification_*_v2 scripts when kfold = TRUE.

make_repeated_stratified_group_kfold <- function(data, group_var, target_var,
                                                 k, repeats, seed) {

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
    }

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
      }
    }

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
    }
  }

  index_list
}
