soil_attr_balance <- local({

  # soil_attr_balance.R
#
# Generic function to evaluate completeness and attribute-combination trade-offs
# in soil databases.
#
# The function identifies which combinations of selected soil attributes preserve
# the highest number of complete observations while allowing optional diagnostics
# by sampling unit, depth class, valid value ranges, attribute weights,
# marginal losses, bottleneck attributes, missingness correlation, and optional
# graphical outputs.
#
# This function assumes that the input database has already been cleaned and
# harmonized. It does not read files, write files, convert units, derive new
# soil variables, perform spatial filtering, or apply project-specific
# pedological rules.
#
# When graphs = TRUE, the function returns a list of ggplot objects in the
# output element graphs. Graphs are created only at the end of the function and
# do not affect the combination analysis, ranking, selected dataset, or any
# tabular diagnostic. The package ggplot2 is required only when graphs = TRUE.
#
# Main outputs include:
#
# input_summary
# attr_summary
# row_attr_summary
# combo_summary
# best_combo_by_size
# top_combo_by_size
# selection_candidates
# pareto_combo
# marginal_loss_summary
# attribute_bottleneck_summary
# missingness_correlation
# missingness_correlation_long
# depth_completeness_summary
# unit_completeness_summary
# target_combo_summary
# selected_summary
# selected_data
# graphs
# decision_notes
#
# Recommended documentation:
#
# See soil_attr_balance_documentation.md for full argument descriptions,
# interpretation guidelines, and examples.
#
# See soil_attr_balance_examples.md for practical use cases.
#
# Typical use without graphs:
#
# source("https://raw.githubusercontent.com/moquedace/funcs/main/utils/soil_attr_balance.R")
#
# res <- soil_attr_balance(
#   data = soil_data,
#   attrs = soil_attributes,
#   unit_cols = "coord_id",
#   depth_cols = c("upper_depth_cm", "lower_depth_cm"),
#   required_attrs = "c_gkg",
#   target_attrs = "c_gkg",
#   min_pct = 0.50,
#   ranking_metric = "weighted_score",
#   selection_priority = "richness_first",
#   return_selected = TRUE
# )
#
# Typical use with graphs:
#
# res_graphs <- soil_attr_balance(
#   data = soil_data,
#   attrs = soil_attributes,
#   unit_cols = "coord_id",
#   depth_cols = c("upper_depth_cm", "lower_depth_cm"),
#   required_attrs = "c_gkg",
#   target_attrs = "c_gkg",
#   min_pct = 0.50,
#   ranking_metric = "weighted_score",
#   selection_priority = "richness_first",
#   return_selected = TRUE,
#   graphs = TRUE,
#   graph_top_n = 30
# )
#
# Example graph calls:
#
# res_graphs$graphs$attribute_completeness
# res_graphs$graphs$row_attribute_availability
# res_graphs$graphs$best_combo_by_size
# res_graphs$graphs$marginal_loss
# res_graphs$graphs$bottleneck_intensity
# res_graphs$graphs$selection_candidates
# res_graphs$graphs$missingness_correlation
# res_graphs$graphs$depth_selected_candidate
# res_graphs$graphs$unit_selected_candidate
  
  soil_attr_balance_load_packages <- function() {
    
    pkg <- c(
      "dplyr", "tidyr", "purrr", "stringr", "tibble", "janitor",
      "future", "furrr", "parallelly"
    )
    
    if (!exists("install_load_pkg", mode = "function")) {
      try(
        source(
          "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/utils/install_load_pkg.R"
        ),
        silent = TRUE
      )
    }
    
    if (exists("install_load_pkg", mode = "function")) {
      invisible(
        install_load_pkg(pkg)
      )
    } else {
      
      missing_pkg <- pkg[
        !vapply(
          pkg,
          requireNamespace,
          quietly = TRUE,
          FUN.VALUE = logical(1)
        )
      ]
      
      if (length(missing_pkg) > 0) {
        stop(
          paste0(
            "These packages are missing and could not be loaded: ",
            paste(missing_pkg, collapse = ", ")
          )
        )
      }
      
      invisible(
        lapply(
          pkg,
          library,
          character.only = TRUE
        )
      )
    }
  }
  
  soil_load_graph_packages <- function() {
    
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      
      if (!exists("install_load_pkg", mode = "function")) {
        try(
          source(
            "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/utils/install_load_pkg.R"
          ),
          silent = TRUE
        )
      }
      
      if (exists("install_load_pkg", mode = "function")) {
        invisible(
          install_load_pkg("ggplot2")
        )
      } else {
        stop("Package ggplot2 is required when graphs = TRUE.")
      }
    }
    
    invisible(TRUE)
  }
  
  soil_format_elapsed_time <- function(time_start, time_end = Sys.time(), digits = 2) {
    
    elapsed_time <- time_end - time_start
    
    paste0(
      round(as.numeric(elapsed_time), digits),
      " ",
      as.character(units(elapsed_time))
    )
  }
  
  soil_safe_ratio <- function(x, y) {
    
    if (length(x) == 0 || length(y) == 0) {
      return(numeric(0))
    }
    
    x <- as.numeric(x)
    y <- as.numeric(y)
    
    output_length <- max(length(x), length(y))
    
    if (!length(x) %in% c(1, output_length)) {
      stop("x must have length 1 or the same length as y.")
    }
    
    if (!length(y) %in% c(1, output_length)) {
      stop("y must have length 1 or the same length as x.")
    }
    
    x <- rep(x, length.out = output_length)
    y <- rep(y, length.out = output_length)
    
    ratio <- rep(NA_real_, output_length)
    
    valid_denominator <- !is.na(y) & y > 0
    
    ratio[valid_denominator] <- x[valid_denominator] / y[valid_denominator]
    
    ratio
  }
  
  soil_clean_vector <- function(x) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    janitor::make_clean_names(x)
  }
  
  soil_make_safe_label <- function(x) {
    
    x %>%
      paste(collapse = "_") %>%
      stringr::str_replace_all("[^A-Za-z0-9_]+", "_") %>%
      stringr::str_replace_all("_+", "_") %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("^_|_$", "")
  }
  
  soil_validate_attributes <- function(data, attrs, argument_name = "attrs") {
    
    if (is.null(attrs) || length(attrs) == 0) {
      stop(
        paste0(argument_name, " must contain at least one attribute.")
      )
    }
    
    missing_attrs <- attrs[!attrs %in% names(data)]
    
    if (length(missing_attrs) > 0) {
      stop(
        paste0(
          "These attributes from ",
          argument_name,
          " were not found in data: ",
          paste(missing_attrs, collapse = ", ")
        )
      )
    }
    
    invisible(TRUE)
  }
  
  soil_prepare_attribute_weights <- function(attrs, attribute_weights = NULL) {
    
    if (is.null(attribute_weights)) {
      
      attribute_weights <- rep(1, length(attrs))
      names(attribute_weights) <- attrs
      
      return(attribute_weights)
    }
    
    if (!is.numeric(attribute_weights)) {
      stop("attribute_weights must be a numeric vector.")
    }
    
    if (is.null(names(attribute_weights))) {
      
      if (length(attribute_weights) != length(attrs)) {
        stop(
          "Unnamed attribute_weights must have the same length as attrs."
        )
      }
      
      names(attribute_weights) <- attrs
    }
    
    unknown_weights <- setdiff(names(attribute_weights), attrs)
    
    if (length(unknown_weights) > 0) {
      stop(
        paste0(
          "These attribute_weights names are not present in attrs: ",
          paste(unknown_weights, collapse = ", ")
        )
      )
    }
    
    if (any(is.na(attribute_weights))) {
      stop("attribute_weights cannot contain NA values.")
    }
    
    final_weights <- rep(1, length(attrs))
    names(final_weights) <- attrs
    
    final_weights[names(attribute_weights)] <- attribute_weights
    
    final_weights
  }
  
  soil_validate_valid_ranges <- function(attrs, valid_ranges = NULL) {
    
    if (is.null(valid_ranges)) {
      return(NULL)
    }
    
    if (is.null(names(valid_ranges))) {
      stop("valid_ranges must be a named list.")
    }
    
    unknown_ranges <- setdiff(names(valid_ranges), attrs)
    
    if (length(unknown_ranges) > 0) {
      stop(
        paste0(
          "These valid_ranges names are not present in attrs: ",
          paste(unknown_ranges, collapse = ", ")
        )
      )
    }
    
    bad_ranges <- names(valid_ranges)[
      !purrr::map_lgl(
        valid_ranges,
        function(x) {
          is.numeric(x) &&
            length(x) == 2 &&
            all(!is.na(x)) &&
            x[1] <= x[2]
        }
      )
    ]
    
    if (length(bad_ranges) > 0) {
      stop(
        paste0(
          "These valid_ranges are invalid: ",
          paste(bad_ranges, collapse = ", "),
          ". Each range must be numeric with length 2."
        )
      )
    }
    
    valid_ranges
  }
  
  soil_build_availability_matrix <- function(
      data,
      attrs,
      valid_ranges = NULL,
      use_valid_ranges_for_combinations = FALSE
  ) {
    
    availability <- purrr::map_dfc(
      attrs,
      function(attr) {
        
        x <- data[[attr]]
        available <- !is.na(x)
        
        if (
          use_valid_ranges_for_combinations &&
            !is.null(valid_ranges) &&
            attr %in% names(valid_ranges)
        ) {
          
          x_num <- suppressWarnings(
            as.numeric(x)
          )
          
          range_attr <- valid_ranges[[attr]]
          
          available <- available &
            !is.na(x_num) &
            x_num >= range_attr[1] &
            x_num <= range_attr[2]
        }
        
        tibble::tibble(
          !!attr := available
        )
      }
    ) %>%
      as.matrix()
    
    storage.mode(availability) <- "logical"
    
    availability
  }
  
  soil_summarise_attribute_completeness <- function(
      data,
      attrs,
      valid_ranges = NULL
  ) {
    
    total_rows <- nrow(data)
    
    tibble::tibble(
      attribute = attrs
    ) %>%
      dplyr::mutate(
        n_rows = total_rows,
        has_valid_range = purrr::map_lgl(
          attribute,
          ~ !is.null(valid_ranges) && .x %in% names(valid_ranges)
        ),
        n_non_na = purrr::map_int(
          attribute,
          ~ sum(!is.na(data[[.x]]))
        ),
        n_na = n_rows - n_non_na,
        pct_non_na = soil_safe_ratio(n_non_na, n_rows),
        pct_na = soil_safe_ratio(n_na, n_rows),
        n_valid = purrr::map_int(
          attribute,
          function(attr) {
            
            x <- data[[attr]]
            
            if (!is.null(valid_ranges) && attr %in% names(valid_ranges)) {
              
              x_num <- suppressWarnings(
                as.numeric(x)
              )
              
              range_attr <- valid_ranges[[attr]]
              
              return(
                sum(
                  !is.na(x_num) &
                    x_num >= range_attr[1] &
                    x_num <= range_attr[2]
                )
              )
            }
            
            sum(!is.na(x))
          }
        ),
        n_invalid = purrr::map_int(
          attribute,
          function(attr) {
            
            x <- data[[attr]]
            
            if (!is.null(valid_ranges) && attr %in% names(valid_ranges)) {
              
              x_num <- suppressWarnings(
                as.numeric(x)
              )
              
              range_attr <- valid_ranges[[attr]]
              
              return(
                sum(
                  !is.na(x) &
                    (
                      is.na(x_num) |
                        x_num < range_attr[1] |
                        x_num > range_attr[2]
                    )
                )
              )
            }
            
            0L
          }
        ),
        pct_valid = soil_safe_ratio(n_valid, n_rows),
        pct_invalid = soil_safe_ratio(n_invalid, n_rows)
      ) %>%
      dplyr::arrange(
        dplyr::desc(n_non_na),
        attribute
      )
  }
  
  soil_summarise_row_attribute_availability <- function(
      data,
      attrs,
      availability_matrix
  ) {
    
    tibble::tibble(
      n_attr_available = rowSums(availability_matrix)
    ) %>%
      dplyr::count(
        n_attr_available,
        name = "n_rows"
      ) %>%
      dplyr::mutate(
        pct_rows = soil_safe_ratio(n_rows, sum(n_rows))
      ) %>%
      dplyr::arrange(
        dplyr::desc(n_attr_available)
      )
  }
  
  soil_prepare_depth_vector <- function(
      data,
      depth_cols = NULL,
      depth_class_col = NULL,
      depth_breaks = c(0, 5, 15, 30, 60, 100, 200),
      depth_method = "midpoint"
  ) {
    
    depth_method <- match.arg(
      depth_method,
      choices = c("midpoint", "exact")
    )
    
    if (!is.null(depth_class_col)) {
      
      if (!depth_class_col %in% names(data)) {
        stop(
          paste0(
            "depth_class_col was not found in data: ",
            depth_class_col
          )
        )
      }
      
      return(
        list(
          depth_vec = as.character(data[[depth_class_col]]),
          depth_source = depth_class_col,
          depth_method = "provided_class"
        )
      )
    }
    
    if (!is.null(depth_cols)) {
      
      if (length(depth_cols) != 2) {
        stop("depth_cols must have length 2.")
      }
      
      soil_validate_attributes(
        data = data,
        attrs = depth_cols,
        argument_name = "depth_cols"
      )
      
      upper <- suppressWarnings(
        as.numeric(data[[depth_cols[1]]])
      )
      
      lower <- suppressWarnings(
        as.numeric(data[[depth_cols[2]]])
      )
      
      depth_labels <- paste0(
        depth_breaks[-length(depth_breaks)],
        "_",
        depth_breaks[-1]
      )
      
      if (depth_method == "exact") {
        
        depth_vec <- rep(NA_character_, nrow(data))
        
        for (i in seq_len(length(depth_breaks) - 1)) {
          
          top_i <- depth_breaks[i]
          bottom_i <- depth_breaks[i + 1]
          label_i <- depth_labels[i]
          
          is_interval <- !is.na(upper) &
            !is.na(lower) &
            abs(upper - top_i) < 1e-8 &
            abs(lower - bottom_i) < 1e-8
          
          depth_vec[is_interval] <- label_i
        }
        
        depth_vec[
          is.na(depth_vec) &
            !is.na(upper) &
            !is.na(lower)
        ] <- "other"
        
      } else {
        
        depth_mid <- (upper + lower) / 2
        
        depth_vec <- as.character(
          cut(
            depth_mid,
            breaks = depth_breaks,
            labels = depth_labels,
            include.lowest = TRUE,
            right = FALSE
          )
        )
        
        depth_vec[
          is.na(depth_vec) &
            !is.na(depth_mid) &
            depth_mid < min(depth_breaks)
        ] <- paste0("shallower_than_", min(depth_breaks))
        
        depth_vec[
          is.na(depth_vec) &
            !is.na(depth_mid) &
            depth_mid >= max(depth_breaks)
        ] <- paste0("deeper_than_", max(depth_breaks))
      }
      
      return(
        list(
          depth_vec = depth_vec,
          depth_source = paste(depth_cols, collapse = " | "),
          depth_method = depth_method
        )
      )
    }
    
    list(
      depth_vec = NULL,
      depth_source = NULL,
      depth_method = NULL
    )
  }
  
  soil_generate_attribute_combinations <- function(
      attrs,
      min_size = 2,
      max_size = length(attrs),
      required_attrs = NULL,
      max_combinations = 500000
  ) {
    
    attrs <- unique(attrs)
    required_attrs <- unique(required_attrs)
    
    if (!is.null(required_attrs)) {
      
      missing_required <- required_attrs[!required_attrs %in% attrs]
      
      if (length(missing_required) > 0) {
        stop(
          paste0(
            "These required_attrs were not found in attrs: ",
            paste(missing_required, collapse = ", ")
          )
        )
      }
    }
    
    min_size <- max(min_size, length(required_attrs))
    max_size <- min(max_size, length(attrs))
    
    if (min_size > max_size) {
      stop("min_size is greater than max_size.")
    }
    
    remaining_attrs <- setdiff(attrs, required_attrs)
    
    n_combinations <- sum(
      purrr::map_dbl(
        min_size:max_size,
        function(combo_size) {
          
          choose_size <- combo_size - length(required_attrs)
          
          if (choose_size < 0 || choose_size > length(remaining_attrs)) {
            return(0)
          }
          
          choose(length(remaining_attrs), choose_size)
        }
      )
    )
    
    if (n_combinations > max_combinations) {
      stop(
        paste0(
          "Too many combinations would be generated: ",
          n_combinations,
          ". Reduce max_size, use required_attrs, or increase max_combinations."
        )
      )
    }
    
    combo_list <- list()
    combo_counter <- 1
    
    for (combo_size in min_size:max_size) {
      
      choose_size <- combo_size - length(required_attrs)
      
      if (choose_size < 0 || choose_size > length(remaining_attrs)) {
        next
      }
      
      extra_combos <- if (choose_size == 0) {
        list(character(0))
      } else {
        utils::combn(
          remaining_attrs,
          choose_size,
          simplify = FALSE
        )
      }
      
      for (extra_attrs in extra_combos) {
        
        selected_attrs <- c(required_attrs, extra_attrs)
        selected_attrs <- attrs[attrs %in% selected_attrs]
        
        combo_list[[combo_counter]] <- tibble::tibble(
          n_attributes = length(selected_attrs),
          combo = list(selected_attrs)
        )
        
        combo_counter <- combo_counter + 1
      }
    }
    
    dplyr::bind_rows(combo_list) %>%
      dplyr::mutate(
        combo_id = dplyr::row_number()
      ) %>%
      dplyr::relocate(combo_id)
  }
  
  soil_evaluate_attribute_combinations <- function(
      data,
      attrs,
      all_combos,
      availability_matrix,
      attribute_weights,
      unit_cols = NULL,
      depth_vec = NULL,
      compute_representativeness = TRUE,
      parallel = TRUE,
      n_workers = NULL,
      chunk_size = 500
  ) {
    
    total_rows <- nrow(data)
    
    attr_index <- seq_along(attrs)
    names(attr_index) <- attrs
    
    unit_cols <- unit_cols[
      !is.na(unit_cols) &
        unit_cols %in% names(data)
    ]
    
    unit_totals <- NULL
    
    if (!is.null(unit_cols) && length(unit_cols) > 0) {
      
      unit_totals <- purrr::map_int(
        unit_cols,
        function(unit_col) {
          dplyr::n_distinct(
            data[[unit_col]][!is.na(data[[unit_col]])]
          )
        }
      )
      
      names(unit_totals) <- unit_cols
    }
    
    has_depth <- !is.null(depth_vec)
    
    combo_chunks <- split(
      all_combos,
      ceiling(seq_len(nrow(all_combos)) / chunk_size)
    )
    
    evaluate_chunk <- function(chunk) {
      
      purrr::map_dfr(
        seq_len(nrow(chunk)),
        function(i) {
          
          combo <- chunk$combo[[i]]
          combo_pos <- attr_index[combo]
          
          complete_rows <- rowSums(
            availability_matrix[, combo_pos, drop = FALSE]
          ) == length(combo_pos)
          
          n_rows_complete <- sum(complete_rows)
          n_rows_missing <- total_rows - n_rows_complete
          
          unit_counts <- NULL
          
          if (!is.null(unit_cols) && length(unit_cols) > 0) {
            
            unit_counts <- purrr::map_dfc(
              unit_cols,
              function(unit_col) {
                
                unit_label <- soil_make_safe_label(unit_col)
                
                n_unit_complete <- dplyr::n_distinct(
                  data[[unit_col]][
                    complete_rows &
                      !is.na(data[[unit_col]])
                  ]
                )
                
                tibble::tibble(
                  !!paste0("n_", unit_label, "_complete") := n_unit_complete,
                  !!paste0("pct_", unit_label, "_complete") := soil_safe_ratio(
                    n_unit_complete,
                    unit_totals[[unit_col]]
                  )
                )
              }
            )
          }
          
          mean_n_units_complete <- NA_real_
          
          if (!is.null(unit_counts)) {
            
            unit_n_cols <- names(unit_counts)[
              stringr::str_detect(names(unit_counts), "^n_")
            ]
            
            mean_n_units_complete <- mean(
              as.numeric(
                unlist(unit_counts[unit_n_cols])
              ),
              na.rm = TRUE
            )
          }
          
          n_depth_classes_complete <- NA_integer_
          
          if (has_depth) {
            
            n_depth_classes_complete <- dplyr::n_distinct(
              depth_vec[
                complete_rows &
                  !is.na(depth_vec)
              ]
            )
          }
          
          attribute_weight_sum <- sum(attribute_weights[combo])
          attribute_weight_mean <- mean(attribute_weights[combo])
          
          weighted_score <- attribute_weight_sum * log1p(n_rows_complete)
          
          representativeness_score <- NA_real_
          
          if (compute_representativeness) {
            
            representativeness_score <- log1p(n_rows_complete)
            
            if (!is.na(mean_n_units_complete)) {
              representativeness_score <- representativeness_score *
                log1p(mean_n_units_complete)
            }
            
            if (!is.na(n_depth_classes_complete)) {
              representativeness_score <- representativeness_score *
                log1p(n_depth_classes_complete)
            }
          }
          
          base_result <- tibble::tibble(
            combo_id = chunk$combo_id[[i]],
            n_attributes = length(combo),
            attributes = paste(combo, collapse = " | "),
            attribute_weight_sum = attribute_weight_sum,
            attribute_weight_mean = attribute_weight_mean,
            n_rows_complete = n_rows_complete,
            pct_rows_complete = soil_safe_ratio(n_rows_complete, total_rows),
            n_rows_missing = n_rows_missing,
            pct_rows_missing = soil_safe_ratio(n_rows_missing, total_rows),
            mean_n_units_complete = mean_n_units_complete,
            n_depth_classes_complete = n_depth_classes_complete,
            weighted_score = weighted_score,
            representativeness_score = representativeness_score
          )
          
          if (!is.null(unit_counts)) {
            base_result <- dplyr::bind_cols(
              base_result,
              unit_counts
            )
          }
          
          base_result
        }
      )
    }
    
    if (is.null(n_workers)) {
      n_workers <- max(1, parallelly::availableCores() - 1)
    }
    
    if (parallel && n_workers > 1) {
      
      old_plan <- future::plan()
      
      on.exit(
        future::plan(old_plan),
        add = TRUE
      )
      
      future::plan(
        future::multisession,
        workers = n_workers
      )
      
      combo_summary <- furrr::future_map_dfr(
        combo_chunks,
        evaluate_chunk,
        .options = furrr::furrr_options(seed = TRUE)
      )
      
    } else {
      
      combo_summary <- purrr::map_dfr(
        combo_chunks,
        evaluate_chunk
      )
    }
    
    combo_summary %>%
      dplyr::arrange(
        n_attributes,
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score),
        attributes
      )
  }
  
  soil_rank_combinations <- function(
      combo_summary,
      min_rows = 1,
      min_pct = 0,
      top_n = 20,
      ranking_metric = "n_rows_complete",
      selection_priority = "richness_first"
  ) {
    
    selection_priority <- match.arg(
      selection_priority,
      choices = c("richness_first", "metric_first", "rows_first")
    )
    
    if (!ranking_metric %in% names(combo_summary)) {
      stop(
        paste0(
          "ranking_metric was not found in combo_summary: ",
          ranking_metric
        )
      )
    }
    
    if (!is.numeric(combo_summary[[ranking_metric]])) {
      stop("ranking_metric must refer to a numeric column.")
    }
    
    best_combo_by_size <- combo_summary %>%
      dplyr::group_by(n_attributes) %>%
      dplyr::arrange(
        dplyr::desc(.data[[ranking_metric]]),
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score),
        attributes,
        .by_group = TRUE
      ) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
    top_combo_by_size <- combo_summary %>%
      dplyr::group_by(n_attributes) %>%
      dplyr::arrange(
        dplyr::desc(.data[[ranking_metric]]),
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score),
        attributes,
        .by_group = TRUE
      ) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::ungroup()
    
    selection_candidates_base <- combo_summary %>%
      dplyr::filter(
        n_rows_complete >= min_rows,
        pct_rows_complete >= min_pct
      )
    
    if (selection_priority == "richness_first") {
      
      selection_candidates <- selection_candidates_base %>%
        dplyr::arrange(
          dplyr::desc(n_attributes),
          dplyr::desc(.data[[ranking_metric]]),
          dplyr::desc(n_rows_complete),
          dplyr::desc(weighted_score),
          attributes
        )
      
    } else if (selection_priority == "metric_first") {
      
      selection_candidates <- selection_candidates_base %>%
        dplyr::arrange(
          dplyr::desc(.data[[ranking_metric]]),
          dplyr::desc(n_attributes),
          dplyr::desc(n_rows_complete),
          dplyr::desc(weighted_score),
          attributes
        )
      
    } else if (selection_priority == "rows_first") {
      
      selection_candidates <- selection_candidates_base %>%
        dplyr::arrange(
          dplyr::desc(n_rows_complete),
          dplyr::desc(n_attributes),
          dplyr::desc(.data[[ranking_metric]]),
          dplyr::desc(weighted_score),
          attributes
        )
    }
    
    pareto_sizes <- combo_summary %>%
      dplyr::group_by(n_attributes) %>%
      dplyr::summarise(
        size_max_rows = as.numeric(max(n_rows_complete)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(
        dplyr::desc(n_attributes)
      ) %>%
      dplyr::mutate(
        best_rows_from_larger_size = dplyr::lag(
          cummax(size_max_rows),
          default = -Inf
        ),
        is_pareto_size = size_max_rows > best_rows_from_larger_size
      ) %>%
      dplyr::filter(is_pareto_size) %>%
      dplyr::select(n_attributes)
    
    pareto_combo <- combo_summary %>%
      dplyr::inner_join(
        pareto_sizes,
        by = "n_attributes"
      ) %>%
      dplyr::group_by(n_attributes) %>%
      dplyr::filter(
        n_rows_complete == max(n_rows_complete)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(
        n_attributes,
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score),
        attributes
      )
    
    list(
      best_combo_by_size = best_combo_by_size,
      top_combo_by_size = top_combo_by_size,
      selection_candidates = selection_candidates,
      pareto_combo = pareto_combo
    )
  }
  
  soil_compute_marginal_loss <- function(best_combo_by_size) {
    
    best_combo_by_size %>%
      dplyr::arrange(n_attributes) %>%
      dplyr::mutate(
        previous_attributes = dplyr::lag(attributes),
        previous_n_rows_complete = dplyr::lag(n_rows_complete),
        marginal_rows_lost = previous_n_rows_complete - n_rows_complete,
        marginal_pct_lost = soil_safe_ratio(
          marginal_rows_lost,
          previous_n_rows_complete
        ),
        cumulative_pct_remaining = soil_safe_ratio(
          n_rows_complete,
          dplyr::first(n_rows_complete)
        )
      )
  }
  
  soil_compute_bottleneck_summary <- function(
      combo_summary,
      attrs,
      attr_summary,
      pareto_combo,
      best_combo_by_size,
      required_attrs = NULL
  ) {
    
    combo_lists <- stringr::str_split(
      combo_summary$attributes,
      " \\| "
    )
    
    pareto_ids <- pareto_combo$combo_id
    best_ids <- best_combo_by_size$combo_id
    total_rows <- unique(attr_summary$n_rows)[1]
    required_attrs <- unique(required_attrs)
    
    purrr::map_dfr(
      attrs,
      function(attr) {
        
        has_attr <- purrr::map_lgl(
          combo_lists,
          ~ attr %in% .x
        )
        
        rows_with_attr <- combo_summary$n_rows_complete[has_attr]
        rows_without_attr <- combo_summary$n_rows_complete[!has_attr]
        
        mean_rows_with_attr <- if (length(rows_with_attr) > 0) {
          mean(rows_with_attr, na.rm = TRUE)
        } else {
          NA_real_
        }
        
        median_rows_with_attr <- if (length(rows_with_attr) > 0) {
          stats::median(rows_with_attr, na.rm = TRUE)
        } else {
          NA_real_
        }
        
        mean_rows_without_attr <- if (length(rows_without_attr) > 0) {
          mean(rows_without_attr, na.rm = TRUE)
        } else {
          NA_real_
        }
        
        if (attr %in% required_attrs) {
          
          bottleneck_effect <- NA_real_
          bottleneck_intensity <- NA_real_
          bottleneck_class <- "required_attribute_not_evaluated"
          
        } else if (all(has_attr) || all(!has_attr)) {
          
          bottleneck_effect <- NA_real_
          bottleneck_intensity <- NA_real_
          bottleneck_class <- "not_evaluated"
          
        } else {
          
          bottleneck_effect <- mean_rows_without_attr - mean_rows_with_attr
          bottleneck_intensity <- soil_safe_ratio(
            bottleneck_effect,
            total_rows
          )
          
          bottleneck_class <- dplyr::case_when(
            is.na(bottleneck_intensity) ~ "not_evaluated",
            bottleneck_intensity <= 0 ~ "no_bottleneck_detected",
            bottleneck_intensity <= 0.05 ~ "low_bottleneck",
            bottleneck_intensity <= 0.20 ~ "moderate_bottleneck",
            TRUE ~ "high_bottleneck"
          )
        }
        
        attr_info <- attr_summary %>%
          dplyr::filter(attribute == attr) %>%
          dplyr::select(
            attribute,
            n_non_na,
            pct_non_na,
            n_valid,
            pct_valid
          )
        
        tibble::tibble(
          attribute = attr,
          n_combinations_with_attribute = sum(has_attr),
          pct_combinations_with_attribute = soil_safe_ratio(
            sum(has_attr),
            length(has_attr)
          ),
          mean_rows_with_attribute = mean_rows_with_attr,
          median_rows_with_attribute = median_rows_with_attr,
          mean_rows_without_attribute = mean_rows_without_attr,
          bottleneck_effect = bottleneck_effect,
          bottleneck_intensity = bottleneck_intensity,
          bottleneck_class = bottleneck_class,
          n_pareto_combinations_with_attribute = sum(
            combo_summary$combo_id[has_attr] %in% pareto_ids
          ),
          n_best_by_size_combinations_with_attribute = sum(
            combo_summary$combo_id[has_attr] %in% best_ids
          )
        ) %>%
          dplyr::left_join(
            attr_info,
            by = "attribute"
          ) %>%
          dplyr::relocate(
            attribute,
            n_non_na,
            pct_non_na,
            n_valid,
            pct_valid
          )
      }
    ) %>%
      dplyr::arrange(
        dplyr::desc(dplyr::coalesce(bottleneck_intensity, -Inf)),
        attribute
      )
  }
  
  soil_compute_missingness_correlation <- function(data, attrs) {
    
    if (length(attrs) < 2) {
      return(
        list(
          missingness_correlation = NULL,
          missingness_correlation_long = NULL
        )
      )
    }
    
    missing_matrix <- data %>%
      dplyr::select(
        dplyr::all_of(attrs)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ as.integer(is.na(.x))
        )
      )
    
    cor_mat <- suppressWarnings(
      stats::cor(
        missing_matrix,
        use = "pairwise.complete.obs"
      )
    )
    
    cor_long <- as.data.frame(
      as.table(cor_mat),
      stringsAsFactors = FALSE
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        attribute_1 = Var1,
        attribute_2 = Var2,
        missingness_correlation = Freq
      ) %>%
      dplyr::filter(
        attribute_1 != attribute_2
      ) %>%
      dplyr::mutate(
        pair_id = purrr::map2_chr(
          attribute_1,
          attribute_2,
          ~ paste(sort(c(.x, .y)), collapse = " | ")
        )
      ) %>%
      dplyr::distinct(
        pair_id,
        .keep_all = TRUE
      ) %>%
      dplyr::select(
        -pair_id
      ) %>%
      dplyr::arrange(
        dplyr::desc(abs(missingness_correlation)),
        attribute_1,
        attribute_2
      )
    
    list(
      missingness_correlation = cor_mat,
      missingness_correlation_long = cor_long
    )
  }
  
  soil_compute_target_combo_summary <- function(
      combo_summary,
      target_attrs = NULL,
      target_mode = "all",
      top_n = 20
  ) {
    
    if (is.null(target_attrs) || length(target_attrs) == 0) {
      return(NULL)
    }
    
    if (!target_mode %in% c("all", "any")) {
      stop("target_mode must be 'all' or 'any'.")
    }
    
    combo_target <- combo_summary %>%
      dplyr::mutate(
        attribute_list = stringr::str_split(attributes, " \\| "),
        has_target = purrr::map_lgl(
          attribute_list,
          function(x) {
            if (target_mode == "all") {
              all(target_attrs %in% x)
            } else {
              any(target_attrs %in% x)
            }
          }
        ),
        n_target_attributes = purrr::map_int(
          attribute_list,
          ~ length(intersect(.x, target_attrs))
        ),
        n_additional_attributes = n_attributes - n_target_attributes
      ) %>%
      dplyr::filter(has_target)
    
    top_target_combinations <- combo_target %>%
      dplyr::arrange(
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score),
        dplyr::desc(n_attributes),
        attributes
      ) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(
        -attribute_list,
        -has_target,
        -n_target_attributes
      )
    
    best_target_plus_by_size <- combo_target %>%
      dplyr::filter(
        n_additional_attributes >= 1
      ) %>%
      dplyr::group_by(n_additional_attributes) %>%
      dplyr::arrange(
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score),
        attributes,
        .by_group = TRUE
      ) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -attribute_list,
        -has_target,
        -n_target_attributes
      )
    
    list(
      top_target_combinations = top_target_combinations,
      best_target_plus_by_size = best_target_plus_by_size
    )
  }
  
  soil_compute_depth_completeness_summary <- function(
      data,
      diagnostic_combos,
      availability_matrix,
      attrs,
      depth_vec
  ) {
    
    if (is.null(depth_vec)) {
      return(NULL)
    }
    
    attr_index <- seq_along(attrs)
    names(attr_index) <- attrs
    
    depth_total <- tibble::tibble(
      depth_class = depth_vec
    ) %>%
      dplyr::filter(
        !is.na(depth_class)
      ) %>%
      dplyr::count(
        depth_class,
        name = "n_rows_depth_total"
      )
    
    diagnostic_combos %>%
      dplyr::mutate(
        attribute_list = stringr::str_split(attributes, " \\| ")
      ) %>%
      purrr::pmap_dfr(
        function(...) {
          
          row_info <- list(...)
          combo <- row_info$attribute_list
          combo_pos <- attr_index[combo]
          
          complete_rows <- rowSums(
            availability_matrix[, combo_pos, drop = FALSE]
          ) == length(combo_pos)
          
          tibble::tibble(
            combo_id = row_info$combo_id,
            n_attributes = row_info$n_attributes,
            attributes = row_info$attributes,
            depth_class = depth_vec[complete_rows]
          ) %>%
            dplyr::filter(
              !is.na(depth_class)
            ) %>%
            dplyr::count(
              combo_id,
              n_attributes,
              attributes,
              depth_class,
              name = "n_rows_complete"
            )
        }
      ) %>%
      dplyr::left_join(
        depth_total,
        by = "depth_class"
      ) %>%
      dplyr::mutate(
        pct_rows_complete_within_depth = soil_safe_ratio(
          n_rows_complete,
          n_rows_depth_total
        )
      ) %>%
      dplyr::arrange(
        combo_id,
        depth_class
      )
  }
  
  soil_compute_unit_completeness_summary <- function(
      data,
      diagnostic_combos,
      availability_matrix,
      attrs,
      unit_cols = NULL
  ) {
    
    if (is.null(unit_cols) || length(unit_cols) == 0) {
      return(NULL)
    }
    
    attr_index <- seq_along(attrs)
    names(attr_index) <- attrs
    
    unit_cols <- unit_cols[unit_cols %in% names(data)]
    
    if (length(unit_cols) == 0) {
      return(NULL)
    }
    
    unit_totals <- purrr::map_int(
      unit_cols,
      function(unit_col) {
        dplyr::n_distinct(
          data[[unit_col]][!is.na(data[[unit_col]])]
        )
      }
    )
    
    names(unit_totals) <- unit_cols
    
    diagnostic_combos %>%
      dplyr::mutate(
        attribute_list = stringr::str_split(attributes, " \\| ")
      ) %>%
      purrr::pmap_dfr(
        function(...) {
          
          row_info <- list(...)
          combo <- row_info$attribute_list
          combo_pos <- attr_index[combo]
          
          complete_rows <- rowSums(
            availability_matrix[, combo_pos, drop = FALSE]
          ) == length(combo_pos)
          
          purrr::map_dfr(
            unit_cols,
            function(unit_col) {
              
              n_units_complete <- dplyr::n_distinct(
                data[[unit_col]][
                  complete_rows &
                    !is.na(data[[unit_col]])
                ]
              )
              
              tibble::tibble(
                combo_id = row_info$combo_id,
                n_attributes = row_info$n_attributes,
                attributes = row_info$attributes,
                unit_col = unit_col,
                n_units_total = unit_totals[[unit_col]],
                n_units_complete = n_units_complete,
                pct_units_complete = soil_safe_ratio(
                  n_units_complete,
                  unit_totals[[unit_col]]
                )
              )
            }
          )
        }
      ) %>%
      dplyr::arrange(
        combo_id,
        unit_col
      )
  }
  
  soil_filter_selected_combination <- function(
      data,
      selected_attrs,
      attrs,
      availability_matrix,
      unit_cols = NULL,
      depth_vec = NULL
  ) {
    
    soil_validate_attributes(
      data = data,
      attrs = selected_attrs,
      argument_name = "selected_attrs"
    )
    
    attr_index <- seq_along(attrs)
    names(attr_index) <- attrs
    
    selected_pos <- attr_index[selected_attrs]
    
    selected_rows <- rowSums(
      availability_matrix[, selected_pos, drop = FALSE]
    ) == length(selected_pos)
    
    selected_data <- data[selected_rows, , drop = FALSE]
    
    selected_summary <- tibble::tibble(
      selected_attributes = paste(selected_attrs, collapse = " | "),
      n_attributes = length(selected_attrs),
      n_rows_original = nrow(data),
      n_rows_selected = nrow(selected_data),
      pct_rows_selected = soil_safe_ratio(
        n_rows_selected,
        n_rows_original
      ),
      n_rows_removed = n_rows_original - n_rows_selected
    )
    
    if (!is.null(unit_cols)) {
      
      unit_cols <- unit_cols[unit_cols %in% names(data)]
      
      if (length(unit_cols) > 0) {
        
        unit_summary <- purrr::map_dfc(
          unit_cols,
          function(unit_col) {
            
            unit_label <- soil_make_safe_label(unit_col)
            
            n_units_original <- dplyr::n_distinct(
              data[[unit_col]][!is.na(data[[unit_col]])]
            )
            
            n_units_selected <- dplyr::n_distinct(
              selected_data[[unit_col]][!is.na(selected_data[[unit_col]])]
            )
            
            tibble::tibble(
              !!paste0("n_", unit_label, "_original") := n_units_original,
              !!paste0("n_", unit_label, "_selected") := n_units_selected,
              !!paste0("pct_", unit_label, "_selected") := soil_safe_ratio(
                n_units_selected,
                n_units_original
              )
            )
          }
        )
        
        selected_summary <- dplyr::bind_cols(
          selected_summary,
          unit_summary
        )
      }
    }
    
    if (!is.null(depth_vec)) {
      
      selected_summary <- selected_summary %>%
        dplyr::mutate(
          n_depth_classes_original = dplyr::n_distinct(
            depth_vec[!is.na(depth_vec)]
          ),
          n_depth_classes_selected = dplyr::n_distinct(
            depth_vec[selected_rows & !is.na(depth_vec)]
          )
        )
    }
    
    list(
      selected_attrs = selected_attrs,
      selected_summary = selected_summary,
      selected_data = selected_data
    )
  }
  
  soil_make_decision_notes <- function(
      input_summary,
      attr_summary,
      best_combo_by_size,
      marginal_loss_summary = NULL,
      attribute_bottleneck_summary = NULL,
      target_combo_summary = NULL,
      depth_source = NULL,
      depth_method = NULL,
      unit_cols = NULL,
      attribute_weights,
      valid_ranges = NULL,
      use_valid_ranges_for_combinations = FALSE,
      required_attrs = NULL,
      selection_priority = "richness_first"
  ) {
    
    notes <- character()
    
    notes <- c(
      notes,
      paste0(
        "The analysis evaluated ",
        input_summary$n_combinations,
        " attribute combinations across ",
        input_summary$n_rows,
        " rows."
      )
    )
    
    notes <- c(
      notes,
      paste0(
        "Selection candidates were ordered using selection_priority = ",
        selection_priority,
        "."
      )
    )
    
    if (!is.null(required_attrs) && length(required_attrs) > 0) {
      notes <- c(
        notes,
        paste0(
          "Required attributes were fixed in all evaluated combinations: ",
          paste(required_attrs, collapse = ", "),
          "."
        )
      )
    }
    
    if (all(attribute_weights == 1)) {
      notes <- c(
        notes,
        "All attributes were assigned equal weight because attribute_weights was NULL or all weights were equal."
      )
    } else {
      notes <- c(
        notes,
        "Attribute weights were used to compute weighted_score."
      )
    }
    
    if (is.null(valid_ranges)) {
      notes <- c(
        notes,
        "No valid_ranges were supplied. Completeness was evaluated using non-missing values only."
      )
    } else if (use_valid_ranges_for_combinations) {
      notes <- c(
        notes,
        "valid_ranges were supplied and used in the combination completeness analysis."
      )
    } else {
      notes <- c(
        notes,
        "valid_ranges were supplied for diagnostics, but combinations were evaluated using non-missing values only."
      )
    }
    
    best_overall <- best_combo_by_size %>%
      dplyr::arrange(
        dplyr::desc(n_rows_complete),
        dplyr::desc(weighted_score)
      ) %>%
      dplyr::slice(1)
    
    if (nrow(best_overall) > 0) {
      notes <- c(
        notes,
        paste0(
          "The strongest overall combination by complete rows was: ",
          best_overall$attributes,
          " with ",
          best_overall$n_rows_complete,
          " complete rows."
        )
      )
    }
    
    if (!is.null(target_combo_summary)) {
      
      best_target <- target_combo_summary$top_target_combinations %>%
        dplyr::arrange(
          dplyr::desc(n_rows_complete),
          dplyr::desc(weighted_score)
        ) %>%
        dplyr::slice(1)
      
      if (nrow(best_target) > 0) {
        notes <- c(
          notes,
          paste0(
            "The strongest target combination by complete rows was: ",
            best_target$attributes,
            " with ",
            best_target$n_rows_complete,
            " complete rows."
          )
        )
      }
    }
    
    if (!is.null(marginal_loss_summary)) {
      
      strongest_loss <- marginal_loss_summary %>%
        dplyr::filter(
          !is.na(marginal_rows_lost)
        ) %>%
        dplyr::arrange(
          dplyr::desc(marginal_rows_lost)
        ) %>%
        dplyr::slice(1)
      
      if (nrow(strongest_loss) > 0) {
        notes <- c(
          notes,
          paste0(
            "The largest marginal loss occurred at ",
            strongest_loss$n_attributes,
            " attributes, with ",
            strongest_loss$marginal_rows_lost,
            " rows lost compared with the previous size."
          )
        )
      }
    }
    
    if (!is.null(attribute_bottleneck_summary)) {
      
      strongest_bottleneck <- attribute_bottleneck_summary %>%
        dplyr::filter(
          !is.na(bottleneck_intensity)
        ) %>%
        dplyr::arrange(
          dplyr::desc(bottleneck_intensity)
        ) %>%
        dplyr::slice(1)
      
      if (nrow(strongest_bottleneck) > 0) {
        notes <- c(
          notes,
          paste0(
            "The strongest bottleneck attribute was ",
            strongest_bottleneck$attribute,
            " with bottleneck class ",
            strongest_bottleneck$bottleneck_class,
            "."
          )
        )
      }
    }
    
    if (is.null(depth_source)) {
      notes <- c(
        notes,
        "Depth diagnostics were skipped because no depth information was supplied."
      )
    } else {
      notes <- c(
        notes,
        paste0(
          "Depth diagnostics were computed using: ",
          depth_source,
          " with method ",
          depth_method,
          "."
        )
      )
    }
    
    if (is.null(unit_cols) || length(unit_cols) == 0) {
      notes <- c(
        notes,
        "Unit diagnostics were skipped because unit_cols was NULL."
      )
    } else {
      notes <- c(
        notes,
        paste0(
          "Unit diagnostics were computed using: ",
          paste(unit_cols, collapse = ", "),
          "."
        )
      )
    }
    
    if (!is.null(target_combo_summary)) {
      notes <- c(
        notes,
        "Target attribute summaries were computed."
      )
    }
    
    tibble::tibble(
      note_id = seq_along(notes),
      note = notes
    )
  }
  
  soil_make_graphs <- function(
      attr_summary,
      row_attr_summary,
      best_combo_by_size,
      selection_candidates,
      marginal_loss_summary = NULL,
      attribute_bottleneck_summary = NULL,
      missingness_correlation_long = NULL,
      depth_completeness_summary = NULL,
      unit_completeness_summary = NULL,
      graph_top_n = 30
  ) {
    
    soil_load_graph_packages()
    
    graph_results <- list()
    
    graph_results$attribute_completeness <- attr_summary %>%
      dplyr::mutate(
        attribute = stats::reorder(attribute, pct_non_na)
      ) %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = attribute,
          y = pct_non_na
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Attribute",
        y = "Proportion of non-missing rows",
        title = "Attribute completeness"
      ) +
      ggplot2::theme_minimal()
    
    graph_results$row_attribute_availability <- row_attr_summary %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = n_attr_available,
          y = n_rows
        )
      ) +
      ggplot2::geom_col() +
      ggplot2::labs(
        x = "Number of available attributes per row",
        y = "Number of rows",
        title = "Row-level attribute availability"
      ) +
      ggplot2::theme_minimal()
    
    graph_results$best_combo_by_size <- best_combo_by_size %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = n_attributes,
          y = n_rows_complete
        )
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = "Number of attributes",
        y = "Complete rows",
        title = "Best combination by number of attributes"
      ) +
      ggplot2::theme_minimal()
    
    if (!is.null(marginal_loss_summary)) {
      
      graph_results$marginal_loss <- marginal_loss_summary %>%
        dplyr::filter(
          !is.na(marginal_rows_lost)
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = n_attributes,
            y = marginal_rows_lost
          )
        ) +
        ggplot2::geom_col() +
        ggplot2::labs(
          x = "Number of attributes",
          y = "Marginal rows lost",
          title = "Marginal loss when adding attributes"
        ) +
        ggplot2::theme_minimal()
    }
    
    if (!is.null(attribute_bottleneck_summary)) {
      
      graph_results$bottleneck_intensity <- attribute_bottleneck_summary %>%
        dplyr::filter(
          !is.na(bottleneck_intensity)
        ) %>%
        dplyr::slice_max(
          order_by = bottleneck_intensity,
          n = graph_top_n,
          with_ties = FALSE
        ) %>%
        dplyr::mutate(
          attribute = stats::reorder(attribute, bottleneck_intensity)
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = attribute,
            y = bottleneck_intensity
          )
        ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = "Attribute",
          y = "Bottleneck intensity",
          title = "Attribute bottleneck intensity"
        ) +
        ggplot2::theme_minimal()
    }
    
    if (!is.null(selection_candidates) && nrow(selection_candidates) > 0) {
      
      graph_results$selection_candidates <- selection_candidates %>%
        dplyr::slice_head(n = graph_top_n) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = n_attributes,
            y = n_rows_complete
          )
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            size = weighted_score
          ),
          alpha = 0.7
        ) +
        ggplot2::labs(
          x = "Number of attributes",
          y = "Complete rows",
          size = "Weighted score",
          title = "Top selection candidates"
        ) +
        ggplot2::theme_minimal()
    }
    
    if (!is.null(missingness_correlation_long)) {
      
      graph_results$missingness_correlation <- missingness_correlation_long %>%
        dplyr::filter(
          !is.na(missingness_correlation)
        ) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = attribute_1,
            y = attribute_2,
            fill = missingness_correlation
          )
        ) +
        ggplot2::geom_tile() +
        ggplot2::coord_equal() +
        ggplot2::labs(
          x = "Attribute",
          y = "Attribute",
          fill = "Correlation",
          title = "Missingness correlation"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1
          )
        )
    }
    
    if (
      !is.null(depth_completeness_summary) &&
        !is.null(selection_candidates) &&
        nrow(selection_candidates) > 0
    ) {
      
      selected_combo_id <- selection_candidates$combo_id[[1]]
      
      depth_selected <- depth_completeness_summary %>%
        dplyr::filter(
          combo_id == selected_combo_id
        )
      
      if (nrow(depth_selected) > 0) {
        
        graph_results$depth_selected_candidate <- depth_selected %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = depth_class,
              y = n_rows_complete
            )
          ) +
          ggplot2::geom_col() +
          ggplot2::labs(
            x = "Depth class",
            y = "Complete rows",
            title = "Depth coverage of selected candidate"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 45,
              hjust = 1
            )
          )
      }
    }
    
    if (
      !is.null(unit_completeness_summary) &&
        !is.null(selection_candidates) &&
        nrow(selection_candidates) > 0
    ) {
      
      selected_combo_id <- selection_candidates$combo_id[[1]]
      
      unit_selected <- unit_completeness_summary %>%
        dplyr::filter(
          combo_id == selected_combo_id
        )
      
      if (nrow(unit_selected) > 0) {
        
        graph_results$unit_selected_candidate <- unit_selected %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = unit_col,
              y = pct_units_complete
            )
          ) +
          ggplot2::geom_col() +
          ggplot2::labs(
            x = "Unit column",
            y = "Proportion of complete units",
            title = "Unit coverage of selected candidate"
          ) +
          ggplot2::theme_minimal()
      }
    }
    
    graph_results
  }
  
  function(
      data,
      attrs,
      unit_cols = NULL,
      depth_cols = NULL,
      depth_class_col = NULL,
      depth_breaks = c(0, 5, 15, 30, 60, 100, 200),
      depth_method = "midpoint",
      attribute_weights = NULL,
      valid_ranges = NULL,
      use_valid_ranges_for_combinations = FALSE,
      min_size = 2,
      max_size = length(attrs),
      required_attrs = NULL,
      target_attrs = NULL,
      target_mode = "all",
      min_rows = 1,
      min_pct = 0,
      top_n = 20,
      ranking_metric = "n_rows_complete",
      selection_priority = "richness_first",
      compute_marginal_loss = TRUE,
      compute_bottleneck = TRUE,
      compute_missingness_correlation = TRUE,
      compute_representativeness = TRUE,
      return_selected = FALSE,
      selected_attrs = NULL,
      graphs = FALSE,
      graph_top_n = 30,
      parallel = TRUE,
      n_workers = NULL,
      chunk_size = 500,
      max_combinations = 500000,
      clean_names = FALSE,
      load_packages = TRUE
  ) {
    
    selection_priority <- match.arg(
      selection_priority,
      choices = c("richness_first", "metric_first", "rows_first")
    )
    
    if (load_packages) {
      soil_attr_balance_load_packages()
    }
    
    data_work <- data %>%
      tibble::as_tibble()
    
    if (clean_names) {
      
      data_work <- data_work %>%
        janitor::clean_names()
      
      attrs <- soil_clean_vector(attrs)
      unit_cols <- soil_clean_vector(unit_cols)
      depth_cols <- soil_clean_vector(depth_cols)
      depth_class_col <- soil_clean_vector(depth_class_col)
      required_attrs <- soil_clean_vector(required_attrs)
      target_attrs <- soil_clean_vector(target_attrs)
      selected_attrs <- soil_clean_vector(selected_attrs)
      
      if (!is.null(attribute_weights) && !is.null(names(attribute_weights))) {
        names(attribute_weights) <- janitor::make_clean_names(
          names(attribute_weights)
        )
      }
      
      if (!is.null(valid_ranges) && !is.null(names(valid_ranges))) {
        names(valid_ranges) <- janitor::make_clean_names(
          names(valid_ranges)
        )
      }
    }
    
    attrs <- unique(attrs)
    required_attrs <- unique(required_attrs)
    target_attrs <- unique(target_attrs)
    
    soil_validate_attributes(
      data = data_work,
      attrs = attrs,
      argument_name = "attrs"
    )
    
    if (!is.null(unit_cols)) {
      soil_validate_attributes(
        data = data_work,
        attrs = unit_cols,
        argument_name = "unit_cols"
      )
    }
    
    if (!is.null(required_attrs)) {
      soil_validate_attributes(
        data = data_work,
        attrs = required_attrs,
        argument_name = "required_attrs"
      )
    }
    
    if (!is.null(target_attrs)) {
      soil_validate_attributes(
        data = data_work,
        attrs = target_attrs,
        argument_name = "target_attrs"
      )
    }
    
    attribute_weights <- soil_prepare_attribute_weights(
      attrs = attrs,
      attribute_weights = attribute_weights
    )
    
    valid_ranges <- soil_validate_valid_ranges(
      attrs = attrs,
      valid_ranges = valid_ranges
    )
    
    availability_matrix <- soil_build_availability_matrix(
      data = data_work,
      attrs = attrs,
      valid_ranges = valid_ranges,
      use_valid_ranges_for_combinations = use_valid_ranges_for_combinations
    )
    
    depth_info <- soil_prepare_depth_vector(
      data = data_work,
      depth_cols = depth_cols,
      depth_class_col = depth_class_col,
      depth_breaks = depth_breaks,
      depth_method = depth_method
    )
    
    attr_summary <- soil_summarise_attribute_completeness(
      data = data_work,
      attrs = attrs,
      valid_ranges = valid_ranges
    )
    
    row_attr_summary <- soil_summarise_row_attribute_availability(
      data = data_work,
      attrs = attrs,
      availability_matrix = availability_matrix
    )
    
    all_combos <- soil_generate_attribute_combinations(
      attrs = attrs,
      min_size = min_size,
      max_size = max_size,
      required_attrs = required_attrs,
      max_combinations = max_combinations
    )
    
    message(
      paste0(
        "Evaluating ",
        nrow(all_combos),
        " attribute combinations."
      )
    )
    
    time_start <- Sys.time()
    
    combo_summary <- soil_evaluate_attribute_combinations(
      data = data_work,
      attrs = attrs,
      all_combos = all_combos,
      availability_matrix = availability_matrix,
      attribute_weights = attribute_weights,
      unit_cols = unit_cols,
      depth_vec = depth_info$depth_vec,
      compute_representativeness = compute_representativeness,
      parallel = parallel,
      n_workers = n_workers,
      chunk_size = chunk_size
    )
    
    time_end <- Sys.time()
    
    message(
      paste0(
        "Combination analysis finished in ",
        soil_format_elapsed_time(
          time_start = time_start,
          time_end = time_end,
          digits = 2
        )
      )
    )
    
    ranked_results <- soil_rank_combinations(
      combo_summary = combo_summary,
      min_rows = min_rows,
      min_pct = min_pct,
      top_n = top_n,
      ranking_metric = ranking_metric,
      selection_priority = selection_priority
    )
    
    best_combo_by_size <- ranked_results$best_combo_by_size
    top_combo_by_size <- ranked_results$top_combo_by_size
    selection_candidates <- ranked_results$selection_candidates
    pareto_combo <- ranked_results$pareto_combo
    
    marginal_loss_summary <- NULL
    
    if (compute_marginal_loss) {
      marginal_loss_summary <- soil_compute_marginal_loss(
        best_combo_by_size = best_combo_by_size
      )
    }
    
    attribute_bottleneck_summary <- NULL
    
    if (compute_bottleneck) {
      attribute_bottleneck_summary <- soil_compute_bottleneck_summary(
        combo_summary = combo_summary,
        attrs = attrs,
        attr_summary = attr_summary,
        pareto_combo = pareto_combo,
        best_combo_by_size = best_combo_by_size,
        required_attrs = required_attrs
      )
    }
    
    missingness_results <- list(
      missingness_correlation = NULL,
      missingness_correlation_long = NULL
    )
    
    if (compute_missingness_correlation) {
      missingness_results <- soil_compute_missingness_correlation(
        data = data_work,
        attrs = attrs
      )
    }
    
    target_combo_summary <- soil_compute_target_combo_summary(
      combo_summary = combo_summary,
      target_attrs = target_attrs,
      target_mode = target_mode,
      top_n = top_n
    )
    
    diagnostic_combos <- dplyr::bind_rows(
      best_combo_by_size %>%
        dplyr::mutate(diagnostic_source = "best_combo_by_size"),
      pareto_combo %>%
        dplyr::mutate(diagnostic_source = "pareto_combo"),
      selection_candidates %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::mutate(diagnostic_source = "selected_candidate")
    ) %>%
      dplyr::distinct(
        combo_id,
        .keep_all = TRUE
      )
    
    depth_completeness_summary <- soil_compute_depth_completeness_summary(
      data = data_work,
      diagnostic_combos = diagnostic_combos,
      availability_matrix = availability_matrix,
      attrs = attrs,
      depth_vec = depth_info$depth_vec
    )
    
    unit_completeness_summary <- soil_compute_unit_completeness_summary(
      data = data_work,
      diagnostic_combos = diagnostic_combos,
      availability_matrix = availability_matrix,
      attrs = attrs,
      unit_cols = unit_cols
    )
    
    selected_results <- list(
      selected_attrs = NULL,
      selected_summary = NULL,
      selected_data = NULL
    )
    
    if (return_selected) {
      
      if (is.null(selected_attrs)) {
        
        if (nrow(selection_candidates) > 0) {
          selected_attrs <- selection_candidates$attributes[[1]] %>%
            stringr::str_split(" \\| ") %>%
            purrr::pluck(1)
        } else {
          selected_attrs <- combo_summary$attributes[[1]] %>%
            stringr::str_split(" \\| ") %>%
            purrr::pluck(1)
        }
      }
      
      selected_results <- soil_filter_selected_combination(
        data = data_work,
        selected_attrs = selected_attrs,
        attrs = attrs,
        availability_matrix = availability_matrix,
        unit_cols = unit_cols,
        depth_vec = depth_info$depth_vec
      )
    }
    
    input_summary <- tibble::tibble(
      n_rows = nrow(data_work),
      n_attributes_evaluated = length(attrs),
      n_combinations = nrow(all_combos),
      min_size = min_size,
      max_size = max_size,
      ranking_metric = ranking_metric,
      selection_priority = selection_priority,
      required_attrs = ifelse(
        is.null(required_attrs),
        NA_character_,
        paste(required_attrs, collapse = " | ")
      ),
      target_attrs = ifelse(
        is.null(target_attrs),
        NA_character_,
        paste(target_attrs, collapse = " | ")
      ),
      depth_source = ifelse(
        is.null(depth_info$depth_source),
        NA_character_,
        depth_info$depth_source
      ),
      depth_method = ifelse(
        is.null(depth_info$depth_method),
        NA_character_,
        depth_info$depth_method
      ),
      use_valid_ranges_for_combinations = use_valid_ranges_for_combinations,
      compute_representativeness = compute_representativeness,
      graphs = graphs,
      graph_top_n = graph_top_n
    )
    
    decision_notes <- soil_make_decision_notes(
      input_summary = input_summary,
      attr_summary = attr_summary,
      best_combo_by_size = best_combo_by_size,
      marginal_loss_summary = marginal_loss_summary,
      attribute_bottleneck_summary = attribute_bottleneck_summary,
      target_combo_summary = target_combo_summary,
      depth_source = depth_info$depth_source,
      depth_method = depth_info$depth_method,
      unit_cols = unit_cols,
      attribute_weights = attribute_weights,
      valid_ranges = valid_ranges,
      use_valid_ranges_for_combinations = use_valid_ranges_for_combinations,
      required_attrs = required_attrs,
      selection_priority = selection_priority
    )
    
    graph_results <- NULL
    
    if (graphs) {
      graph_results <- soil_make_graphs(
        attr_summary = attr_summary,
        row_attr_summary = row_attr_summary,
        best_combo_by_size = best_combo_by_size,
        selection_candidates = selection_candidates,
        marginal_loss_summary = marginal_loss_summary,
        attribute_bottleneck_summary = attribute_bottleneck_summary,
        missingness_correlation_long = missingness_results$missingness_correlation_long,
        depth_completeness_summary = depth_completeness_summary,
        unit_completeness_summary = unit_completeness_summary,
        graph_top_n = graph_top_n
      )
    }
    
    list(
      input_summary = input_summary,
      attr_summary = attr_summary,
      row_attr_summary = row_attr_summary,
      combo_summary = combo_summary,
      best_combo_by_size = best_combo_by_size,
      top_combo_by_size = top_combo_by_size,
      selection_candidates = selection_candidates,
      pareto_combo = pareto_combo,
      marginal_loss_summary = marginal_loss_summary,
      attribute_bottleneck_summary = attribute_bottleneck_summary,
      missingness_correlation = missingness_results$missingness_correlation,
      missingness_correlation_long = missingness_results$missingness_correlation_long,
      depth_completeness_summary = depth_completeness_summary,
      unit_completeness_summary = unit_completeness_summary,
      target_combo_summary = target_combo_summary,
      selected_summary = selected_results$selected_summary,
      selected_data = selected_results$selected_data,
      graphs = graph_results,
      decision_notes = decision_notes
    )
  }
})
