ensure_febr_downloader_packages <- function() {
  if (
    exists(
      ".febr_downloader_packages_loaded",
      envir = .GlobalEnv
    ) &&
      isTRUE(
        get(
          ".febr_downloader_packages_loaded",
          envir = .GlobalEnv
        )
      )
  ) {
    return(invisible(TRUE))
  }
  
  source(
    "https://raw.githubusercontent.com/moquedace/funcs/refs/heads/main/install_load_pkg.R"
  )
  
  pkg <- c(
    "dplyr",
    "tidyr",
    "stringr",
    "purrr",
    "tibble",
    "data.table",
    "readr",
    "fs",
    "glue",
    "febr"
  )
  
  install_load_pkg(pkg)
  
  assign(
    ".febr_downloader_packages_loaded",
    TRUE,
    envir = .GlobalEnv
  )
  
  invisible(TRUE)
}

valid_febr_table_names <- function() {
  c(
    "identificacao",
    "versionamento",
    "metadado",
    "observacao",
    "camada"
  )
}

check_febr_table_names <- function(table_names) {
  ensure_febr_downloader_packages()
  
  valid_names <- valid_febr_table_names()
  
  invalid_names <- setdiff(
    table_names,
    valid_names
  )
  
  if (length(invalid_names) > 0) {
    stop(
      "Invalid FEBR table name(s): ",
      paste(invalid_names, collapse = ", "),
      ". Valid names are: ",
      paste(valid_names, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}

detect_febr_dataset_ids <- function(index_tbl) {
  ensure_febr_downloader_packages()
  
  index_tbl <- index_tbl %>%
    tibble::as_tibble()
  
  possible_id_columns <- c(
    "dataset_id",
    "data_set",
    "data.set",
    "dataset",
    "id",
    "codigo",
    "identificacao",
    "data_id"
  )
  
  matched_id_columns <- intersect(
    possible_id_columns,
    names(index_tbl)
  )
  
  if (length(matched_id_columns) > 0) {
    dataset_ids <- index_tbl %>%
      dplyr::pull(
        dplyr::all_of(matched_id_columns[1])
      ) %>%
      as.character() %>%
      stringr::str_trim() %>%
      stats::na.omit() %>%
      unique()
    
    dataset_ids <- dataset_ids[
      stringr::str_detect(dataset_ids, "^[A-Za-z]{3}[0-9]{4}$")
    ]
    
    dataset_ids <- sort(
      tolower(dataset_ids)
    )
    
    if (length(dataset_ids) > 0) {
      return(dataset_ids)
    }
  }
  
  index_long <- index_tbl %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "column_name",
      values_to = "value"
    ) %>%
    dplyr::filter(
      !is.na(value)
    ) %>%
    dplyr::mutate(
      value = stringr::str_trim(value),
      value = tolower(value)
    )
  
  dataset_ids <- index_long %>%
    dplyr::filter(
      stringr::str_detect(value, "^[a-z]{3}[0-9]{4}$")
    ) %>%
    dplyr::distinct(value) %>%
    dplyr::arrange(value) %>%
    dplyr::pull(value)
  
  if (length(dataset_ids) == 0) {
    stop(
      "Could not detect FEBR dataset IDs in the index returned by febr::readIndex()."
    )
  }
  
  dataset_ids
}

get_febr_dataset_ids <- function(
    dataset_ids = "all",
    save_index = FALSE,
    metadata_dir = NULL
) {
  ensure_febr_downloader_packages()
  
  if (
    length(dataset_ids) == 1 &&
    identical(dataset_ids, "all")
  ) {
    index_tbl <- febr::readIndex()
    
    detected_ids <- detect_febr_dataset_ids(
      index_tbl = index_tbl
    )
    
    if (isTRUE(save_index)) {
      if (is.null(metadata_dir)) {
        stop("metadata_dir must be provided when save_index = TRUE.")
      }
      
      fs::dir_create(metadata_dir)
      
      readr::write_csv(
        tibble::as_tibble(index_tbl),
        file.path(metadata_dir, "febr_dataset_index.csv")
      )
    }
    
    return(
      list(
        dataset_ids = detected_ids,
        index_tbl = tibble::as_tibble(index_tbl)
      )
    )
  }
  
  dataset_ids <- dataset_ids %>%
    as.character() %>%
    stringr::str_trim() %>%
    tolower()
  
  invalid_ids <- dataset_ids[
    !stringr::str_detect(dataset_ids, "^[a-z]{3}[0-9]{4}$")
  ]
  
  if (length(invalid_ids) > 0) {
    stop(
      "Invalid FEBR dataset ID(s): ",
      paste(invalid_ids, collapse = ", "),
      ". Expected pattern similar to ctb0003."
    )
  }
  
  list(
    dataset_ids = unique(dataset_ids),
    index_tbl = NULL
  )
}

find_table_recursive <- function(x, table_name) {
  ensure_febr_downloader_packages()
  
  if (is.null(x)) {
    return(NULL)
  }
  
  if (
    table_name %in% names(x) &&
    (
      is.data.frame(x[[table_name]]) ||
        data.table::is.data.table(x[[table_name]])
    )
  ) {
    return(x[[table_name]])
  }
  
  if (
    is.data.frame(x) ||
    data.table::is.data.table(x)
  ) {
    return(NULL)
  }
  
  if (!is.list(x)) {
    return(NULL)
  }
  
  for (element_name in names(x)) {
    result <- find_table_recursive(
      x = x[[element_name]],
      table_name = table_name
    )
    
    if (!is.null(result)) {
      return(result)
    }
  }
  
  NULL
}

write_febr_raw_table <- function(
    table_obj,
    output_path,
    sep = "\t",
    dec = ",",
    quote = TRUE
) {
  ensure_febr_downloader_packages()
  
  if (is.null(table_obj)) {
    return(FALSE)
  }
  
  output_dir <- dirname(output_path)
  
  fs::dir_create(output_dir)
  
  table_obj <- table_obj %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )
  
  data.table::fwrite(
    x = table_obj,
    file = output_path,
    sep = sep,
    dec = dec,
    na = "",
    quote = quote
  )
  
  file.exists(output_path)
}

download_one_febr_dataset <- function(
    dataset_id,
    output_dir,
    table_names = valid_febr_table_names(),
    overwrite = FALSE,
    verbose = TRUE,
    febr_repo = NULL,
    sep = "\t",
    dec = ",",
    quote = TRUE
) {
  ensure_febr_downloader_packages()
  
  dataset_id <- dataset_id %>%
    as.character() %>%
    stringr::str_trim() %>%
    tolower()
  
  if (!stringr::str_detect(dataset_id, "^[a-z]{3}[0-9]{4}$")) {
    stop(
      "Invalid FEBR dataset ID: ",
      dataset_id,
      ". Expected pattern similar to ctb0003."
    )
  }
  
  check_febr_table_names(table_names)
  
  if (is.null(output_dir) || !nzchar(output_dir)) {
    stop("output_dir must be a valid directory path.")
  }
  
  dataset_dir <- file.path(
    output_dir,
    dataset_id
  )
  
  fs::dir_create(dataset_dir)
  
  expected_manifest <- tibble::tibble(
    dataset_id = dataset_id,
    table_name = table_names,
    file_path = file.path(
      dataset_dir,
      paste0(dataset_id, "-", table_names, ".txt")
    ),
    existed_before = file.exists(file_path)
  )
  
  if (
    all(expected_manifest$existed_before) &&
    !isTRUE(overwrite)
  ) {
    return(
      expected_manifest %>%
        dplyr::mutate(
          attempted_download = FALSE,
          downloaded = FALSE,
          saved = TRUE,
          checked_after = file.exists(file_path),
          status = "already_exists"
        )
    )
  }
  
  febr_result <- tryCatch(
    {
      febr::readFEBR(
        data.set = dataset_id,
        data.table = table_names,
        febr.repo = febr_repo,
        verbose = verbose,
        colClasses = "character"
      )
    },
    error = function(e) {
      warning(
        "Download error for dataset ",
        dataset_id,
        ": ",
        e$message
      )
      
      return(NULL)
    }
  )
  
  if (is.null(febr_result)) {
    return(
      expected_manifest %>%
        dplyr::mutate(
          attempted_download = TRUE,
          downloaded = FALSE,
          saved = FALSE,
          checked_after = file.exists(file_path),
          status = "download_error"
        )
    )
  }
  
  output_manifest <- purrr::map_dfr(table_names, function(table_name_i) {
    output_path_i <- file.path(
      dataset_dir,
      paste0(dataset_id, "-", table_name_i, ".txt")
    )
    
    if (
      file.exists(output_path_i) &&
      !isTRUE(overwrite)
    ) {
      return(
        tibble::tibble(
          dataset_id = dataset_id,
          table_name = table_name_i,
          file_path = output_path_i,
          existed_before = TRUE,
          attempted_download = TRUE,
          downloaded = TRUE,
          saved = TRUE,
          checked_after = file.exists(output_path_i),
          status = "already_exists_after_download"
        )
      )
    }
    
    table_obj_i <- find_table_recursive(
      x = febr_result,
      table_name = table_name_i
    )
    
    saved_i <- write_febr_raw_table(
      table_obj = table_obj_i,
      output_path = output_path_i,
      sep = sep,
      dec = dec,
      quote = quote
    )
    
    tibble::tibble(
      dataset_id = dataset_id,
      table_name = table_name_i,
      file_path = output_path_i,
      existed_before = FALSE,
      attempted_download = TRUE,
      downloaded = TRUE,
      saved = saved_i,
      checked_after = file.exists(output_path_i),
      status = dplyr::case_when(
        saved_i ~ "saved",
        is.null(table_obj_i) ~ "table_not_found_in_febr_result",
        TRUE ~ "write_error"
      )
    )
  })
  
  output_manifest
}

download_febr_soildata <- function(
    output_dir,
    dataset_ids = "all",
    table_names = valid_febr_table_names(),
    overwrite = FALSE,
    verbose = TRUE,
    wait_seconds = 0.2,
    metadata_dir = NULL,
    febr_repo = NULL,
    sep = "\t",
    dec = ",",
    quote = TRUE
) {
  ensure_febr_downloader_packages()
  
  check_febr_table_names(table_names)
  
  if (is.null(output_dir) || !nzchar(output_dir)) {
    stop("output_dir must be a valid directory path.")
  }
  
  fs::dir_create(output_dir)
  
  if (!is.null(metadata_dir)) {
    fs::dir_create(metadata_dir)
  }
  
  dataset_info <- get_febr_dataset_ids(
    dataset_ids = dataset_ids,
    save_index = !is.null(metadata_dir),
    metadata_dir = metadata_dir
  )
  
  dataset_ids_to_download <- dataset_info$dataset_ids
  
  if (length(dataset_ids_to_download) == 0) {
    stop("No FEBR dataset IDs were selected.")
  }
  
  message(
    "FEBR datasets selected: ",
    format(
      length(dataset_ids_to_download),
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  download_manifest <- purrr::map_dfr(seq_along(dataset_ids_to_download), function(i) {
    dataset_id_i <- dataset_ids_to_download[i]
    
    message(
      "[",
      i,
      "/",
      length(dataset_ids_to_download),
      "] Processing dataset: ",
      dataset_id_i
    )
    
    manifest_i <- download_one_febr_dataset(
      dataset_id = dataset_id_i,
      output_dir = output_dir,
      table_names = table_names,
      overwrite = overwrite,
      verbose = verbose,
      febr_repo = febr_repo,
      sep = sep,
      dec = dec,
      quote = quote
    )
    
    if (
      !is.null(wait_seconds) &&
      is.numeric(wait_seconds) &&
      wait_seconds > 0
    ) {
      Sys.sleep(wait_seconds)
    }
    
    manifest_i
  })
  
  download_manifest <- download_manifest %>%
    dplyr::mutate(
      checked_after = file.exists(file_path)
    ) %>%
    dplyr::arrange(
      dataset_id,
      table_name
    )
  
  if (!is.null(metadata_dir)) {
    readr::write_csv(
      download_manifest,
      file.path(metadata_dir, "febr_download_manifest.csv")
    )
    
    download_summary <- download_manifest %>%
      dplyr::count(
        status,
        checked_after,
        name = "n_files"
      ) %>%
      dplyr::arrange(
        status,
        checked_after
      )
    
    readr::write_csv(
      download_summary,
      file.path(metadata_dir, "febr_download_summary.csv")
    )
    
    missing_after_download <- download_manifest %>%
      dplyr::filter(
        !checked_after
      )
    
    readr::write_csv(
      missing_after_download,
      file.path(metadata_dir, "febr_missing_after_download.csv")
    )
  }
  
  download_manifest
}
