gcs_local_list_only <- function(
  local_dir,
  file_exts = character(0),
  pattern = NULL,
  recursive = TRUE
) {
  
  # Argument descriptions
  # local_dir: local folder where files will be searched.
  # file_exts: optional vector of file extensions to keep, for example ".tif" or c(".tif", ".vrt").
  # pattern: optional regular expression applied to file basenames.
  # recursive: if TRUE, searches files inside subfolders.
  
  normalize_exts <- function(exts) {
    if (!length(exts)) {
      return(character(0))
    }
    
    exts <- tolower(as.character(exts))
    exts <- ifelse(startsWith(exts, "."), exts, paste0(".", exts))
    
    unique(exts)
  }
  
  ends_with_any <- function(x, exts) {
    if (!length(exts)) {
      return(rep(TRUE, length(x)))
    }
    
    regex <- paste0(
      "(",
      paste0(gsub("\\.", "\\\\.", exts), collapse = "|"),
      ")$"
    )
    
    grepl(regex, tolower(x))
  }
  
  if (!dir.exists(local_dir)) {
    stop("Local directory does not exist: ", local_dir)
  }
  
  local_dir <- normalizePath(
    local_dir,
    winslash = "\\",
    mustWork = TRUE
  )
  
  files <- list.files(
    path = local_dir,
    recursive = recursive,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  
  files <- files[file.exists(files)]
  files <- files[!dir.exists(files)]
  
  exts <- normalize_exts(file_exts)
  files <- files[ends_with_any(files, exts)]
  
  if (!is.null(pattern) && nzchar(pattern)) {
    files <- files[grepl(pattern, basename(files))]
  }
  
  message(
    sprintf(
      "found %d local file(s)%s",
      length(files),
      if (length(exts)) {
        paste0(" with extensions: ", paste(exts, collapse = ", "))
      } else {
        ""
      }
    )
  )
  
  if (length(files)) {
    cat(
      paste(
        sprintf("%03d - %s", seq_along(files), files),
        collapse = "\n"
      ),
      "\n"
    )
  }
  
  invisible(files)
}

gcs_upload_files <- function(
  local_files,
  local_root,
  bucket,
  prefix,
  overwrite = FALSE,
  dry_run = TRUE,
  tries = 3,
  create_prefix_placeholder = FALSE,
  stop_if_gsutil_is_running = TRUE,
  stop_if_robocopy_is_running = TRUE,
  log_dir = file.path(local_root, "logs_gcs_upload"),
  gsutil_path = NULL
) {
  
  # Argument descriptions
  # local_files: vector of local file paths to upload. This works like local "uris".
  # local_root: root local folder used to preserve relative paths inside the bucket.
  # bucket: Google Cloud Storage bucket name, without gs://.
  # prefix: destination path inside the bucket. This works like a folder path.
  # overwrite: if FALSE, existing objects in the bucket are not overwritten.
  # dry_run: if TRUE, only shows what would be uploaded and does not upload files.
  # tries: number of retry attempts for each file upload.
  # create_prefix_placeholder: if TRUE, creates a .keep object in the target prefix.
  # stop_if_gsutil_is_running: if TRUE, stops when another gsutil process is already running.
  # stop_if_robocopy_is_running: if TRUE, stops when robocopy is still running.
  # log_dir: local folder where upload logs and the temporary lock file will be saved.
  # gsutil_path: optional full path to gsutil.cmd. If NULL, the function tries to detect it.
  
  detect_bin <- function(hints = character()) {
    candidates <- c(
      hints,
      Sys.which("gsutil"),
      Sys.which("gsutil.cmd")
    )
    
    candidates <- candidates[nzchar(candidates)]
    candidates <- unique(candidates)
    found <- candidates[file.exists(candidates)]
    
    if (length(found)) {
      return(found[[1]])
    }
    
    ""
  }
  
  sanitize_file_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    
    x
  }
  
  escape_regex <- function(x) {
    gsub("([\\.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", x)
  }
  
  normalize_gcs_uri <- function(bucket, prefix) {
    bucket <- gsub("^gs://", "", bucket)
    bucket <- gsub("/+$", "", bucket)
    
    prefix <- gsub("^/+", "", prefix)
    prefix <- gsub("/+$", "", prefix)
    
    if (!nzchar(prefix)) {
      return(paste0("gs://", bucket))
    }
    
    paste0("gs://", bucket, "/", prefix)
  }
  
  make_gcs_file_uri <- function(local_file, local_root, gcs_root_uri) {
    local_file_norm <- normalizePath(
      local_file,
      winslash = "/",
      mustWork = TRUE
    )
    
    local_root_norm <- normalizePath(
      local_root,
      winslash = "/",
      mustWork = TRUE
    )
    
    relative_path <- sub(
      paste0("^", escape_regex(local_root_norm), "/?"),
      "",
      local_file_norm
    )
    
    relative_path <- gsub("\\\\", "/", relative_path)
    
    paste0(gcs_root_uri, "/", relative_path)
  }
  
  upload_one <- function(gsutil_path, source_file, destination_uri, overwrite) {
    args <- c("cp")
    
    if (!overwrite) {
      args <- c(args, "-n")
    }
    
    args <- c(
      args,
      shQuote(source_file),
      shQuote(destination_uri)
    )
    
    status <- suppressWarnings(
      system2(
        command = gsutil_path,
        args = args,
        stdout = "",
        stderr = ""
      )
    )
    
    is.null(attr(status, "status")) || attr(status, "status") == 0
  }
  
  retry_upload <- function(gsutil_path, source_file, destination_uri, overwrite, tries = 3) {
    for (i in seq_len(tries)) {
      ok <- upload_one(
        gsutil_path = gsutil_path,
        source_file = source_file,
        destination_uri = destination_uri,
        overwrite = overwrite
      )
      
      if (ok) {
        return(TRUE)
      }
      
      Sys.sleep(2^(i - 1))
    }
    
    FALSE
  }
  
  if (.Platform$OS.type != "windows") {
    stop("This function was prepared for Windows because it uses gsutil.cmd.")
  }
  
  if (!length(local_files)) {
    message("no local files provided.")
    return(invisible(character(0)))
  }
  
  if (!dir.exists(local_root)) {
    stop("Local root does not exist: ", local_root)
  }
  
  local_files <- normalizePath(
    local_files,
    winslash = "\\",
    mustWork = TRUE
  )
  
  local_root <- normalizePath(
    local_root,
    winslash = "\\",
    mustWork = TRUE
  )
  
  local_files <- local_files[file.exists(local_files)]
  local_files <- local_files[!dir.exists(local_files)]
  
  if (!length(local_files)) {
    message("no valid local files to upload.")
    return(invisible(character(0)))
  }
  
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (is.null(gsutil_path) || !nzchar(gsutil_path)) {
    gsutil_path <- detect_bin(
      hints = c(
        "C:\\Program Files (x86)\\Google\\Cloud SDK\\google-cloud-sdk\\bin\\gsutil.cmd",
        "C:\\Program Files\\Google\\Cloud SDK\\google-cloud-sdk\\bin\\gsutil.cmd"
      )
    )
  }
  
  if (!nzchar(gsutil_path) || !file.exists(gsutil_path)) {
    stop("gsutil not found. Check Google Cloud SDK installation.")
  }
  
  if (stop_if_gsutil_is_running) {
    tasklist_output <- system2(
      command = "tasklist",
      args = c("/FI", shQuote("IMAGENAME eq gsutil.exe")),
      stdout = TRUE,
      stderr = TRUE
    )
    
    gsutil_is_running <- any(
      grepl("gsutil.exe", tasklist_output, ignore.case = TRUE)
    )
    
    if (gsutil_is_running) {
      stop(
        "There is already a gsutil process running. ",
        "Cancel it before starting a new upload."
      )
    }
  }
  
  if (stop_if_robocopy_is_running) {
    tasklist_output <- system2(
      command = "tasklist",
      args = c("/FI", shQuote("IMAGENAME eq robocopy.exe")),
      stdout = TRUE,
      stderr = TRUE
    )
    
    robocopy_is_running <- any(
      grepl("robocopy.exe", tasklist_output, ignore.case = TRUE)
    )
    
    if (robocopy_is_running) {
      stop(
        "There is still a robocopy process running. ",
        "Wait until the local copy is fully finished before uploading to GCS."
      )
    }
  }
  
  gcs_root_uri <- normalize_gcs_uri(
    bucket = bucket,
    prefix = prefix
  )
  
  destination_name <- sanitize_file_name(
    gsub("^gs://", "", gcs_root_uri)
  )
  
  lock_file <- file.path(
    log_dir,
    paste0("gcs_upload_selected_", destination_name, ".lock")
  )
  
  if (file.exists(lock_file)) {
    stop(
      "A lock file already exists: ", lock_file, "\n",
      "If no upload is running, delete this lock file manually and run the function again."
    )
  }
  
  writeLines(
    text = c(
      paste("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("Local root:", local_root),
      paste("GCS destination:", gcs_root_uri),
      paste("Dry run:", dry_run),
      paste("Overwrite:", overwrite),
      paste("Files:", length(local_files))
    ),
    con = lock_file
  )
  
  on.exit(
    {
      if (file.exists(lock_file)) {
        unlink(lock_file)
      }
    },
    add = TRUE
  )
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  log_file <- file.path(
    log_dir,
    paste0(
      "gcs_upload_selected_",
      destination_name,
      "_",
      timestamp,
      ifelse(dry_run, "_dry_run", ""),
      ".log"
    )
  )
  
  destination_uris <- vapply(
    local_files,
    make_gcs_file_uri,
    character(1),
    local_root = local_root,
    gcs_root_uri = gcs_root_uri
  )
  
  if (create_prefix_placeholder && !dry_run) {
    keep_file <- tempfile(pattern = "gcs_prefix_", fileext = ".keep")
    writeLines("prefix placeholder", keep_file)
    
    keep_uri <- paste0(gcs_root_uri, "/.keep")
    
    suppressWarnings(
      system2(
        command = gsutil_path,
        args = c("cp", shQuote(keep_file), shQuote(keep_uri)),
        stdout = "",
        stderr = ""
      )
    )
    
    unlink(keep_file)
  }
  
  message(
    if (dry_run) {
      "dry-run enabled. no files will be uploaded."
    } else {
      "starting selected file upload."
    }
  )
  
  writeLines(
    text = c(
      paste("Local root:", local_root),
      paste("GCS root:", gcs_root_uri),
      paste("Dry run:", dry_run),
      paste("Overwrite:", overwrite),
      "",
      paste0(local_files, " -> ", destination_uris)
    ),
    con = log_file
  )
  
  uploaded <- character(0)
  failed <- character(0)
  
  start_time <- Sys.time()
  
  for (i in seq_along(local_files)) {
    source_file <- local_files[[i]]
    destination_uri <- destination_uris[[i]]
    
    if (dry_run) {
      message(
        sprintf(
          "[dry-run] %03d/%03d %s -> %s",
          i,
          length(local_files),
          source_file,
          destination_uri
        )
      )
      
      uploaded <- c(uploaded, destination_uri)
      next
    }
    
    message(
      sprintf(
        "uploading %03d/%03d: %s",
        i,
        length(local_files),
        basename(source_file)
      )
    )
    
    ok <- retry_upload(
      gsutil_path = gsutil_path,
      source_file = source_file,
      destination_uri = destination_uri,
      overwrite = overwrite,
      tries = tries
    )
    
    if (ok) {
      message(sprintf("uploaded: %s", destination_uri))
      uploaded <- c(uploaded, destination_uri)
    } else {
      warning(sprintf("upload failed after retries: %s", source_file))
      failed <- c(failed, source_file)
    }
  }
  
  end_time <- Sys.time()
  
  duration <- difftime(end_time, start_time, units = "mins")
  
  message("selected file upload finished.")
  message(sprintf("uploaded: %d file(s)", length(uploaded)))
  message(sprintf("failed: %d file(s)", length(failed)))
  message(sprintf("duration: %.2f minutes", as.numeric(duration)))
  message(sprintf("log file: %s", log_file))
  
  invisible(
    list(
      local_root = local_root,
      gcs_root_uri = gcs_root_uri,
      local_files = local_files,
      destination_uris = destination_uris,
      uploaded = uploaded,
      failed = failed,
      log_file = log_file,
      lock_file = lock_file,
      dry_run = dry_run,
      overwrite = overwrite,
      start_time = start_time,
      end_time = end_time,
      duration_minutes = as.numeric(duration)
    )
  )
}

gcs_upload_with_gsutil <- function(
  local_dir,
  bucket,
  prefix,
  log_dir = file.path(local_dir, "logs_gcs_upload"),
  dry_run = TRUE,
  parallel = TRUE,
  recursive = TRUE,
  delete_extra_in_bucket = FALSE,
  create_prefix_placeholder = FALSE,
  stop_if_gsutil_is_running = TRUE,
  exclude_regex = NULL,
  gsutil_path = NULL
) {
  
  # Argument descriptions
  # local_dir: local folder to upload. Use only a folder with completed files.
  # bucket: Google Cloud Storage bucket name, without gs://.
  # prefix: destination path inside the bucket. This works like a folder path in GCS.
  # log_dir: local folder where upload logs and the temporary lock file will be saved.
  # dry_run: if TRUE, gsutil only shows what would be synchronized and does not upload files.
  # parallel: if TRUE, uses gsutil -m for parallel transfers.
  # recursive: if TRUE, uploads files inside subfolders.
  # delete_extra_in_bucket: if TRUE, deletes objects in the bucket prefix that do not exist locally. Keep FALSE for safety.
  # create_prefix_placeholder: if TRUE, creates a .keep object in the bucket prefix before upload.
  # stop_if_gsutil_is_running: if TRUE, stops when another gsutil process is already running.
  # exclude_regex: optional regular expression to exclude files from upload.
  # gsutil_path: optional full path to gsutil.cmd. If NULL, the function tries to detect it.
  
  detect_bin <- function(hints = character()) {
    candidates <- c(
      hints,
      Sys.which("gsutil"),
      Sys.which("gsutil.cmd")
    )
    
    candidates <- candidates[nzchar(candidates)]
    candidates <- unique(candidates)
    found <- candidates[file.exists(candidates)]
    
    if (length(found)) {
      return(found[[1]])
    }
    
    return("")
  }
  
  sanitize_file_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    
    return(x)
  }
  
  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }
  
  normalize_gcs_uri <- function(bucket, prefix) {
    bucket <- gsub("^gs://", "", bucket)
    bucket <- gsub("/+$", "", bucket)
    prefix <- gsub("^/+", "", prefix)
    prefix <- gsub("/+$", "", prefix)
    
    if (!nzchar(prefix)) {
      return(paste0("gs://", bucket))
    }
    
    return(paste0("gs://", bucket, "/", prefix))
  }
  
  if (.Platform$OS.type != "windows") {
    stop("This function was prepared for Windows because it calls gsutil through PowerShell.")
  }
  
  if (!dir.exists(local_dir)) {
    stop("Local directory does not exist: ", local_dir)
  }
  
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (is.null(gsutil_path) || !nzchar(gsutil_path)) {
    gsutil_path <- detect_bin(
      hints = c(
        "C:\\Program Files (x86)\\Google\\Cloud SDK\\google-cloud-sdk\\bin\\gsutil.cmd",
        "C:\\Program Files\\Google\\Cloud SDK\\google-cloud-sdk\\bin\\gsutil.cmd"
      )
    )
  }
  
  if (!nzchar(gsutil_path) || !file.exists(gsutil_path)) {
    stop("gsutil not found. Check Google Cloud SDK installation.")
  }
  
  if (isTRUE(delete_extra_in_bucket)) {
    warning(
      "delete_extra_in_bucket = TRUE will delete objects from the bucket prefix ",
      "when they do not exist in local_dir. Use this only when you really want mirroring."
    )
  }
  
  if (stop_if_gsutil_is_running) {
    tasklist_output <- system2(
      command = "tasklist",
      args = c("/FI", shQuote("IMAGENAME eq gsutil.exe")),
      stdout = TRUE,
      stderr = TRUE
    )
    
    gsutil_is_running <- any(
      grepl("gsutil.exe", tasklist_output, ignore.case = TRUE)
    )
    
    if (gsutil_is_running) {
      stop(
        "There is already a gsutil process running. ",
        "Cancel it before starting a new upload."
      )
    }
  }
  
  local_dir <- normalizePath(local_dir, winslash = "\\", mustWork = TRUE)
  gcs_uri <- normalize_gcs_uri(bucket = bucket, prefix = prefix)
  
  destination_name <- sanitize_file_name(
    paste0(gsub("^gs://", "", gcs_uri))
  )
  
  lock_file <- file.path(
    log_dir,
    paste0("gcs_upload_", destination_name, ".lock")
  )
  
  if (file.exists(lock_file)) {
    stop(
      "A lock file already exists: ", lock_file, "\n",
      "If no upload is running, delete this lock file manually and run the function again."
    )
  }
  
  writeLines(
    text = c(
      paste("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("Local source:", local_dir),
      paste("GCS destination:", gcs_uri),
      paste("Dry run:", dry_run),
      paste("Parallel:", parallel),
      paste("Delete extra in bucket:", delete_extra_in_bucket)
    ),
    con = lock_file
  )
  
  on.exit(
    {
      if (file.exists(lock_file)) {
        unlink(lock_file)
      }
    },
    add = TRUE
  )
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  log_file <- file.path(
    log_dir,
    paste0(
      "gcs_upload_",
      destination_name,
      "_",
      timestamp,
      ifelse(dry_run, "_dry_run", ""),
      ".log"
    )
  )
  
  if (create_prefix_placeholder && !dry_run) {
    keep_file <- tempfile(pattern = "gcs_prefix_", fileext = ".keep")
    writeLines("prefix placeholder", keep_file)
    
    keep_uri <- paste0(gcs_uri, "/.keep")
    
    placeholder_args <- c(
      "cp",
      ps_quote(keep_file),
      ps_quote(keep_uri)
    )
    
    placeholder_command <- paste(
      c(
        "&",
        ps_quote(gsutil_path),
        placeholder_args
      ),
      collapse = " "
    )
    
    placeholder_status <- system2(
      command = "powershell",
      args = c(
        "-NoProfile",
        "-ExecutionPolicy",
        "Bypass",
        "-Command",
        placeholder_command
      ),
      stdout = "",
      stderr = ""
    )
    
    unlink(keep_file)
    
    if (!is.null(attr(placeholder_status, "status")) && attr(placeholder_status, "status") != 0) {
      warning("Could not create prefix placeholder in GCS.")
    }
  }
  
  gsutil_args <- c()
  
  if (parallel) {
    gsutil_args <- c(gsutil_args, "-m")
  }
  
  gsutil_args <- c(gsutil_args, "rsync")
  
  if (dry_run) {
    gsutil_args <- c(gsutil_args, "-n")
  }
  
  if (recursive) {
    gsutil_args <- c(gsutil_args, "-r")
  }
  
  if (delete_extra_in_bucket) {
    gsutil_args <- c(gsutil_args, "-d")
  }
  
  if (!is.null(exclude_regex) && nzchar(exclude_regex)) {
    gsutil_args <- c(gsutil_args, "-x", exclude_regex)
  }
  
  gsutil_args <- c(gsutil_args, local_dir, gcs_uri)
  
  powershell_command <- paste(
    c(
      "&",
      ps_quote(gsutil_path),
      vapply(gsutil_args, ps_quote, character(1)),
      "2>&1",
      "|",
      "Tee-Object",
      "-FilePath",
      ps_quote(log_file),
      ";",
      "$exit_code = $LASTEXITCODE",
      ";",
      "exit $exit_code"
    ),
    collapse = " "
  )
  
  cat("Starting GCS upload with gsutil rsync...\n")
  cat("Local source: ", local_dir, "\n", sep = "")
  cat("GCS destination: ", gcs_uri, "\n", sep = "")
  cat("Dry run: ", dry_run, "\n", sep = "")
  cat("Parallel: ", parallel, "\n", sep = "")
  cat("Delete extra in bucket: ", delete_extra_in_bucket, "\n", sep = "")
  cat("Log file: ", log_file, "\n\n", sep = "")
  
  start_time <- Sys.time()
  
  exit_code <- suppressWarnings(
    system2(
      command = "powershell",
      args = c(
        "-NoProfile",
        "-ExecutionPolicy",
        "Bypass",
        "-Command",
        powershell_command
      ),
      stdout = "",
      stderr = ""
    )
  )
  
  end_time <- Sys.time()
  
  duration <- difftime(end_time, start_time, units = "mins")
  
  status_code <- attr(exit_code, "status")
  
  if (is.null(status_code)) {
    status_code <- 0
  }
  
  cat("\nGCS upload finished.\n")
  cat("Duration: ", round(as.numeric(duration), 2), " minutes\n", sep = "")
  cat("Exit code: ", status_code, "\n", sep = "")
  cat("Log file: ", log_file, "\n", sep = "")
  
  if (status_code != 0) {
    warning("gsutil reported a failure. Check the log file: ", log_file)
  }
  
  invisible(
    list(
      local_dir = local_dir,
      bucket = bucket,
      prefix = prefix,
      gcs_uri = gcs_uri,
      log_file = log_file,
      lock_file = lock_file,
      dry_run = dry_run,
      parallel = parallel,
      recursive = recursive,
      delete_extra_in_bucket = delete_extra_in_bucket,
      exit_code = status_code,
      start_time = start_time,
      end_time = end_time,
      duration_minutes = as.numeric(duration)
    )
  )
}
