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
