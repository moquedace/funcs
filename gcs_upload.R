run_gcloud_auth_in_sdk <- function(gcloud_path = NULL) {
  detect_bin <- function(hints = character(), fallback = "") {
    cands <- c(hints, Sys.which(basename(hints)), fallback)
    cands <- cands[nzchar(cands)]
    found <- cands[file.exists(cands)]
    if (length(found)) return(found[[1]])
    if (nzchar(Sys.which("gcloud"))) return(Sys.which("gcloud"))
    ""
  }
  if (is.null(gcloud_path) || !nzchar(gcloud_path)) {
    gcloud_path <- detect_bin(c(
      "C:\\\\Program Files (x86)\\\\Google\\\\Cloud SDK\\\\google-cloud-sdk\\\\bin\\\\gcloud.cmd",
      "C:\\\\Program Files\\\\Google\\\\Cloud SDK\\\\google-cloud-sdk\\\\bin\\\\gcloud.cmd"
    ))
  }
  if (!nzchar(gcloud_path) || !file.exists(gcloud_path)) stop(sprintf("gcloud not found at: %s", gcloud_path))
  status <- suppressWarnings(system2(gcloud_path, c("auth", "application-default", "login"), stdout = "", stderr = ""))
  if (!is.null(attr(status, "status")) && attr(status, "status") != 0) stop("authentication via gcloud failed.")
  message("authentication completed via gcloud cli.")
}


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
  log_dir = NULL,
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
  
  ps_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  }
  
  is_process_running <- function(process_name) {
    process_name <- sub("\\.exe$", "", process_name, ignore.case = TRUE)
    
    ps_command <- paste(
      "$p = Get-Process -Name",
      ps_quote(process_name),
      "-ErrorAction SilentlyContinue;",
      "if ($null -eq $p) { '0' } else { [string]@($p).Count }"
    )
    
    out <- suppressWarnings(
      system2(
        command = "powershell",
        args = c(
          "-NoProfile",
          "-ExecutionPolicy",
          "Bypass",
          "-Command",
          ps_command
        ),
        stdout = TRUE,
        stderr = TRUE
      )
    )
    
    out <- suppressWarnings(as.integer(tail(out, 1)))
    
    isTRUE(out > 0)
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
  
  if (is.null(log_dir) || !nzchar(log_dir)) {
    default_log_root <- Sys.getenv("USERPROFILE")
    
    if (!nzchar(default_log_root)) {
      default_log_root <- tempdir()
    }
    
    log_dir <- file.path(default_log_root, "gcs_upload_logs")
  }
  
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!dir.exists(log_dir)) {
    stop("Could not create log directory: ", log_dir)
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
  
  if (stop_if_gsutil_is_running && is_process_running("gsutil")) {
    stop(
      "There is already a gsutil process running. ",
      "Cancel it before starting a new upload."
    )
  }
  
  if (stop_if_robocopy_is_running && is_process_running("robocopy")) {
    stop(
      "There is still a robocopy process running. ",
      "Wait until the local copy is fully finished before uploading to GCS."
    )
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
