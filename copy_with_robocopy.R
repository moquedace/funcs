copy_with_robocopy <- function(
  source_dir,
  destination_dir,
  log_dir = file.path(destination_dir, "logs"),
  mt_threads = 16,
  dry_run = TRUE,
  stop_if_robocopy_is_running = TRUE,
  protect_newer_destination_files = TRUE,
  show_progress = TRUE,
  retry_count = 2,
  retry_wait_seconds = 5
) {
  
  # Argument descriptions
  # source_dir: source folder path. This is the NAS folder from which files will be copied.
  # destination_dir: destination folder path. This is the local folder where files will be saved.
  # log_dir: folder where robocopy log files and the temporary lock file will be stored.
  # mt_threads: number of parallel robocopy threads. Higher values can speed up many-file transfers, but may overload VPN or NAS.
  # dry_run: if TRUE, robocopy only lists what would be copied and does not copy files. If FALSE, files are copied.
  # stop_if_robocopy_is_running: if TRUE, the function stops when another robocopy process is already running.
  # protect_newer_destination_files: if TRUE, files in the destination that are newer than source files are not overwritten.
  # show_progress: if TRUE, robocopy prints progress and estimated time in the R console.
  # retry_count: number of retry attempts for failed file copies.
  # retry_wait_seconds: waiting time in seconds between retry attempts.
  
  interpret_robocopy_exit_code <- function(exit_code) {
    exit_message <- switch(
      as.character(exit_code),
      "0" = "No files were copied. Source and destination are already synchronized.",
      "1" = "Files were copied successfully.",
      "2" = "Extra files or directories were detected in the destination. No critical error.",
      "3" = "Files were copied and extra files or directories were detected. No critical error.",
      "4" = "Mismatched files or directories were detected. No critical error, but check the log.",
      "5" = "Files were copied and mismatched files or directories were detected. Check the log.",
      "6" = "Extra and mismatched files or directories were detected. Check the log.",
      "7" = "Files were copied, and extra or mismatched files were detected. Check the log.",
      "Robocopy reported a failure."
    )
    
    return(exit_message)
  }
  
  sanitize_file_name <- function(x) {
    x <- gsub("[^A-Za-z0-9_]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_|_$", "", x)
    
    return(x)
  }
  
  if (.Platform$OS.type != "windows") {
    stop("This function requires Windows because it uses robocopy.")
  }
  
  if (!dir.exists(source_dir)) {
    stop("Source directory does not exist or is not accessible: ", source_dir)
  }
  
  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!is.numeric(mt_threads) || length(mt_threads) != 1 || mt_threads < 1) {
    stop("mt_threads must be a single numeric value greater than or equal to 1.")
  }
  
  mt_threads <- as.integer(mt_threads)
  
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
        "There is already a robocopy process running. ",
        "Cancel it before starting a new copy."
      )
    }
  }
  
  destination_name <- basename(
    normalizePath(destination_dir, winslash = "\\", mustWork = FALSE)
  )
  
  lock_file <- file.path(
    log_dir,
    paste0("robocopy_", sanitize_file_name(destination_name), ".lock")
  )
  
  if (file.exists(lock_file)) {
    stop(
      "A lock file already exists: ", lock_file, "\n",
      "If no robocopy process is running, delete this lock file manually and run the function again."
    )
  }
  
  writeLines(
    text = c(
      paste("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("Source:", source_dir),
      paste("Destination:", destination_dir),
      paste("Dry run:", dry_run),
      paste("Threads:", mt_threads)
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
      "robocopy_",
      sanitize_file_name(destination_name),
      "_mt",
      mt_threads,
      "_",
      timestamp,
      ifelse(dry_run, "_dry_run", ""),
      ".log"
    )
  )
  
  robocopy_args <- c(
    shQuote(source_dir),
    shQuote(destination_dir),
    "/E",
    "/Z",
    "/J",
    paste0("/MT:", mt_threads),
    paste0("/R:", retry_count),
    paste0("/W:", retry_wait_seconds),
    "/FFT",
    "/COPY:DAT",
    "/DCOPY:DAT",
    paste0("/LOG:", shQuote(log_file))
  )
  
  if (protect_newer_destination_files) {
    robocopy_args <- c(robocopy_args, "/XO")
  }
  
  if (show_progress) {
    robocopy_args <- c(robocopy_args, "/TEE", "/ETA")
  } else {
    robocopy_args <- c(robocopy_args, "/NP")
  }
  
  if (dry_run) {
    robocopy_args <- c(robocopy_args, "/L")
  }
  
  cat("Starting robocopy...\n")
  cat("Source: ", source_dir, "\n", sep = "")
  cat("Destination: ", destination_dir, "\n", sep = "")
  cat("Log file: ", log_file, "\n", sep = "")
  cat("Threads: ", mt_threads, "\n", sep = "")
  cat("Dry run: ", dry_run, "\n", sep = "")
  cat("Protect newer destination files: ", protect_newer_destination_files, "\n\n", sep = "")
  
  start_time <- Sys.time()
  
  exit_code <- suppressWarnings(
    system2(
      command = "robocopy",
      args = robocopy_args,
      stdout = "",
      stderr = ""
    )
  )
  
  end_time <- Sys.time()
  
  duration <- difftime(end_time, start_time, units = "mins")
  
  exit_message <- interpret_robocopy_exit_code(exit_code)
  
  cat("\nRobocopy finished.\n")
  cat("Duration: ", round(as.numeric(duration), 2), " minutes\n", sep = "")
  cat("Exit code: ", exit_code, "\n", sep = "")
  cat("Status: ", exit_message, "\n", sep = "")
  cat("Log file: ", log_file, "\n", sep = "")
  
  if (exit_code >= 8) {
    warning("Robocopy reported a critical failure. Check the log file: ", log_file)
  }
  
  invisible(
    list(
      source_dir = source_dir,
      destination_dir = destination_dir,
      log_file = log_file,
      lock_file = lock_file,
      mt_threads = mt_threads,
      dry_run = dry_run,
      exit_code = exit_code,
      exit_message = exit_message,
      start_time = start_time,
      end_time = end_time,
      duration_minutes = as.numeric(duration)
    )
  )
}
