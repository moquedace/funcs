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

gcs_list_only <- function(bucket, prefix, file_exts = character(0), gsutil_path = NULL) {
  detect_bin <- function(hints = character(), fallback = "") {
    cands <- c(hints, Sys.which(basename(hints)), fallback)
    cands <- cands[nzchar(cands)]
    found <- cands[file.exists(cands)]
    if (length(found)) return(found[[1]])
    if (nzchar(Sys.which("gsutil"))) return(Sys.which("gsutil"))
    ""
  }
  normalize_exts <- function(exts) {
    if (!length(exts)) return(character(0))
    exts <- tolower(as.character(exts))
    add_dot <- function(e) if (startsWith(e, ".")) e else paste0(".", e)
    unique(vapply(exts, add_dot, character(1)))
  }
  ends_with_any <- function(x, exts) {
    if (!length(exts)) return(rep(TRUE, length(x)))
    grepl(paste0("(", paste0(gsub("\\.", "\\\\.", tolower(exts)), collapse = "|"), ")$"), tolower(x))
  }
  list_gs <- function(gsutil_path, uri) {
    suppressWarnings(system2(gsutil_path, c("ls", "-r", uri), stdout = TRUE, stderr = ""))
  }
  if (is.null(gsutil_path) || !nzchar(gsutil_path)) {
    gsutil_path <- detect_bin(c(
      "C:\\\\Program Files (x86)\\\\Google\\\\Cloud SDK\\\\google-cloud-sdk\\\\bin\\\\gsutil.cmd",
      "C:\\\\Program Files\\\\Google\\\\Cloud SDK\\\\google-cloud-sdk\\\\bin\\\\gsutil.cmd"
    ))
  }
  if (!nzchar(gsutil_path) || !file.exists(gsutil_path)) stop(sprintf("gsutil not found at: %s", gsutil_path))
  exts <- normalize_exts(file_exts)
  uri  <- sprintf("gs://%s/%s/**", bucket, prefix)
  message(sprintf("listing: %s", uri))
  listed <- list_gs(gsutil_path, uri)
  objs <- listed[grepl(sprintf("^gs://%s/%s", bucket, prefix), listed)]
  objs <- objs[!grepl("/$", objs)]
  matched <- objs[ends_with_any(objs, exts)]
  message(sprintf("found %d file(s)%s", length(matched), if (length(exts)) paste0(" with extensions: ", paste(exts, collapse = ", ")) else ""))
  if (length(matched)) cat(paste(sprintf("%03d - %s", seq_along(matched), matched), collapse = "\n"), "\n")
  invisible(matched)
}

gcs_download <- function(uris, prefix, local_dir,
                         pattern = NULL,
                         overwrite = TRUE, resumable = TRUE,
                         dry_run = FALSE, tries = 3, gsutil_path = NULL) {
  detect_bin <- function(hints = character(), fallback = "") {
    cands <- c(hints, Sys.which(basename(hints)), fallback)
    cands <- cands[nzchar(cands)]
    found <- cands[file.exists(cands)]
    if (length(found)) return(found[[1]])
    if (nzchar(Sys.which("gsutil"))) return(Sys.which("gsutil"))
    ""
  }
  escape_regex <- function(x) gsub("([\\.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", x)
  make_local_path <- function(obj_path, prefix, local_dir) {
    rel <- sub(paste0("^", escape_regex(prefix), "/?"), "", obj_path)
    file.path(local_dir, rel)
  }
  ensure_dir <- function(path) {
    d <- dirname(path)
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  download_one <- function(gsutil_path, src, dst, overwrite = TRUE, resumable = TRUE) {
    ensure_dir(dst)
    flags <- character(0)
    if (!overwrite) flags <- c(flags, "-n")
    if (resumable) flags <- c(flags, "-c")
    status <- suppressWarnings(system2(gsutil_path, c("cp", flags, shQuote(src), shQuote(dst)), stdout = "", stderr = ""))
    is.null(attr(status, "status")) || attr(status, "status") == 0
  }
  retry_download <- function(gsutil_path, src, dst, overwrite, resumable, tries = 3, base_wait = 1) {
    for (i in seq_len(tries)) {
      ok <- download_one(gsutil_path, src, dst, overwrite = overwrite, resumable = resumable)
      if (ok) return(TRUE)
      Sys.sleep(base_wait * 2^(i - 1))
    }
    FALSE
  }
  if (!length(uris)) {
    message("no uris provided.")
    return(invisible(character(0)))
  }
  if (is.null(gsutil_path) || !nzchar(gsutil_path)) {
    gsutil_path <- detect_bin(c(
      "C:\\\\Program Files (x86)\\\\Google\\\\Cloud SDK\\\\google-cloud-sdk\\\\bin\\\\gsutil.cmd",
      "C:\\\\Program Files\\\\Google\\\\Cloud SDK\\\\google-cloud-sdk\\\\bin\\\\gsutil.cmd"
    ))
  }
  if (!nzchar(gsutil_path) || !file.exists(gsutil_path)) stop(sprintf("gsutil not found at: %s", gsutil_path))
  chosen <- uris
  if (!is.null(pattern)) chosen <- chosen[grepl(pattern, basename(chosen))]
  if (!length(chosen)) {
    message("no matches to download after optional filter.")
    return(invisible(character(0)))
  }
  message(if (dry_run) "dry-run enabled. no files will be written." else "starting batch download.")
  out <- character(0)
  for (obj in chosen) {
    obj_rel  <- sub("^gs://[^/]+/", "", obj)
    dst_path <- make_local_path(obj_rel, prefix, local_dir)
    if (dry_run) {
      message(sprintf("[dry-run] %s -> %s", obj, dst_path))
      out <- c(out, dst_path)
    } else {
      ok <- retry_download(gsutil_path, obj, dst_path, overwrite = overwrite, resumable = resumable, tries = tries, base_wait = 1)
      if (ok) {
        message(sprintf("downloaded: %s -> %s", obj, dst_path))
        out <- c(out, dst_path)
      } else {
        warning(sprintf("download failed after retries: %s", obj))
      }
    }
  }
  message("batch finished.")
  invisible(out)
}
