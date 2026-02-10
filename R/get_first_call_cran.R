#' Find the first CRAN release containing a call to a function in an R file
#'
#' Scans a package's CRAN source tarballs (current + archive) in chronological
#' order and returns the earliest release where a call to `fname()` appears
#' in any `R/` source file.
#'
#' This function shares the same cache root and version index as
#' `get_first_export_cran()`. Once a version has been scanned, its result
#' is cached permanently and never refreshed.
#'
#' @param package Character. The name of the CRAN package.
#' @param fname Character. Function name to search for (e.g. "ggplot").
#' @param date_only Logical. If TRUE, return only the CRAN publication Date.
#' @param repos Character. CRAN mirror URL.
#' @param cache_dir Character. Cache root directory (shared with
#'   `get_first_export_cran()`).
#'
#' @return
#' If `date_only = TRUE`, a Date or NA.
#'
#' Otherwise, a one-row data.frame with:
#' package, fname, version, first_call, file
#'
#' @export
get_first_call_cran <- function(package, fname,
                                date_only = FALSE,
                                repos = "https://cloud.r-project.org",
                                cache_dir = getOption(
                                  "ggext.cran_cache",
                                  file.path(tempdir(), "cran_export_cache")
                                )) {

  message("Processing: ", package)

  if (any(is.na(c(package, fname))) || !nzchar(package) || !nzchar(fname)) {
    return(if (date_only) as.Date(NA) else NULL)
  }

  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  pkg_cache <- file.path(cache_dir, package)
  if (!dir.exists(pkg_cache)) dir.create(pkg_cache, recursive = TRUE)

  # ---- shared version index (cache-first, no refresh) ----
  get_version_index <- function() {
    index_path <- file.path(pkg_cache, "index.rds")
    if (file.exists(index_path)) return(readRDS(index_path))

    arch_url <- sprintf("%s/src/contrib/Archive/%s/", repos, package)

    arch_files <- character()
    arch_lines <- suppressWarnings(
      tryCatch(readLines(arch_url, warn = FALSE), error = function(e) NULL)
    )
    if (!is.null(arch_lines)) {
      m <- regmatches(
        arch_lines,
        gregexpr(sprintf("%s_.*?\\.tar\\.gz", package), arch_lines)
      )
      arch_files <- unique(unlist(m))
    }

    ap <- tryCatch(
      utils::available.packages(repos = repos, type = "source"),
      error = function(e) NULL
    )

    curr_file <- if (!is.null(ap) && package %in% rownames(ap)) {
      sprintf("%s_%s.tar.gz", package, ap[package, "Version"])
    } else character()

    all_files <- unique(c(arch_files, curr_file))
    if (!length(all_files)) return(NULL)

    vers <- sub(paste0("^", package, "_"), "", sub("\\.tar\\.gz$", "", all_files))

    df <- data.frame(
      version = vers,
      url = ifelse(
        all_files %in% arch_files,
        paste0(arch_url, all_files),
        sprintf("%s/src/contrib/%s", repos, all_files)
      ),
      stringsAsFactors = FALSE
    )

    df <- df[order(numeric_version(df$version)), ]
    saveRDS(df, index_path)
    df
  }

  # ---- per-version call cache ----
  calls_cache <- file.path(pkg_cache, "calls", fname)
  if (!dir.exists(calls_cache)) dir.create(calls_cache, recursive = TRUE)

  scan_version <- function(url, version) {
    cache_path <- file.path(calls_cache, paste0(version, ".rds"))

    # STRICT cache-first: never rescan if cache exists
    if (file.exists(cache_path)) {
      return(readRDS(cache_path))
    }

    tmp_tar <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(tmp_tar), add = TRUE)

    ok <- tryCatch(
      utils::download.file(url, tmp_tar, quiet = TRUE, mode = "wb"),
      error = function(e) 1
    )
    if (ok != 0) {
      saveRDS(NULL, cache_path)
      return(NULL)
    }

    members <- tryCatch(utils::untar(tmp_tar, list = TRUE), error = function(e) character())
    r_members <- members[grepl(sprintf("^(\\./)?%s/R/.*\\.[Rr]$", package), members)]
    if (!length(r_members)) {
      saveRDS(NULL, cache_path)
      return(NULL)
    }

    rx <- paste0("\\b(?:[[:alnum:]_.]+::)?", fname, "\\s*\\(")

    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    utils::untar(tmp_tar, files = r_members, exdir = tmp_dir)

    for (m in r_members) {
      f <- file.path(tmp_dir, m)
      if (!file.exists(f)) next
      txt <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
      if (!is.null(txt) && any(grepl(rx, txt, perl = TRUE))) {
        saveRDS(m, cache_path)
        return(m)
      }
    }

    saveRDS(NULL, cache_path)
    NULL
  }

  # ---- publication date (shared parsed cache) ----
  parsed_cache <- file.path(pkg_cache, "parsed")
  if (!dir.exists(parsed_cache)) dir.create(parsed_cache, recursive = TRUE)

  get_version_date <- function(url, version) {
    cache_path <- file.path(parsed_cache, paste0(version, ".rds"))
    if (file.exists(cache_path)) {
      cached <- readRDS(cache_path)
      if (!is.null(cached$date)) return(cached$date)
    }

    tmp_tar <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(tmp_tar), add = TRUE)

    utils::download.file(url, tmp_tar, quiet = TRUE, mode = "wb")

    members <- utils::untar(tmp_tar, list = TRUE)
    target <- members[grepl(sprintf("^(\\./)?%s/DESCRIPTION$", package), members)]
    if (!length(target)) return(as.Date(NA))

    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    utils::untar(tmp_tar, files = target, exdir = tmp_dir)

    desc <- read.dcf(list.files(tmp_dir, "DESCRIPTION", recursive = TRUE, full.names = TRUE)[1])
    date_str <- if ("Date/Publication" %in% colnames(desc)) {
      desc[1, "Date/Publication"]
    } else if ("Date" %in% colnames(desc)) {
      desc[1, "Date"]
    } else NA

    as.Date(date_str)
  }

  # ---- main loop ----
  versions <- get_version_index()
  if (is.null(versions)) return(if (date_only) as.Date(NA) else NULL)

  for (i in seq_len(nrow(versions))) {
    version <- versions$version[i]
    url <- versions$url[i]

    file_hit <- scan_version(url, version)
    if (is.null(file_hit)) next

    date <- get_version_date(url, version)

    if (date_only) return(date)

    return(data.frame(
      package = package,
      fname = fname,
      version = version,
      first_call = date,
      file = file_hit
    ))
  }

  if (date_only) as.Date(NA) else NULL
}
