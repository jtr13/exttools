#' Find the first CRAN release exporting a symbol
#'
#' This function identifies the earliest version of a package on CRAN that
#' **explicitly** exports a given symbol in its `NAMESPACE` via `export()`.
#' It intentionally ignores `exportPattern()` to avoid false positives.
#'
#' Re-exports are detected by checking for a corresponding `importFrom()`
#' directive for the same symbol.
#'
#' Results are cached per package and per version to avoid repeatedly
#' downloading and parsing CRAN tarballs.
#'
#' @param package Character. The name of the CRAN package (e.g. `"ggplot2"`).
#' @param fname Character. The symbol to search for (e.g. `"geom_point"`).
#' @param date_only Logical. If `TRUE`, return only the publication `Date`.
#'   Defaults to `FALSE`.
#' @param repos Character. The CRAN mirror URL. Defaults to
#'   `"https://cloud.r-project.org"`.
#' @param cache_dir Character. Directory used to cache parsed CRAN metadata.
#'   Defaults to `getOption("ggext.cran_cache")`, falling back to a temporary
#'   directory.
#'
#' @return
#' If `date_only = TRUE`, a `Date` object or `NA`.
#'
#' Otherwise, a one-row `data.frame` with columns:
#' - `package`: package name
#' - `fname`: function name
#' - `version`: CRAN version where the symbol is first exported
#' - `first_cran`: CRAN publication date (may be `NA` for older releases)
#' - `is_reexport`: logical indicating whether the symbol is imported via
#'   `importFrom()` (i.e., a re-export)
#'
#' Returns `NULL` if the symbol is never exported.
#'
#' @export
get_first_export_cran <- function(package, fname,
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
  parsed_cache <- file.path(pkg_cache, "parsed")
  if (!dir.exists(parsed_cache)) dir.create(parsed_cache, recursive = TRUE)

  # ---- version index ----
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

  # ---- parse one version ----
  parse_version <- function(url, version) {
    cache_path <- file.path(parsed_cache, paste0(version, ".rds"))

    # If cache exists, try reading it; if malformed, delete and rebuild.
    if (file.exists(cache_path)) {
      cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)

      # Backward-compat: accept old caches that used `cran_first`
      if (is.list(cached) && is.null(cached$date) && !is.null(cached$cran_first)) {
        cached$date <- cached$cran_first
      }

      ok <- is.list(cached) &&
        all(c("version", "date", "ns") %in% names(cached)) &&
        length(cached$version) == 1 &&
        (inherits(cached$date, "Date") || (length(cached$date) == 1 && is.na(cached$date))) &&
        is.character(cached$ns)

      if (ok) return(cached)
      unlink(cache_path, force = TRUE)
    }

    tmp_tar <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(tmp_tar), add = TRUE)

    ok <- tryCatch(
      utils::download.file(url, tmp_tar, quiet = TRUE, mode = "wb"),
      error = function(e) 1
    )
    if (ok != 0) return(NULL)

    members <- tryCatch(utils::untar(tmp_tar, list = TRUE), error = function(e) character())
    if (!length(members)) return(NULL)

    targets <- members[
      grepl(sprintf("^(\\./)?%s/(NAMESPACE|DESCRIPTION)$", package), members)
    ]
    if (!length(targets)) return(NULL)

    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    # Extract only NAMESPACE + DESCRIPTION if present
    tryCatch(
      utils::untar(tmp_tar, files = targets, exdir = tmp_dir),
      error = function(e) return(NULL)
    )

    ns_files <- list.files(tmp_dir, "NAMESPACE", recursive = TRUE, full.names = TRUE)
    desc_files <- list.files(tmp_dir, "DESCRIPTION", recursive = TRUE, full.names = TRUE)
    if (!length(ns_files) || !length(desc_files)) return(NULL)

    ns <- readLines(ns_files[1], warn = FALSE)
    desc <- tryCatch(read.dcf(desc_files[1]), error = function(e) NULL)
    if (is.null(desc)) return(NULL)

    # Robust date extraction: Date/Publication -> Date -> NA
    date_str <- if ("Date/Publication" %in% colnames(desc)) {
      desc[1, "Date/Publication"]
    } else if ("Date" %in% colnames(desc)) {
      desc[1, "Date"]
    } else {
      NA_character_
    }
    date_str <- unname(as.character(date_str))

    res <- list(
      version = version,
      date = suppressWarnings(as.Date(date_str)),
      ns = ns
    )
    saveRDS(res, cache_path)
    res
  }

  # ---- main loop ----
  versions <- get_version_index()
  if (is.null(versions)) return(if (date_only) as.Date(NA) else NULL)

  for (i in seq_len(nrow(versions))) {
    info <- parse_version(versions$url[i], versions$version[i])
    if (is.null(info)) next

    exported <- any(grepl(
      sprintf("^\\s*export\\s*\\(\\s*[^)]*\\b%s\\b", fname),
      info$ns,
      perl = TRUE
    ))

    if (exported) {
      if (date_only) return(info$date)

      is_reexport <- any(grepl(
        sprintf("^\\s*importFrom\\s*\\([^,]+,\\s*[^)]*\\b%s\\b", fname),
        info$ns,
        perl = TRUE
      ))

      return(data.frame(
        package = package,
        fname = fname,
        version = info$version,
        first_cran = info$date,
        is_reexport = is_reexport
      ))
    }
  }

  if (date_only) as.Date(NA) else NULL
}


