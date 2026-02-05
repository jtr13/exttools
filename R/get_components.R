#' Extract ggplot2-extension components from a CRAN package
#'
#' Downloads a CRAN source tarball for `pkg_name` (unless cached), extracts the
#' package `NAMESPACE`, and calls `analyze_pkg_source()` to identify exported
#' ggplot2 “grammar” components (e.g., functions named like `geom_*`, `stat_*`,
#' `scale_*`, etc., depending on what `analyze_pkg_source()` detects).
#'
#' The function caches the `NAMESPACE` file at `cache_root/<pkg>/<version>/NAMESPACE`
#' and keeps that directory “clean” by removing any other files that might be
#' present.
#'
#' @param pkg_name Character scalar. CRAN package name.
#' @param cran_db Optional CRAN package database as returned by
#'   [tools::CRAN_package_db()]. If `NULL`, it is computed on demand.
#' @param cache_root Character scalar. Root directory used for on-disk caching of
#'   extracted `NAMESPACE` files. Default is `"~/CRAN"`.
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{package}{Package name.}
#'   \item{component}{Component type/category identified by `analyze_pkg_source()`.}
#'   \item{fname}{Function name.}
#' }
#' If the package cannot be found on CRAN, downloaded, or its `NAMESPACE` cannot be
#' extracted, returns an empty data frame with the same columns.
#'
#' @details
#' Network access is required on the first run for a given package version. The
#' cache is keyed by package version from `cran_db`, so updates on CRAN will be
#' cached separately.
#'
#' This function assumes `analyze_pkg_source(pkg_name, cache_dir)` exists and can
#' operate given a directory that contains (at minimum) a `NAMESPACE` file.
#'
#' @examples
#' \dontrun{
#' # Using a precomputed CRAN DB speeds up repeated calls
#' db <- tools::CRAN_package_db()
#' out <- get_components_cran("ggrepel", cran_db = db, cache_root = tempdir())
#' head(out)
#'
#' # Use the default cache under ~/CRAN
#' out2 <- get_components_cran("ggridges")
#' }
#'
#' @seealso [tools::CRAN_package_db()], [utils::download.file()], [utils::untar()]
#' @export
get_components_cran <- function(pkg_name, cran_db = NULL, cache_root = "~/CRAN") {
  cache_root <- path.expand(cache_root)
  message("Processing ", pkg_name)

  empty <- data.frame(
    package   = character(),
    component = character(),
    fname     = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(cran_db)) cran_db <- tools::CRAN_package_db()

  idx <- match(pkg_name, cran_db$Package)
  if (is.na(idx)) {
    message("Error: Package ", pkg_name, " not found on CRAN.")
    return(empty)
  }

  version <- cran_db$Version[idx]
  tarball <- paste0(pkg_name, "_", version, ".tar.gz")
  url <- paste0("https://cran.r-project.org/src/contrib/", tarball)

  cache_dir <- file.path(cache_root, pkg_name, version)
  cache_ns  <- file.path(cache_dir, "NAMESPACE")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # If cached, use it (and also keep cache_dir clean)
  if (file.exists(cache_ns)) {
    junk <- setdiff(list.files(cache_dir, all.files = TRUE, no.. = TRUE), "NAMESPACE")
    if (length(junk)) unlink(file.path(cache_dir, junk), recursive = TRUE, force = TRUE)
    return(analyze_pkg_source(pkg_name, cache_dir))
  }

  td <- tempfile(pattern = paste0("ns_", pkg_name, "_"))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  tf <- file.path(td, "pkg.tar.gz")
  ok <- tryCatch(utils::download.file(url, tf, quiet = TRUE) == 0, error = function(e) FALSE)
  if (!ok) {
    message("Failed to download or timed out: ", pkg_name)
    return(empty)
  }

  ns_in_tar <- file.path(pkg_name, "NAMESPACE")
  ok2 <- tryCatch({ utils::untar(tf, exdir = td, files = ns_in_tar); TRUE }, error = function(e) FALSE)
  src_ns <- file.path(td, ns_in_tar)
  if (!ok2 || !file.exists(src_ns)) {
    message("Failed to extract NAMESPACE: ", pkg_name)
    return(empty)
  }

  file.copy(src_ns, cache_ns, overwrite = TRUE)

  # Hard guarantee: cache_dir contains only NAMESPACE
  junk <- setdiff(list.files(cache_dir, all.files = TRUE, no.. = TRUE), "NAMESPACE")
  if (length(junk)) unlink(file.path(cache_dir, junk), recursive = TRUE, force = TRUE)

  analyze_pkg_source(pkg_name, cache_dir)
}


#' Extract ggplot2 grammar components from a GitHub repository
#'
#' @param pkg_name Package name
#' @param repo_url GitHub repository URL (optionally including /tree/branch/path)
#' @return A data.frame with package, component, and function names
#' @export
get_components_github <- function(pkg_name, repo_url) {

  empty <- data.frame(
    package   = character(),
    component = character(),
    fname     = character(),
    stringsAsFactors = FALSE
  )

  temp_dir <- file.path(tempdir(), paste0("pkg_", pkg_name, "_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  m <- regexec("^https://github\\.com/[^/]+/[^/]+/tree/[^/]+/(.+)$", repo_url)
  subdir <- regmatches(repo_url, m)[[1]][2]

  repo_root_url <- sub("/tree/.*$", "", repo_url)
  repo_tar <- paste0(repo_root_url, "/archive/HEAD.tar.gz")
  dest <- file.path(temp_dir, "repo.tar.gz")

  ok <- tryCatch(
    suppressWarnings(utils::download.file(repo_tar, dest, quiet = TRUE)) == 0,
    error = function(e) FALSE
  )

  if (!ok) {
    message("Failed to download GitHub repo for ", pkg_name)
    return(empty)
  }

  utils::untar(dest, exdir = temp_dir)
  repo_root <- list.dirs(temp_dir, recursive = FALSE, full.names = TRUE)[1]

  pkg_dir <- if (!is.na(subdir)) file.path(repo_root, subdir) else repo_root
  if (!file.exists(pkg_dir)) {
    return(empty)
  }

  analyze_pkg_source(pkg_name, pkg_dir)
}


# ---- internal helper (not exported) ----

analyze_pkg_source <- function(pkg_name, pkg_source_dir) {

  empty <- data.frame(
    package   = character(),
    component = character(),
    fname     = character(),
    stringsAsFactors = FALSE
  )

  ns_path <- file.path(pkg_source_dir, "NAMESPACE")
  if (!file.exists(ns_path)) return(empty)

  lines <- readLines(ns_path, warn = FALSE)
  exported <- unique(
    sub("^export\\(([^)]+)\\)$", "\\1",
        lines[grepl("^export\\(", lines)])
  )
  if (!length(exported)) return(empty)

  patterns <- c(
    geom       = "^geom_",
    stat       = "^stat_",
    scale      = "^scale_",
    coord      = "^coord_",
    facet      = "^facet_",
    theme      = "^theme_",
    position   = "^position_",
    guide      = "^guide_"
  )

  rows <- lapply(names(patterns), function(nm) {
    m <- exported[grepl(patterns[[nm]], exported)]
    if (!length(m)) return(NULL)

    data.frame(
      package   = rep(pkg_name, length(m)),
      component = rep(nm, length(m)),
      fname     = m,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
