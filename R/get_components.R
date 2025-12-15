#' Extract ggplot2 grammar components from a CRAN package
#'
#' @param pkg_name Package name
#' @param cran_db Optional CRAN package database
#' @return A data.table with package, component, and function names
#' @export
get_components_cran <- function(pkg_name, cran_db = NULL) {

  if (is.null(cran_db)) {
    cran_db <- tools::CRAN_package_db()
  }

  pkg_results <- data.table::data.table(
    package   = character(),
    component = character(),
    fname     = character()
  )

  idx <- match(pkg_name, cran_db$Package)
  if (is.na(idx)) {
    message("Error: Package ", pkg_name, " not found on CRAN.")
    return(pkg_results)
  }

  version <- cran_db$Version[idx]
  tarball <- paste0(pkg_name, "_", version, ".tar.gz")
  url <- paste0("https://cran.r-project.org/src/contrib/", tarball)

  temp_dir <- file.path(tempdir(), paste0("pkg_", pkg_name, "_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  dest <- file.path(temp_dir, tarball)

  ok <- tryCatch(
    utils::download.file(url, dest, quiet = TRUE) == 0,
    error = function(e) FALSE
  )

  if (!ok) {
    message("Failed to download or timed out: ", pkg_name)
    return(pkg_results)
  }

  utils::untar(dest, exdir = temp_dir)
  src_dir <- file.path(temp_dir, pkg_name)

  analyze_pkg_source(pkg_name, src_dir)
}


#' Extract ggplot2 grammar components from a GitHub repository
#'
#' @param pkg_name Package name
#' @param repo_url GitHub repository URL (optionally including /tree/branch/path)
#' @return A data.table with package, component, and function names
#' @export
get_components_github <- function(pkg_name, repo_url) {

  temp_dir <- file.path(tempdir(), paste0("pkg_", pkg_name, "_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE)
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
    return(data.table::data.table())
  }

  utils::untar(dest, exdir = temp_dir)
  repo_root <- list.dirs(temp_dir, recursive = FALSE)[1]

  pkg_dir <- if (!is.na(subdir)) file.path(repo_root, subdir) else repo_root
  if (!file.exists(pkg_dir)) {
    return(data.table::data.table())
  }

  analyze_pkg_source(pkg_name, pkg_dir)
}


# ---- internal helper (not exported) ----

analyze_pkg_source <- function(pkg_name, pkg_source_dir) {

  pkg_results <- data.table::data.table(
    package   = character(),
    component = character(),
    fname     = character()
  )

  ns_path <- file.path(pkg_source_dir, "NAMESPACE")
  if (!file.exists(ns_path)) return(pkg_results)

  lines <- readLines(ns_path, warn = FALSE)
  exported <- unique(
    sub("^export\\(([^)]+)\\)$", "\\1",
        lines[grepl("^export\\(", lines)])
  )

  if (!length(exported)) return(pkg_results)

  patterns <- c(
    geom       = "^geom_",
    stat       = "^stat_",
    scale      = "^scale_",
    coordinate = "^coord_",
    facet      = "^facet_",
    theme      = "^theme_"
  )

  rows <- vector("list", length(patterns))
  names(rows) <- names(patterns)

  for (nm in names(patterns)) {
    m <- exported[grepl(patterns[[nm]], exported)]
    if (length(m)) {
      rows[[nm]] <- data.table::data.table(
        package   = pkg_name,
        component = nm,
        fname     = m
      )
    }
  }

  data.table::rbindlist(rows, use.names = TRUE)
}
