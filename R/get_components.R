#' Extract ggplot2 grammar components from a CRAN package
#'
#' @param pkg_name Package name
#' @param cran_db Optional CRAN package database
#' @return A data.frame with package, component, and function names
#' @export
get_components_cran <- function(pkg_name, cran_db = NULL) {

  empty <- data.frame(
    package   = character(),
    component = character(),
    fname     = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(cran_db)) {
    cran_db <- tools::CRAN_package_db()
  }

  idx <- match(pkg_name, cran_db$Package)
  if (is.na(idx)) {
    message("Error: Package ", pkg_name, " not found on CRAN.")
    return(empty)
  }

  version <- cran_db$Version[idx]
  tarball <- paste0(pkg_name, "_", version, ".tar.gz")
  url <- paste0("https://cran.r-project.org/src/contrib/", tarball)

  temp_dir <- file.path(tempdir(), paste0("pkg_", pkg_name, "_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  dest <- file.path(temp_dir, tarball)

  ok <- tryCatch(
    utils::download.file(url, dest, quiet = TRUE) == 0,
    error = function(e) FALSE
  )

  if (!ok) {
    message("Failed to download or timed out: ", pkg_name)
    return(empty)
  }

  utils::untar(dest, exdir = temp_dir)
  src_dir <- file.path(temp_dir, pkg_name)

  analyze_pkg_source(pkg_name, src_dir)
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
    coordinate = "^coord_",
    facet      = "^facet_",
    theme      = "^theme_"
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
