#' Classify ggplot2 dependency type for CRAN packages
#'
#' @param pkgs Character vector of package names.
#' @param cran_db A data frame from tools::CRAN_package_db().
#'
#' @return Character vector with values "depends", "imports", "suggests", or NA.
#' @export
which_dep_cran <- function(pkgs, cran_db) {
  idx <- match(pkgs, cran_db$Package)

  out <- rep(NA_character_, length(pkgs))
  ok <- !is.na(idx)

  d <- cran_db$Depends[idx[ok]]
  i <- cran_db$Imports[idx[ok]]
  s <- cran_db$Suggests[idx[ok]]

  out[ok & !is.na(d) & grepl("\\bggplot2\\b", d)] <- "depends"
  out[ok & is.na(out) & !is.na(i) & grepl("\\bggplot2\\b", i)] <- "imports"
  out[ok & is.na(out) & !is.na(s) & grepl("\\bggplot2\\b", s)] <- "suggests"

  out
}



#' Determine ggplot2 dependency for a GitHub package
#'
#' @param repo_url GitHub repository URL
#' @return One of "depends", "imports", "suggests", or NA
#' @export
which_dep_github <- function(repo_url) {

  repo_url <- sub("/tree/[^/]+(/.*)?$", "", repo_url)

  base <- sub(
    "^https://github\\.com/([^/]+)/([^/]+)$",
    "https://raw.githubusercontent.com/\\1/\\2/HEAD",
    repo_url
  )

  dcf <- NULL
  for (path in c("DESCRIPTION", "pkg/DESCRIPTION")) {
    dcf <- tryCatch(
      suppressWarnings(read.dcf(url(paste0(base, "/", path)))),
      error = function(e) NULL
    )
    if (!is.null(dcf)) break
  }

  if (is.null(dcf)) return(NA_character_)

  for (f in c("Depends", "Imports", "Suggests")) {
    if (f %in% colnames(dcf) &&
        grepl("\\bggplot2\\b", dcf[1, f])) {
      return(tolower(f))
    }
  }

  NA_character_
}
