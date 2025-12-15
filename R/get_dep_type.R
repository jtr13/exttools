#' Determine ggplot2 dependency for a CRAN package
#'
#' @param pkg Package name
#' @param cran_db Optional CRAN package database
#' @return One of "depends", "imports", "suggests", or NA
#' @export
which_dep_cran <- function(pkg, cran_db = NULL) {

  if (is.null(cran_db)) {
    cran_db <- tools::CRAN_package_db()
  }

  row <- cran_db[cran_db$Package == pkg, ][1, ]
  if (nrow(row) == 0) {
    return(NA_character_)
  }

  if (!is.na(row$Depends) && grepl("\\bggplot2\\b", row$Depends)) {
    "depends"
  } else if (!is.na(row$Imports) && grepl("\\bggplot2\\b", row$Imports)) {
    "imports"
  } else if (!is.na(row$Suggests) && grepl("\\bggplot2\\b", row$Suggests)) {
    "suggests"
  } else {
    NA_character_
  }
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
