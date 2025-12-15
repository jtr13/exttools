#' Classify CRAN package as in-grammar or other
#'
#' @param package Package name
#' @param cran_db Optional CRAN package database
#' @return "in grammar" or "other"
#' @export
in_grammar_check_cran <- function(package, cran_db = NULL) {

  if (nrow(get_components_cran(package, cran_db = cran_db)) > 0) {
    "in grammar"
  } else {
    "other"
  }
}


#' Classify GitHub package as in-grammar or other
#'
#' @param package Package name
#' @param url GitHub repository URL
#' @return "in grammar", "other", or NA
#' @export
in_grammar_check_github <- function(package, url) {

  if (!grepl("^https://github\\.com/[^/]+/[^/]+(/tree/[^/]+/.+)?$", url)) {
    return(NA_character_)
  }

  n <- tryCatch(
    nrow(get_components_github(package, url)),
    error = function(e) NA_integer_
  )

  if (is.na(n)) return(NA_character_)
  if (n > 0) "in grammar" else "other"
}
