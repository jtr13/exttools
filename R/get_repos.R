#' Convert GitHub Pages URLs to GitHub repository URLs
#'
#' Transforms GitHub Pages URLs of the form
#' `https://<user>.github.io/<repo>/` into the corresponding
#' GitHub repository URLs:
#' `https://github.com/<user>/<repo>`.
#'
#' Non–GitHub Pages URLs are returned unchanged.
#'
#' @param url A character vector of URLs. Each element may be a GitHub
#'   Pages URL or any other URL.
#'
#' @return A character vector of URLs of the same length as `url`. GitHub
#'   Pages URLs are converted to GitHub repository URLs; all other URLs
#'   are returned as-is.
#'
#' @details
#' This function detects GitHub Pages URLs using the pattern
#' `https://<user>.github.io/<repo>/`. Only user-level Pages URLs that
#' include a repository path are converted. Root Pages URLs such as
#' `https://<user>.github.io/` are left unchanged, as they do not map
#' unambiguously to a single repository.
#'
#' @examples
#' github_pages_to_repo("https://hadley.github.io/httr/")
#'
#' github_pages_to_repo(c(
#'   "https://hadley.github.io/httr/",
#'   "https://github.com/hadley/httr",
#'   "https://example.com"
#' ))
#'
#' @export
#'
github_pages_to_repo <- function(url) {

  is_pages <- grepl("^https://[^.]+\\.github\\.io/", url)

  url[is_pages] <- sub(
    "^https://([^.]+)\\.github\\.io/([^/]+)/?$",
    "https://github.com/\\1/\\2",
    url[is_pages]
  )

  url
}

#' Extract GitHub user and repository from text
#'
#' Parses a character string (typically derived from CRAN `URL` and/or
#' `BugReports` fields) and extracts the first GitHub
#' `user/repository` pair it finds.
#'
#' The function looks for URLs of the form:
#' `github.com/<user>/<repo>` (optionally ending in `.git`).
#'
#' If no GitHub repository can be identified, a one-row data frame
#' with `NA` values is returned. The function always returns a
#' data frame, making it safe to use with `purrr::map()` followed by
#' `purrr::list_rbind()`.
#'
#' @param url_text A single character string containing one or more URLs
#'   or free-form text that may include a GitHub repository link.
#'
#' @return A one-row data frame with two character columns:
#'   \describe{
#'     \item{github_user}{GitHub account name, or `NA_character_` if not found}
#'     \item{github_repo}{GitHub repository name, or `NA_character_` if not found}
#'   }
#'
#' @examples
#' extract_github_repo("https://github.com/tidyverse/ggplot2")
#'
#' extract_github_repo("Bug reports at https://github.com/r-lib/vctrs/issues")
#'
#' extract_github_repo(NA_character_)
#'
#' @export

extract_github_repo <- function(url_text) {
  if (is.na(url_text) || !nzchar(url_text)) {
    return(data.frame(github_user = NA_character_, github_repo = NA_character_))
  }

  pattern <- "(?i)github\\.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)(?:\\.git)?"
  m <- regexec(pattern, url_text, perl = TRUE)
  reg <- regmatches(url_text, m)[[1]]

  if (length(reg) < 3L) {
    return(data.frame(github_user = NA_character_, github_repo = NA_character_))
  }

  data.frame(
    github_user = reg[2],
    github_repo = reg[3]
  )
}
