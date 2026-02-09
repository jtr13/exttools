#' Find the first commit in a GitHub repository
#'
#' Finds the earliest (root) commit in a GitHub repository using the GitHub
#' REST API. The function does not require a local clone and queries GitHub
#' directly via the commits endpoint.
#'
#' Pagination is handled implicitly: the function follows the Link header
#' to retrieve the final page of commits and returns the oldest commit listed.
#'
#' If the repository does not exist, is inaccessible, or the request fails:
#' \itemize{
#'   \item Returns NA when date_only = TRUE
#'   \item Returns NULL otherwise
#' }
#'
#' @param owner Character. GitHub repository owner.
#' @param repo Character. GitHub repository name.
#' @param sha Optional character. Commit SHA or branch name to start from.
#'   Defaults to the repository's default branch.
#' @param date_only Logical. If TRUE, return only the commit date
#'   as a Date scalar.
#'
#' @return
#' When date_only = TRUE, a Date scalar giving the date of the first commit,
#' or NA if the repository is not found.
#'
#' When date_only = FALSE, a one-row data frame with columns:
#' \describe{
#'   \item{first_repo}{Date of the first commit}
#'   \item{author}{Commit author name}
#'   \item{message}{Commit message}
#'   \item{url}{URL of the commit on GitHub}
#' }
#' or NULL if the repository is not found.
#'
#' @examples
#' \dontrun{
#' get_first_repo_commit("tidyverse", "ggplot2")
#' get_first_repo_commit("tidyverse", "ggplot2", date_only = TRUE)
#' }
#'
#' @export
get_first_repo_commit <- function(owner, repo, sha = NULL,
                                  date_only = FALSE) {

  base <- sprintf("https://api.github.com/repos/%s/%s/commits", owner, repo)

  req <- httr2::request(base) |>
    httr2::req_url_query(per_page = 1)

  if (!is.null(sha)) {
    req <- httr2::req_url_query(req, sha = sha)
  }

  r1 <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(r1)) {
    return(if (date_only) NA else NULL)
  }

  link <- httr2::resp_header(r1, "link")

  if (is.null(link) || is.na(link) || !nzchar(link)) {
    cmt <- httr2::resp_body_json(r1)[[1]]
  } else {
    last <- strsplit(link, ",\\s*")[[1]]
    last <- last[grepl('rel="last"', last)][1]
    last_url <- sub("^<([^>]+)>.*$", "\\1", last)

    rlast <- httr2::req_perform(httr2::request(last_url))
    cmt <- httr2::resp_body_json(rlast)[[1]]
  }

  date <- cmt$commit$author$date
  if (is.null(date)) date <- cmt$commit$committer$date

  if (date_only) {
    return(as.Date(date))
  }

  data.frame(
    first_repo = as.Date(date),
    author     = cmt$commit$author$name,
    message    = cmt$commit$message,
    url        = cmt$html_url,
    stringsAsFactors = FALSE
  )
}
