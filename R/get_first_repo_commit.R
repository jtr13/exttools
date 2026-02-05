
#' Find the first commit in a GitHub repository
#'
#' Finds the earliest (root) commit in a GitHub repository using the GitHub
#' REST API. Does not require a local clone.
#'
#' The function follows pagination links to retrieve the oldest commit reachable
#' from the specified ref.
#'
#' @param owner GitHub repository owner.
#' @param repo GitHub repository name.
#' @param sha Optional commit SHA or branch name to start from. Defaults to the
#'   repository’s default branch.
#'
#' @return
#' A one-row data frame with commit SHA, date, author, message, and URL.
#'
#' @examples
#' \dontrun{
#' get_first_repo_commit("tidyverse", "ggplot2")
#' }
#'
#' @export


get_first_repo_commit <- function(owner, repo, sha = NULL) {
  base <- sprintf("https://api.github.com/repos/%s/%s/commits", owner, repo)

  req <- httr2::request(base) |>
    httr2::req_url_query(per_page = 1)

  if (!is.null(sha)) {
    req <- httr2::req_url_query(req, sha = sha)
  }

  r1 <- httr2::req_perform(req)
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

  data.frame(
    date    = as.Date(date),
    author  = cmt$commit$author$name,
    message = cmt$commit$message,
    url     = cmt$html_url,
    stringsAsFactors = FALSE
  )
}


