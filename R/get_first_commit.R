#' Find the first GitHub commit introducing a function
#'
#' Finds the earliest commit in which a function appears in a file, using the
#' GitHub API. This is a GitHub-based approximation of:
#'
#' ```
#' git log -S <function>
#' ```
#'
#' and does **not** require a local clone.
#'
#' ## Method
#'
#' - If `path` is not supplied, GitHub code search is used to select a candidate
#'   file containing `function_name` (currently restricted to R files).
#' - The commit history for that file path is retrieved.
#' - File contents at each commit are searched for `pattern`.
#' - The earliest matching commit for that file path is returned.
#'
#' ## Limitations
#'
#' GitHub does not expose content-based history search or rename tracking.
#' Results may be incomplete if the function moved between files, the file was
#' renamed or deleted, or the wrong file is selected when `path = NULL`.
#'
#' For exact historical results, use a local clone and `git log -S`.
#'
#' @param owner GitHub repository owner (e.g. `"tidyverse"`).
#' @param repo GitHub repository name (e.g. `"ggplot2"`).
#' @param function_name Name of the function to search for.
#' @param path Optional file path within the repository. Supplying this avoids
#'   code search and is recommended when renames are suspected.
#' @param pattern Optional regular expression used to match file contents.
#'   Defaults to matching R-style function definitions.
#' @param branch Currently ignored (reserved for future use).
#' @param max_commits Maximum number of commits to inspect before giving up.
#'
#' @return
#' A one-row data frame with commit metadata (`commit`, `date`, `author`,
#' `message`, `url`, `file`), or `NULL` if no match is found.
#'
#' @examples
#' \dontrun{
#' get_first_commit("hojsgaard", "doBy", "scale_by")
#'
#' get_first_commit(
#'   "hojsgaard", "doBy", "scale_by",
#'   path = "R/by_scaleBy.R"
#' )
#' }
#'
#' @export

get_first_commit <- function(owner, repo, function_name,
                             path = NULL,
                             pattern = NULL,
                             branch = NULL,
                             max_commits = Inf) {

  if (is.null(pattern)) {
    pattern <- sprintf(
      "\\b%s\\s*<-\\s*function\\b|\\b%s\\s*=\\s*function\\b",
      function_name, function_name
    )
  }

  repo_spec <- sprintf("%s/%s", owner, repo)

  if (is.null(path)) {
    q <- sprintf('repo:%s language:R "%s"', repo_spec, function_name)
    code_res <- gh::gh("/search/code", q = q, per_page = 10)

    if (is.null(code_res$total_count) || code_res$total_count < 1) {
      return(NULL)
    }

    paths <- vapply(code_res$items, `[[`, character(1), "path")
    paths <- paths[order(!grepl("^R/", paths), paths)]
    path <- paths[[1]]
  }

  get_file_text_at_sha <- function(sha) {
    x <- tryCatch(
      gh::gh(
        "/repos/{owner}/{repo}/contents/{path}",
        owner = owner, repo = repo, path = path, ref = sha
      ),
      error = function(e) NULL
    )

    if (is.null(x$content) || x$encoding != "base64") {
      return(NA_character_)
    }

    raw <- base64enc::base64decode(gsub("\\s+", "", x$content))
    rawToChar(raw)
  }

  page <- 1L
  checked <- 0L

  repeat {
    batch <- gh::gh(
      "/repos/{owner}/{repo}/commits",
      owner = owner, repo = repo,
      path = path,
      per_page = 100,
      page = page
    )

    if (length(batch) == 0) break

    for (cmt in rev(batch)) {
      checked <- checked + 1L
      if (checked > max_commits) return(NULL)

      txt <- get_file_text_at_sha(cmt$sha)
      if (!is.na(txt) && grepl(pattern, txt, perl = TRUE)) {
        sha <- cmt$sha
        return(data.frame(
          commit  = sha,
          date    = cmt$commit$author$date,
          author  = cmt$commit$author$name,
          message = cmt$commit$message,
          url     = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, sha),
          file    = path,
          stringsAsFactors = FALSE
        ))
      }
    }

    page <- page + 1L
  }

  NULL
}

