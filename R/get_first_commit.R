#' Find the first GitHub commit introducing a symbol
#'
#' Searches a GitHub repository for the earliest commit in which a given
#' symbol (typically a function name) is *assigned* in an R source file.
#' The search is performed via the GitHub API and does not require a local clone.
#'
#' If `path` is not supplied, GitHub code search is used to identify candidate
#' `.R` files, and the function selects the first file whose current contents
#' actually assign the symbol before scanning its commit history.
#'
#' @param owner GitHub account name.
#' @param repo GitHub repository name.
#' @param function_name Name of the symbol to search for.
#' @param date_only Logical; if `TRUE` (default), return only the commit date.
#'   If `FALSE`, return a one-row data frame with commit metadata.
#' @param path Optional path to an R source file to search. If `NULL`,
#'   a suitable file is inferred automatically.
#' @param pattern Optional regular expression used to detect the symbol
#'   assignment. Defaults to matching `<name> <-` or `<name> =`.
#' @param branch Optional branch or ref to search. Defaults to the repository’s
#'   default branch.
#' @param max_commits Maximum number of commits to inspect before giving up.
#'
#' @return If `date_only = TRUE`, a character string giving the commit date
#'   (ISO 8601), or `NULL` if no match is found. If `date_only = FALSE`,
#'   a one-row data frame with commit metadata, or `NULL` if no match is found.
#'
#' @export
get_first_commit <- function(owner, repo, function_name,
                             date_only = TRUE,
                             path = NULL,
                             pattern = NULL,
                             branch = NULL,
                             max_commits = Inf) {

  if (is.null(pattern)) {
    pattern <- sprintf("\\b%s\\b\\s*(<-|=)", function_name)
  }

  get_file_text_at_ref <- function(path, ref) {
    x <- tryCatch(
      gh::gh(
        "/repos/{owner}/{repo}/contents/{path}",
        owner = owner, repo = repo, path = path, ref = ref
      ),
      error = function(e) NULL
    )

    if (is.null(x$content) || is.null(x$encoding) || x$encoding != "base64") {
      return(NA_character_)
    }

    raw <- base64enc::base64decode(gsub("\\s+", "", x$content))
    rawToChar(raw)
  }

  # Infer path if needed
  if (is.null(path)) {
    q <- sprintf(
      'repo:%s/%s language:R extension:R "%s" -path:man -path:vignettes -path:inst/doc',
      owner, repo, function_name
    )

    code_res <- gh::gh("/search/code", q = q, per_page = 50)
    if (is.null(code_res$total_count) || code_res$total_count < 1) {
      return(NULL)
    }

    paths <- unique(vapply(code_res$items, `[[`, character(1), "path"))
    paths <- paths[order(!grepl("^R/", paths), paths)]

    head_ref <- branch
    if (is.null(head_ref) || is.na(head_ref) || !nzchar(head_ref)) {
      repo_meta <- gh::gh("/repos/{owner}/{repo}", owner = owner, repo = repo)
      head_ref <- repo_meta$default_branch
    }

    picked <- NA_character_
    for (p in paths) {
      txt <- get_file_text_at_ref(p, head_ref)
      if (!is.na(txt) && grepl(pattern, txt, perl = TRUE)) {
        picked <- p
        break
      }
    }

    if (is.na(picked)) {
      return(NULL)
    }

    path <- picked
  }

  page <- 1L
  checked <- 0L

  repeat {
    args <- list(
      owner = owner, repo = repo,
      path = path,
      per_page = 100,
      page = page
    )
    if (!is.null(branch) && !is.na(branch) && nzchar(branch)) {
      args$sha <- branch
    }

    batch <- do.call(gh::gh, c(list("/repos/{owner}/{repo}/commits"), args))
    if (length(batch) == 0) break

    for (cmt in rev(batch)) {
      checked <- checked + 1L
      if (checked > max_commits) return(NULL)

      txt <- get_file_text_at_ref(path, cmt$sha)
      if (!is.na(txt) && grepl(pattern, txt, perl = TRUE)) {
        if (date_only) {
          return(as.Date(cmt$commit$author$date))
        } else {
          sha <- cmt$sha
          return(data.frame(
            commit  = sha,
            date    = as.Date(cmt$commit$author$date),
            author  = cmt$commit$author$name,
            message = cmt$commit$message,
            url     = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, sha),
            file    = path,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    page <- page + 1L
  }

  NULL
}
