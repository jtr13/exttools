#' Find the first (root) commit in a GitHub repository
#'
#' Clones (or refreshes) a GitHub repository into a shared local cache and
#' determines the earliest (root) commit using `git` history, without relying
#' on the GitHub REST API.
#'
#' This function uses the same on-disk Git repository cache as related helpers:
#'
#' - `get_first_commit()`
#' - `get_first_export()`
#' - `get_first_call_github()`
#'
#' The cache location is controlled by the `ggext.git_cache` option. Repositories
#' cloned or refreshed by any of these functions are reused across calls, avoiding
#' redundant network access.
#'
#' @param owner Character scalar. GitHub username or organization.
#' @param repo Character scalar. GitHub repository name.
#' @param branch Optional character scalar. Git ref to use (e.g. `"main"`).
#'   If `NULL` or empty, the function uses `origin/HEAD` when available,
#'   otherwise `HEAD`.
#' @param date_only Logical. If `TRUE`, return only the `Date` of the first commit.
#'   Defaults to `FALSE`.
#' @param cache_dir Directory used to cache cloned repositories. Defaults to
#'   `getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))`.
#'
#' @return
#' If a root commit is found:
#'
#' - When `date_only = TRUE`, a `Date`.
#' - Otherwise, a one-row `data.frame` with columns:
#'   `first_repo`, `author`, `message`, and `url`.
#'
#' If the repository cannot be accessed or inspected:
#'
#' - Returns `NA` when `date_only = TRUE`
#' - Returns `NULL` otherwise
#'
#' @examples
#' \dontrun{
#' get_first_repo_commit("YuLab-SMU", "ggtree")
#' get_first_repo_commit("tidyverse", "ggplot2", date_only = TRUE)
#'
#' options(ggext.git_cache = "~/Library/Caches/ggext_git")
#' }
#'
#' @seealso
#' `get_first_commit()`, `get_first_export()`, `get_first_call_github()`
#'
#' @export
get_first_repo_commit <- function(owner, repo,
                                  branch = NULL,
                                  date_only = FALSE,
                                  cache_dir = getOption(
                                    "ggext.git_cache",
                                    file.path(tempdir(), "gh_repo_cache")
                                  )) {

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required.")
  }

  fail <- function(reason, details = NULL) {
    message(
      paste0(
        "get_first_repo_commit(): ", reason,
        " [", owner, "/", repo, "]",
        if (!is.null(details) && nzchar(details)) paste0("\n", details) else ""
      )
    )
    if (isTRUE(date_only)) as.Date(NA) else NULL
  }

  if (any(is.na(c(owner, repo))) || !nzchar(owner) || !nzchar(repo)) {
    return(fail("invalid inputs (owner/repo missing or empty)"))
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  repo_dir <- file.path(cache_dir, paste0(owner, "__", repo))
  remote   <- sprintf("https://github.com/%s/%s.git", owner, repo)

  run_git <- function(args) {
    processx::run(
      "git",
      args = args,
      echo_cmd = FALSE,
      echo = FALSE,
      error_on_status = FALSE,
      env = c("GIT_TERMINAL_PROMPT" = "0")
    )
  }

  # Clone or refresh (reclone on fetch failure)
  if (!dir.exists(file.path(repo_dir, ".git"))) {
    if (dir.exists(repo_dir)) unlink(repo_dir, recursive = TRUE, force = TRUE)
    dir.create(repo_dir, recursive = TRUE)
    res <- run_git(c("clone", remote, repo_dir))
    if (res$status != 0L) {
      return(fail(
        "repository not found or inaccessible (clone failed)",
        paste(c(res$stdout, res$stderr), collapse = "\n")
      ))
    }
  } else {
    res <- run_git(c("-C", repo_dir, "fetch", "--all", "--prune"))
    if (res$status != 0L) {
      unlink(repo_dir, recursive = TRUE, force = TRUE)
      dir.create(repo_dir, recursive = TRUE)
      res2 <- run_git(c("clone", remote, repo_dir))
      if (res2$status != 0L) {
        return(fail(
          "failed to fetch cached repo and reclone failed",
          paste(c(res$stdout, res$stderr, res2$stdout, res2$stderr), collapse = "\n")
        ))
      }
    }
  }

  # Choose ref
  ref <- branch
  if (is.null(ref) || !nzchar(ref)) {
    res <- run_git(c("-C", repo_dir, "symbolic-ref", "--quiet", "refs/remotes/origin/HEAD"))
    if (res$status == 0L && nzchar(res$stdout)) {
      ref <- sub("^refs/remotes/origin/", "", trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]][1]))
    } else {
      ref <- "HEAD"
    }
  }

  # Checkout quietly
  res <- run_git(c("-C", repo_dir, "checkout", "-q", ref))
  if (res$status != 0L) {
    return(fail(
      sprintf("failed to checkout ref '%s'", ref),
      paste(c(res$stdout, res$stderr), collapse = "\n")
    ))
  }

  # Root commit
  res <- run_git(c("-C", repo_dir, "rev-list", "--max-parents=0", "HEAD"))
  if (res$status != 0L || !nzchar(res$stdout)) {
    return(fail(
      "git rev-list failed to find root commit",
      paste(c(res$stdout, res$stderr), collapse = "\n")
    ))
  }
  root <- trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]][1])

  # Root commit metadata
  fmt <- "%ad%x09%an%x09%s"
  res <- run_git(c("-C", repo_dir, "show", "-s", "--date=short", paste0("--format=", fmt), root))
  if (res$status != 0L || !nzchar(res$stdout)) {
    return(fail(
      "git show failed for root commit",
      paste(c(res$stdout, res$stderr), collapse = "\n")
    ))
  }

  parts <- strsplit(trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]][1]), "\t", fixed = TRUE)[[1]]
  if (length(parts) < 3) {
    return(fail("could not parse git show output for root commit", res$stdout))
  }

  dt  <- suppressWarnings(as.Date(parts[1]))
  au  <- parts[2]
  msg <- parts[3]

  if (isTRUE(date_only)) return(dt)

  data.frame(
    first_repo = dt,
    author     = au,
    message    = msg,
    url        = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, root),
    stringsAsFactors = FALSE
  )
}
