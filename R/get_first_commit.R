#' Find the first commit that introduces a function definition in a GitHub repo
#'
#' Clones (or refreshes) a GitHub repository into a local cache directory, checks
#' out a target ref (branch or default), then scans commits (oldest to newest)
#' that *touch* occurrences of `function_name` under `R/`. For each candidate
#' commit, it verifies the function is actually defined (assigned via `<-` or
#' `=`) somewhere in the tree at that commit using `git grep`.
#'
#' The return value is either the commit date (when `date_only = TRUE`) or a
#' one-row `data.frame` with commit metadata and a GitHub URL.
#'
#' This function emits diagnostic `message()` output when it cannot determine an
#' answer (e.g., repository not found/inaccessible, checkout failed, git log/grep
#' errors) or when the symbol is never defined under `R/`.
#'
#' @param owner Character scalar. GitHub username/organization.
#' @param repo Character scalar. GitHub repository name.
#' @param function_name Character scalar. Symbol to search for (e.g. `"geom_foo"`).
#' @param date_only Logical; if `TRUE`, return only the `Date` of the first
#'   verified defining commit. Defaults to `FALSE`.
#' @param pattern Optional character regex intended for verification. If `NULL`,
#'   defaults to a PCRE pattern approximating an assignment to `function_name`.
#'   Note: this argument is currently not used by the implementation (verification
#'   is done via `git grep -E` with an internally constructed ERE).
#' @param branch Optional character scalar. Ref to check out before searching.
#'   If `NULL`/empty, the function uses `origin/HEAD` when available, otherwise `HEAD`.
#' @param max_commits Maximum number of candidate commits to verify (in order).
#'   Use to cap runtime on very large histories. Defaults to `Inf` (no cap).
#' @param cache_dir Directory used to cache cloned repositories. Defaults to
#'   `getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))`.
#'
#' @details
#' Candidate commits are obtained with:
#' \itemize{
#'   \item `git log --reverse -S <function_name> <ref> -- R`
#' }
#' which searches for commits where the literal string `function_name` appears in
#' diffs under `R/` (additions or removals). Each candidate is then validated by
#' searching the repository *tree* at that commit with `git grep` for an
#' assignment-like pattern such as `foo <-` or `foo =`.
#'
#' If the local cached clone is corrupted or fetch fails, the cache directory for
#' that repo is deleted and recloned.
#'
#' For best performance across sessions, set a persistent cache location:
#' \preformatted{
#' options(ggext.git_cache = "~/Library/Caches/ggext_git")
#' }
#'
#' @return
#' If a defining commit is found:
#' \itemize{
#'   \item If `date_only = TRUE`, a `Date`.
#'   \item Otherwise, a one-row `data.frame` with columns:
#'     `package`, `fname`, `first_commit`, `author`, `message`, `url`, `file`.
#' }
#' If not found (or if the repo cannot be searched), returns `NA` when
#' `date_only = TRUE`, otherwise `NULL`.
#'
#' @examples
#' \dontrun{
#' # Return full metadata for first defining commit of "geom_ridgeline"
#' x <- get_first_commit("wilkelab", "ggridges", "geom_ridgeline")
#'
#' # Date only
#' d <- get_first_commit("tidyverse", "ggplot2", "geom_point", date_only = TRUE)
#'
#' # Limit verification work
#' x2 <- get_first_commit("someorg", "somerepo", "foo", max_commits = 200)
#' }
#'
#' @seealso
#' \code{\link[processx:run]{processx::run}}
#'
#' @export
get_first_commit <- function(owner, repo, function_name,
                             date_only = FALSE,
                             pattern = NULL,
                             branch = NULL,
                             max_commits = Inf,
                             cache_dir = getOption(
                               "ggext.git_cache",
                               file.path(tempdir(), "gh_repo_cache")
                             )) {

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it first.")
  }

  message("Processing: ", owner, " ", repo, " ", function_name)

  fail <- function(reason, details = NULL) {
    message(
      paste0(
        "get_first_commit(): ", reason,
        " [", owner, "/", repo, " :: ", function_name, "]",
        if (!is.null(details) && nzchar(details)) paste0("\n", details) else ""
      )
    )
    if (date_only) as.Date(NA) else NULL
  }

  if (any(is.na(c(owner, repo, function_name))) ||
      !nzchar(owner) || !nzchar(repo) || !nzchar(function_name)) {
    return(fail("invalid inputs (owner/repo/function_name missing or empty)"))
  }

  if (is.null(pattern)) {
    # Kept for API compatibility; verification uses git-grep ERE below.
    pattern <- sprintf("\\b%s\\b\\s*(<-|=)", function_name)
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

  fmt <- "%H%x09%ad%x09%an%x09%s"

  # Candidate commits: where the symbol string appears in a diff somewhere under R/
  res <- run_git(c(
    "-C", repo_dir, "log",
    "--reverse",
    "--date=short",
    paste0("--format=", fmt),
    "-S", function_name,
    ref,
    "--", "R"
  ))
  if (res$status != 0L) {
    return(fail(
      "git log failed while searching candidates under R/",
      paste(c(res$stdout, res$stderr), collapse = "\n")
    ))
  }
  if (!nzchar(res$stdout)) {
    # Could be "no candidates" OR "R/ doesn't exist". Disambiguate.
    chk <- run_git(c("-C", repo_dir, "ls-tree", "-r", "--name-only", ref, "--", "R"))
    if (chk$status != 0L) {
      return(fail(
        "could not determine (git ls-tree failed for 'R/')",
        paste(c(chk$stdout, chk$stderr), collapse = "\n")
      ))
    }
    if (!nzchar(chk$stdout)) {
      return(fail("directory 'R/' not found in repo at selected ref"))
    }
    return(fail("symbol never defined under R/ (no candidate commits)"))
  }

  # Parse commit lines
  lines <- trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return(fail("could not parse git log output (no commit lines)"))
  }

  # Build a git-grep regex (extended regex) for assignment.
  # ERE doesn't support \\b, so approximate with "word-ish" boundaries.
  grep_re <- sprintf("(^|[^A-Za-z0-9_.])%s([[:space:]]*)(<-|=)", function_name)

  checked <- 0L
  for (ln in lines) {
    checked <- checked + 1L
    if (checked > max_commits) {
      return(fail(sprintf("reached max_commits (%s) without verifying a definition", max_commits)))
    }

    parts <- strsplit(ln, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 4) next

    sha <- parts[1]
    dt  <- suppressWarnings(as.Date(parts[2]))
    au  <- parts[3]
    msg <- parts[4]

    gg <- run_git(c(
      "-C", repo_dir, "grep",
      "-n", "-E", grep_re,
      sha, "--", "R"
    ))

    if (gg$status == 0L && nzchar(gg$stdout)) {
      # Pick the first matching path for reporting
      first_hit <- strsplit(gg$stdout, "\n", fixed = TRUE)[[1]][1]
      # git grep output for tree searches is "sha:path:line:match"
      file_hit <- sub(":.*$", "", sub("^[^:]*:", "", first_hit))

      if (isTRUE(date_only)) return(dt)

      return(data.frame(
        package     = repo,
        fname       = function_name,
        first_commit = dt,
        author      = au,
        message     = msg,
        url         = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, sha),
        file        = file_hit
      ))
    }

    if (gg$status != 0L && gg$status != 1L) {
      # 1 means "no matches"; other nonzero codes suggest an error
      message(
        "get_first_commit(): git grep failed for candidate commit [",
        owner, "/", repo, " :: ", function_name, " @ ", sha, "]\n",
        paste(c(gg$stdout, gg$stderr), collapse = "\n")
      )
    }
  }

  fail("symbol never defined under R/ (no verified defining commit found)")
}
