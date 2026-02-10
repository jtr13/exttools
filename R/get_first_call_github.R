#' Find the first commit where a function is called in a GitHub repo
#'
#' Clones a GitHub repository into a local cache directory (if not already
#' present), checks out a target ref (branch or default), then scans commits
#' (oldest to newest) whose diffs under `dir` match a call-like pattern for
#' `fname` (e.g. `foo(` or `pkg::foo(`). For each candidate commit, it verifies
#' the call is present somewhere in the repository tree at that commit using
#' `git grep`.
#'
#' Unlike functions that track *latest* state, this function is concerned only
#' with historical "firsts". Therefore, **if a cached clone already exists, no
#' network fetch is performed**. The cached repository is assumed to contain a
#' complete history.
#'
#' If the cached clone is detected to be shallow or invalid, it is deleted and
#' recloned to ensure correctness.
#'
#' The return value is either the commit date (when `date_only = TRUE`) or a
#' one-row `data.frame` with commit metadata and a GitHub URL.
#'
#' This function emits diagnostic `message()` output when it cannot determine an
#' answer (e.g., repository not found/inaccessible, checkout failed, git errors)
#' or when `fname` is never called under `dir`.
#'
#' @param github_user Character scalar. GitHub username or organization.
#' @param repo Character scalar. GitHub repository name.
#' @param fname Character scalar. Symbol to search for (e.g. `"ggplot"`).
#' @param date_only Logical; if `TRUE`, return only the `Date` of the first
#'   verified call commit. Defaults to `FALSE`.
#' @param branch Optional character scalar. Ref to check out before searching.
#'   If `NULL` or empty, the function uses `origin/HEAD` when available,
#'   otherwise `HEAD`.
#' @param max_commits Maximum number of candidate commits to verify (in order).
#'   Use to cap runtime on very large histories. Defaults to `Inf` (no cap).
#' @param cache_dir Directory used to cache cloned repositories. Defaults to
#'   `getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))`.
#' @param dir Directory within the repo to search. Defaults to `"R"`.
#'
#' @details
#' Candidate commits are obtained with `git log --reverse -G <regex>` under
#' `dir`, using patterns that match `fname` calls like:
#' \itemize{
#'   \item `fname[[:space:]]*\\(`
#'   \item `::fname[[:space:]]*\\(`
#' }
#' Each candidate is then validated by searching the repository *tree* at that
#' commit with `git grep -E` for the same call-like pattern.
#'
#' This function shares the same on-disk Git clone cache as
#' `get_first_commit()` via the `ggext.git_cache` option. Repositories
#' cloned by either function are reused by the other.
#'
#' For best performance across sessions, set a persistent cache location:
#' \preformatted{
#' options(ggext.git_cache = "~/Library/Caches/ggext_git")
#' }
#'
#' @return
#' If a call is found:
#' \itemize{
#'   \item If `date_only = TRUE`, a `Date`.
#'   \item Otherwise, a one-row `data.frame` with columns:
#'     `github_user`, `repo`, `fname`, `first_call`, `author`, `message`, `url`, `file`.
#' }
#' If not found (or if the repository cannot be searched), returns `NA` when
#' `date_only = TRUE`, otherwise `NULL`.
#'
#' @export
get_first_call_github <- function(github_user, github_repo, fname = "ggplot",
                                  date_only = FALSE,
                                  branch = NULL,
                                  max_commits = Inf,
                                  cache_dir = getOption(
                                    "ggext.git_cache",
                                    file.path(tempdir(), "gh_repo_cache")
                                  ),
                                  dir = "R") {

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it first.")
  }

  message("Processing: ", github_user, " ", github_repo, " ", fname)

  fail <- function(reason, details = NULL) {
    message(
      paste0(
        "get_first_call_github(): ", reason,
        " [", github_user, "/", github_repo, " :: ", fname, "]",
        if (!is.null(details) && nzchar(details)) paste0("\n", details) else ""
      )
    )
    if (date_only) as.Date(NA) else NULL
  }

  if (any(is.na(c(github_user, github_repo, fname))) ||
      !nzchar(github_user) || !nzchar(github_repo) || !nzchar(fname)) {
    return(fail("invalid inputs (github_user/github_repo/fname missing or empty)"))
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  repo_dir <- file.path(cache_dir, paste0(github_user, "__", github_repo))
  remote   <- sprintf("https://github.com/%s/%s.git", github_user, github_repo)

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

  # ---- clone or reuse cache (NO FETCH) ----
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
    # detect shallow / partial clone
    chk <- run_git(c("-C", repo_dir, "rev-parse", "--is-shallow-repository"))
    if (chk$status != 0L || trimws(chk$stdout) == "true") {
      unlink(repo_dir, recursive = TRUE, force = TRUE)
      dir.create(repo_dir, recursive = TRUE)
      res <- run_git(c("clone", remote, repo_dir))
      if (res$status != 0L) {
        return(fail(
          "cached repo was shallow or corrupt and reclone failed",
          paste(c(res$stdout, res$stderr), collapse = "\n")
        ))
      }
    }
  }

  # ---- choose ref ----
  ref <- branch
  if (is.null(ref) || !nzchar(ref)) {
    res <- run_git(c(
      "-C", repo_dir,
      "symbolic-ref", "--quiet", "refs/remotes/origin/HEAD"
    ))
    if (res$status == 0L && nzchar(res$stdout)) {
      ref <- sub(
        "^refs/remotes/origin/",
        "",
        trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]][1])
      )
    } else {
      ref <- "HEAD"
    }
  }

  # ---- checkout ----
  res <- run_git(c("-C", repo_dir, "checkout", "-q", ref))
  if (res$status != 0L) {
    return(fail(
      sprintf("failed to checkout ref '%s'", ref),
      paste(c(res$stdout, res$stderr), collapse = "\n")
    ))
  }

  fmt <- "%H%x09%ad%x09%an%x09%s"

  parse_git_log <- function(stdout) {
    if (!nzchar(stdout)) return(character())
    lines <- strsplit(stdout, "\n", fixed = TRUE)[[1]]
    lines <- trimws(lines)
    lines[nzchar(lines)]
  }

  pat_plain <- sprintf("%s[[:space:]]*\\(", fname)
  pat_ns    <- sprintf("::%s[[:space:]]*\\(", fname)

  get_candidates <- function(pat) {
    res <- run_git(c(
      "-C", repo_dir, "log",
      "--reverse",
      "--date=short",
      paste0("--format=", fmt),
      "-G", pat,
      ref,
      "--", dir
    ))
    if (res$status != 0L) {
      attr(res, "err") <- paste(c(res$stdout, res$stderr), collapse = "\n")
      return(res)
    }
    parse_git_log(res$stdout)
  }

  cand1 <- get_candidates(pat_plain)
  cand2 <- get_candidates(pat_ns)

  if (inherits(cand1, "processx_result") ||
      inherits(cand2, "processx_result")) {
    det <- c()
    if (inherits(cand1, "processx_result")) det <- c(det, attr(cand1, "err"))
    if (inherits(cand2, "processx_result")) det <- c(det, attr(cand2, "err"))
    return(fail("git log failed while searching candidate commits",
                paste(det, collapse = "\n")))
  }

  lines <- unique(c(cand1, cand2))
  if (!length(lines)) {
    return(fail(sprintf("symbol never called under %s/ (no candidate commits)", dir)))
  }

  grep_plain <- sprintf("(^|[^A-Za-z0-9_.])%s[[:space:]]*\\(", fname)
  grep_ns    <- sprintf("(^|[^A-Za-z0-9_.]):{2}%s[[:space:]]*\\(", fname)

  checked <- 0L
  for (ln in lines) {
    checked <- checked + 1L
    if (checked > max_commits) {
      return(fail(sprintf("reached max_commits (%s) without verifying a call", max_commits)))
    }

    parts <- strsplit(ln, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 4) next

    sha <- parts[1]
    dt  <- suppressWarnings(as.Date(parts[2]))
    au  <- parts[3]
    msg <- parts[4]

    gg <- run_git(c("-C", repo_dir, "grep", "-n", "-E", grep_plain, sha, "--", dir))
    gg2 <- NULL
    if (!(gg$status == 0L && nzchar(gg$stdout))) {
      gg2 <- run_git(c("-C", repo_dir, "grep", "-n", "-E", grep_ns, sha, "--", dir))
    }

    if ((gg$status == 0L && nzchar(gg$stdout)) ||
        (!is.null(gg2) && gg2$status == 0L && nzchar(gg2$stdout))) {

      src <- if (gg$status == 0L && nzchar(gg$stdout)) gg$stdout else gg2$stdout
      first_hit <- strsplit(src, "\n", fixed = TRUE)[[1]][1]
      file_hit <- sub(":.*$", "", sub("^[^:]*:", "", first_hit))

      if (isTRUE(date_only)) return(dt)

      return(data.frame(
        github_user      = github_user,
        github_repo       = github_repo,
        fname      = fname,
        first_call = dt,
        author     = au,
        message    = msg,
        url        = sprintf("https://github.com/%s/%s/commit/%s", github_user, github_repo, sha),
        file       = file_hit
      ))
    }
  }

  fail(sprintf("symbol never called under %s/ (no verified call found)", dir))
}
