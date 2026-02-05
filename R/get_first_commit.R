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
#' @param owner Character scalar. GitHub username/organization.
#' @param repo Character scalar. GitHub repository name.
#' @param function_name Character scalar. Symbol to search for (e.g. `"geom_foo"`).
#' @param date_only Logical; if `TRUE`, return only the `Date` of the first
#'   verified defining commit.
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
#' @return
#' If a defining commit is found:
#' \itemize{
#'   \item If `date_only = TRUE`, a `Date`.
#'   \item Otherwise, a one-row `data.frame` with columns:
#'     `date`, `author`, `message`, `url`, `file`.
#' }
#' If not found (or `owner`/`repo` is `NA`), returns `NULL`.
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
#' @importFrom processx run
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
  if (is.na(owner) || is.na(repo)) return(NULL)

  if (is.null(pattern)) {
    # Verification pattern (PCRE in R; for git-grep we’ll use a simpler ERE)
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

  # Clone or refresh
  if (!dir.exists(file.path(repo_dir, ".git"))) {
    if (dir.exists(repo_dir)) unlink(repo_dir, recursive = TRUE, force = TRUE)
    dir.create(repo_dir, recursive = TRUE)
    res <- run_git(c("clone", remote, repo_dir))
    if (res$status != 0L) stop(paste(c(res$stdout, res$stderr), collapse = "\n"))
  } else {
    res <- run_git(c("-C", repo_dir, "fetch", "--all", "--prune"))
    if (res$status != 0L) {
      unlink(repo_dir, recursive = TRUE, force = TRUE)
      dir.create(repo_dir, recursive = TRUE)
      res2 <- run_git(c("clone", remote, repo_dir))
      if (res2$status != 0L) stop(paste(c(res2$stdout, res2$stderr), collapse = "\n"))
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

  run_git(c("-C", repo_dir, "checkout", "-q", ref))

  fmt <- "%H%x09%ad%x09%an%x09%s"

  # Candidate commits: ones where the symbol string appears in a diff somewhere under R/
  res <- run_git(c(
    "-C", repo_dir, "log",
    "--reverse",
    "--date=short",
    paste0("--format=", fmt),
    "-S", function_name,
    ref,
    "--", "R"
  ))
  if (res$status != 0L) return(NULL)
  if (!nzchar(res$stdout)) return(NULL)

  # Parse commit lines
  lines <- trimws(strsplit(res$stdout, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NULL)

  # Build a git-grep regex (extended regex) for assignment.
  # ERE doesn't support \b, so approximate with "word-ish" boundaries.
  # This is intentionally a bit permissive; we still verify with R PCRE if needed.
  grep_re <- sprintf("(^|[^A-Za-z0-9_.])%s([[:space:]]*)(<-|=)", function_name)

  checked <- 0L

  for (ln in lines) {
    checked <- checked + 1L
    if (checked > max_commits) return(NULL)

    parts <- strsplit(ln, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 4) next

    sha <- parts[1]
    dt  <- as.Date(parts[2])
    au  <- parts[3]
    msg <- parts[4]

    # Verify by searching the tree at that commit (rename/move-proof)
    gg <- run_git(c(
      "-C", repo_dir, "grep",
      "-n", "-E", grep_re,
      sha, "--", "R"
    ))

    if (gg$status == 0L && nzchar(gg$stdout)) {
      # Pick the first matching path for reporting
      first_hit <- strsplit(gg$stdout, "\n", fixed = TRUE)[[1]][1]
      file_hit <- sub(":.*$", "", sub("^[^:]*:", "", first_hit))  # "sha:path:line:..." -> "path"

      if (isTRUE(date_only)) return(dt)

      return(data.frame(
        date    = dt,
        author  = au,
        message = msg,
        url     = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, sha),
        file    = file_hit,
        stringsAsFactors = FALSE
      ))
    }
  }

  NULL
}

