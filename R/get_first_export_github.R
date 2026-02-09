get_first_export_github <- function(owner, repo, fname,
                                    date_only = FALSE,
                                    branch = NULL,
                                    cache_dir = getOption(
                                      "ggext.git_cache",
                                      file.path(tempdir(), "gh_repo_cache")
                                    ),
                                    file = "NAMESPACE") {

  message("Processing: ", owner, " ", repo, " ", fname)

  if (any(is.na(c(owner, repo, fname))) ||
      !nzchar(owner) || !nzchar(repo) || !nzchar(fname)) {
    msg <- "invalid inputs (owner/repo/fname missing or empty)"
    message("get_first_export_github(): ", msg,
            " [", owner, "/", repo, " :: ", fname, "]")
    return(if (date_only) as.Date(NA) else NULL)
  }

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it first.")
  }

  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
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

  fail <- function(reason, details = NULL) {
    message(
      paste0(
        "get_first_export_github(): ",
        reason,
        " [", owner, "/", repo, " :: ", fname, "]",
        if (!is.null(details) && nzchar(details)) paste0("\n", details) else ""
      )
    )
    if (date_only) as.Date(NA) else NULL
  }

  # ---- clean empty cache dirs (failed prior clone) ----
  if (dir.exists(repo_dir) &&
      length(list.files(repo_dir, all.files = TRUE, no.. = TRUE)) == 0L) {
    unlink(repo_dir, recursive = TRUE, force = TRUE)
  }

  # ---- clone only if not cached (NO FETCH) ----
  if (!dir.exists(file.path(repo_dir, ".git"))) {
    if (dir.exists(repo_dir)) unlink(repo_dir, recursive = TRUE, force = TRUE)
    res <- run_git(c("clone", remote, repo_dir))
    if (res$status != 0L) {
      return(fail(
        "repository not found or inaccessible (clone failed)",
        paste(c(res$stdout, res$stderr), collapse = "\n")
      ))
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

  needles <- c(
    sprintf("export(%s", fname),
    sprintf('export("%s"', fname),
    sprintf("export('%s'", fname)
  )

  parse_git_log <- function(stdout) {
    if (!nzchar(stdout)) return(character())
    lines <- strsplit(stdout, "\n", fixed = TRUE)[[1]]
    lines <- trimws(lines)
    lines[nzchar(lines)]
  }

  search_one <- function(needle) {
    res <- run_git(c(
      "-C", repo_dir, "log",
      "--reverse",
      "--date=short",
      paste0("--format=", fmt),
      "-S", needle,
      "--", file
    ))
    if (res$status != 0L) return(NULL)

    lines <- parse_git_log(res$stdout)
    if (!length(lines)) return(NULL)

    parts <- strsplit(lines[1], "\t", fixed = TRUE)[[1]]
    if (length(parts) < 4) return(NULL)

    list(
      sha = parts[1],
      date = suppressWarnings(as.Date(parts[2])),
      author = parts[3],
      message = parts[4]
    )
  }

  hits <- Filter(is.list, lapply(needles, search_one))
  if (!length(hits)) {
    chk <- run_git(c("-C", repo_dir, "ls-tree", "-r", "--name-only", ref, "--", file))
    if (chk$status != 0L) {
      return(fail(
        sprintf("could not determine (git ls-tree failed for file '%s')", file),
        paste(c(chk$stdout, chk$stderr), collapse = "\n")
      ))
    }
    if (!nzchar(chk$stdout)) {
      return(fail(sprintf("file '%s' not found in repo at ref '%s'", file, ref)))
    }
    return(fail("symbol never explicitly exported (no matching export() in file history)"))
  }

  hit <- hits[[which.min(vapply(hits, function(x) as.numeric(x$date), numeric(1))) ]]

  if (date_only) return(hit$date)

  data.frame(
    package  = repo,
    fname    = fname,
    first_gh = hit$date,
    author   = hit$author,
    message  = hit$message,
    url      = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, hit$sha),
    file     = file
  )
}
