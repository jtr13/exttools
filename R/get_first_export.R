#' Find the first CRAN release exporting a symbol
#'
#' This function identifies the earliest version of a package on CRAN that
#' **explicitly** exports a given symbol in its `NAMESPACE` via `export()`.
#' It intentionally ignores `exportPattern()` to avoid false positives.
#'
#' Re-exports are detected by checking for a corresponding `importFrom()`
#' directive for the same symbol.
#'
#' Results are cached per package and per version to avoid repeatedly
#' downloading and parsing CRAN tarballs.
#'
#' @param package Character. The name of the CRAN package (e.g. `"ggplot2"`).
#' @param fname Character. The symbol to search for (e.g. `"geom_point"`).
#' @param date_only Logical. If `TRUE`, return only the publication `Date`.
#'   Defaults to `FALSE`.
#' @param repos Character. The CRAN mirror URL. Defaults to
#'   `"https://cloud.r-project.org"`.
#' @param cache_dir Character. Directory used to cache parsed CRAN metadata.
#'   Defaults to `getOption("ggext.cran_cache")`, falling back to a temporary
#'   directory.
#'
#' @return
#' If `date_only = TRUE`, a `Date` object or `NA`.
#'
#' Otherwise, a one-row `data.frame` with columns:
#' - `package`: package name
#' - `fname`: function name
#' - `version`: CRAN version where the symbol is first exported
#' - `first_cran`: CRAN publication date (may be `NA` for older releases)
#' - `is_reexport`: logical indicating whether the symbol is imported via
#'   `importFrom()` (i.e., a re-export)
#'
#' Returns `NULL` if the symbol is never exported.
#'
#' @export
get_first_export_cran <- function(package, fname,
                                  date_only = FALSE,
                                  repos = "https://cloud.r-project.org",
                                  cache_dir = getOption(
                                    "ggext.cran_cache",
                                    file.path(tempdir(), "cran_export_cache")
                                  )) {
  message("Processing: ", package)

  if (any(is.na(c(package, fname))) || !nzchar(package) || !nzchar(fname)) {
    return(if (date_only) as.Date(NA) else NULL)
  }

  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  pkg_cache <- file.path(cache_dir, package)
  parsed_cache <- file.path(pkg_cache, "parsed")
  if (!dir.exists(parsed_cache)) dir.create(parsed_cache, recursive = TRUE)

  # ---- version index ----
  get_version_index <- function() {
    index_path <- file.path(pkg_cache, "index.rds")
    if (file.exists(index_path)) return(readRDS(index_path))

    arch_url <- sprintf("%s/src/contrib/Archive/%s/", repos, package)

    arch_files <- character()
    arch_lines <- suppressWarnings(
      tryCatch(readLines(arch_url, warn = FALSE), error = function(e) NULL)
    )
    if (!is.null(arch_lines)) {
      m <- regmatches(
        arch_lines,
        gregexpr(sprintf("%s_.*?\\.tar\\.gz", package), arch_lines)
      )
      arch_files <- unique(unlist(m))
    }

    ap <- tryCatch(
      utils::available.packages(repos = repos, type = "source"),
      error = function(e) NULL
    )

    curr_file <- if (!is.null(ap) && package %in% rownames(ap)) {
      sprintf("%s_%s.tar.gz", package, ap[package, "Version"])
    } else character()

    all_files <- unique(c(arch_files, curr_file))
    if (!length(all_files)) return(NULL)

    vers <- sub(paste0("^", package, "_"), "", sub("\\.tar\\.gz$", "", all_files))

    df <- data.frame(
      version = vers,
      url = ifelse(
        all_files %in% arch_files,
        paste0(arch_url, all_files),
        sprintf("%s/src/contrib/%s", repos, all_files)
      ),
      stringsAsFactors = FALSE
    )

    df <- df[order(numeric_version(df$version)), ]
    saveRDS(df, index_path)
    df
  }

  # ---- parse one version ----
  parse_version <- function(url, version) {
    cache_path <- file.path(parsed_cache, paste0(version, ".rds"))

    # If cache exists, try reading it; if malformed, delete and rebuild.
    if (file.exists(cache_path)) {
      cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)

      # Backward-compat: accept old caches that used `cran_first`
      if (is.list(cached) && is.null(cached$date) && !is.null(cached$cran_first)) {
        cached$date <- cached$cran_first
      }

      ok <- is.list(cached) &&
        all(c("version", "date", "ns") %in% names(cached)) &&
        length(cached$version) == 1 &&
        (inherits(cached$date, "Date") || (length(cached$date) == 1 && is.na(cached$date))) &&
        is.character(cached$ns)

      if (ok) return(cached)
      unlink(cache_path, force = TRUE)
    }

    tmp_tar <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(tmp_tar), add = TRUE)

    ok <- tryCatch(
      utils::download.file(url, tmp_tar, quiet = TRUE, mode = "wb"),
      error = function(e) 1
    )
    if (ok != 0) return(NULL)

    members <- tryCatch(utils::untar(tmp_tar, list = TRUE), error = function(e) character())
    if (!length(members)) return(NULL)

    targets <- members[
      grepl(sprintf("^(\\./)?%s/(NAMESPACE|DESCRIPTION)$", package), members)
    ]
    if (!length(targets)) return(NULL)

    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    # Extract only NAMESPACE + DESCRIPTION if present
    tryCatch(
      utils::untar(tmp_tar, files = targets, exdir = tmp_dir),
      error = function(e) return(NULL)
    )

    ns_files <- list.files(tmp_dir, "NAMESPACE", recursive = TRUE, full.names = TRUE)
    desc_files <- list.files(tmp_dir, "DESCRIPTION", recursive = TRUE, full.names = TRUE)
    if (!length(ns_files) || !length(desc_files)) return(NULL)

    ns <- readLines(ns_files[1], warn = FALSE)
    desc <- tryCatch(read.dcf(desc_files[1]), error = function(e) NULL)
    if (is.null(desc)) return(NULL)

    # Robust date extraction: Date/Publication -> Date -> NA
    date_str <- if ("Date/Publication" %in% colnames(desc)) {
      desc[1, "Date/Publication"]
    } else if ("Date" %in% colnames(desc)) {
      desc[1, "Date"]
    } else {
      NA_character_
    }
    date_str <- unname(as.character(date_str))

    res <- list(
      version = version,
      date = suppressWarnings(as.Date(date_str)),
      ns = ns
    )
    saveRDS(res, cache_path)
    res
  }

  # ---- main loop ----
  versions <- get_version_index()
  if (is.null(versions)) return(if (date_only) as.Date(NA) else NULL)

  for (i in seq_len(nrow(versions))) {
    info <- parse_version(versions$url[i], versions$version[i])
    if (is.null(info)) next

    exported <- any(grepl(
      sprintf("^\\s*export\\s*\\(\\s*[^)]*\\b%s\\b", fname),
      info$ns,
      perl = TRUE
    ))

    if (exported) {
      if (date_only) return(info$date)

      is_reexport <- any(grepl(
        sprintf("^\\s*importFrom\\s*\\([^,]+,\\s*[^)]*\\b%s\\b", fname),
        info$ns,
        perl = TRUE
      ))

      return(data.frame(
        package = package,
        fname = fname,
        version = info$version,
        first_cran = info$date,
        is_reexport = is_reexport
      ))
    }
  }

  if (date_only) as.Date(NA) else NULL
}


#' Find the first commit where a function is exported
#'
#' Identifies the earliest Git commit in which a given symbol becomes
#' *explicitly exported* in a package’s `NAMESPACE` file (e.g. via
#' `export(foo)` or `export("foo")`).
#'
#' The function works by cloning the package repository locally (once, into
#' a cache directory) and searching the Git history of the `NAMESPACE` file.
#' After the initial clone, all operations are local and do not use the
#' GitHub API, avoiding rate-limit issues.
#'
#' This function detects **explicit exports only**. It does not interpret
#' `exportPattern()` semantics; packages that rely solely on pattern-based
#' exports return `NULL` (or `NA` when `date_only = TRUE`).
#'
#' @param owner GitHub account name (e.g. `"YuLab-SMU"`).
#' @param repo GitHub repository name (e.g. `"ggtree"`).
#' @param fname Name of the symbol to search for (e.g. `"geom_aline"`).
#' @param date_only Logical; if `TRUE`, return only the commit date.
#'   Defaults to `FALSE`.
#' @param branch Optional branch or ref to search. Defaults to the repository’s
#'   default branch.
#' @param cache_dir Directory used to cache cloned repositories. If not
#'   supplied explicitly, the function uses the value of the
#'   `ggext.git_cache` option, falling back to a temporary directory.
#' @param file Path to the NAMESPACE file within the repository.
#'   Defaults to `"NAMESPACE"`.
#'
#' @return
#' If `date_only = TRUE`, a `Date` giving the commit date when the symbol was
#' first explicitly exported, or `NA`.
#'
#' If `date_only = FALSE`, a one-row `data.frame` with columns:
#' \describe{
#'   \item{package}{Repository name (assumed package name).}
#'   \item{fname}{Symbol searched.}
#'   \item{first_gh}{Commit date.}
#'   \item{author}{Commit author name.}
#'   \item{message}{Commit message.}
#'   \item{url}{URL of the commit on GitHub.}
#'   \item{file}{File searched (usually `NAMESPACE`).}
#' }
#'
#' @details
#' Prints a progress message via `message()`. On failure or no hit, prints a
#' diagnostic message indicating the reason (e.g., repository not found,
#' checkout failed, symbol not exported).
#'
#' The initial call for a given repository may be slow due to cloning.
#' Subsequent calls are fast as long as the cached clone is reused.
#'
#' For best performance across sessions, set a persistent cache location:
#' \preformatted{
#' options(ggext.git_cache = "~/Library/Caches/ggext_git")
#' }
#'
#' @seealso get_first_commit
#'
#' @examples
#' \dontrun{
#' get_first_export_github("YuLab-SMU", "ggtree", "geom_aline")
#' get_first_export_github("YuLab-SMU", "ggtree", "geom_aline", date_only = TRUE)
#' }
#'
#' @export
get_first_export_github <- function(owner, repo, fname,
                                    date_only = FALSE,
                                    branch = NULL,
                                    cache_dir = getOption(
                                      "ggext.git_cache",
                                      file.path(tempdir(), "gh_repo_cache")
                                    ),
                                    file = "NAMESPACE") {
  message("Processing: ", owner, " ", repo, " ", fname)

  if (any(is.na(c(owner, repo, fname))) || !nzchar(owner) || !nzchar(repo) || !nzchar(fname)) {
    msg <- "invalid inputs (owner/repo/fname missing or empty)"
    message("get_first_export_github(): ", msg, " [", owner, "/", repo, " :: ", fname, "]")
    return(if (date_only) as.Date(NA) else NULL)
  }

  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required. Install it first.")
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

  fail <- function(reason, details = NULL) {
    msg <- paste0(
      "get_first_export_github(): ",
      reason,
      " [", owner, "/", repo, " :: ", fname, "]",
      if (!is.null(details) && nzchar(details)) paste0("\n", details) else ""
    )
    message(msg)
    if (date_only) as.Date(NA) else NULL
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

    # If git returns nonzero, treat as "could not determine" for this needle.
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

  hits <- lapply(needles, search_one)
  hits <- Filter(is.list, hits)
  if (!length(hits)) {
    # Optional extra check: does the file exist at all at the searched ref?
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

  hit <- hits[[which.min(vapply(hits, function(x) as.numeric(x$date), numeric(1)))]]

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
