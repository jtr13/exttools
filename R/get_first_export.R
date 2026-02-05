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
#' @param function_name Character. The symbol to search for (e.g. `"geom_point"`).
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
#' - `version`: CRAN version where the symbol is first exported
#' - `date`: CRAN publication date (may be `NA` for older releases)
#' - `is_reexport`: logical indicating whether the symbol is imported via
#'   `importFrom()` (i.e., a re-export)
#'
#' Returns `NULL` if the symbol is never exported.
#'
#' @export
get_first_export_cran <- function(package, function_name,
                                  date_only = FALSE,
                                  repos = "https://cloud.r-project.org",
                                  cache_dir = getOption(
                                    "ggext.cran_cache",
                                    file.path(tempdir(), "cran_export_cache")
                                  )) {

  if (any(is.na(c(package, function_name))) ||
      !nzchar(package) || !nzchar(function_name)) {
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
    if (file.exists(cache_path)) return(readRDS(cache_path))

    tmp_tar <- tempfile(fileext = ".tar.gz")
    on.exit(unlink(tmp_tar), add = TRUE)

    ok <- tryCatch(
      utils::download.file(url, tmp_tar, quiet = TRUE, mode = "wb"),
      error = function(e) 1
    )
    if (ok != 0) return(NULL)

    members <- utils::untar(tmp_tar, list = TRUE)
    targets <- members[
      grepl(sprintf("^(\\./)?%s/(NAMESPACE|DESCRIPTION)$", package), members)
    ]

    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    utils::untar(tmp_tar, files = targets, exdir = tmp_dir)

    ns_file <- list.files(tmp_dir, "NAMESPACE", recursive = TRUE, full.names = TRUE)[1]
    desc_file <- list.files(tmp_dir, "DESCRIPTION", recursive = TRUE, full.names = TRUE)[1]

    ns <- readLines(ns_file, warn = FALSE)
    desc <- read.dcf(desc_file)

    date_str <- if ("Date/Publication" %in% colnames(desc)) {
      desc[1, "Date/Publication"]
    } else {
      desc[1, "Date"]
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
      sprintf("^\\s*export\\s*\\(\\s*[^)]*\\b%s\\b", function_name),
      info$ns,
      perl = TRUE
    ))

    if (exported) {
      if (date_only) return(info$date)

      is_reexport <- any(grepl(
        sprintf("^\\s*importFrom\\s*\\([^,]+,\\s*[^)]*\\b%s\\b", function_name),
        info$ns,
        perl = TRUE
      ))

      return(data.frame(
        package = package,
        version = info$version,
        date = info$date,
        is_reexport = is_reexport,
        stringsAsFactors = FALSE
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
#' exports may return `NULL`.
#'
#' @param owner GitHub account name (e.g. `"YuLab-SMU"`).
#' @param repo GitHub repository name (e.g. `"ggtree"`).
#' @param function_name Name of the symbol to search for (e.g. `"geom_aline"`).
#' @param date_only Logical; if `TRUE` (default), return only the commit date.
#'   If `FALSE`, return a one-row data frame with commit metadata.
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
#' first explicitly exported, or `NULL` if no explicit export is found.
#'
#' If `date_only = FALSE`, a one-row `data.frame` with columns:
#' \describe{
#'   \item{date}{Commit date}
#'   \item{author}{Commit author name}
#'   \item{message}{Commit message}
#'   \item{url}{URL of the commit on GitHub}
#'   \item{file}{File searched (usually `NAMESPACE`)}
#' }
#'
#' @details
#' Prints a progress message via `message()`.
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
#' get_first_export_github("YuLab-SMU", "ggtree", "geom_aline", date_only = FALSE)
#' }
#'
#' @export
get_first_export_github <- function(owner, repo, function_name,
                                    date_only = TRUE,
                                    branch = NULL,
                                    cache_dir = getOption(
                                      "ggext.git_cache",
                                      file.path(tempdir(), "gh_repo_cache")
                                    ),
                                    file = "NAMESPACE") {
  message("Processing:", owner, repo, function_name)

  if (is.na(owner) || is.na(repo)) {
    return(NULL)
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

  # Clone or refresh (reclone on fetch failure)
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

  # Checkout quietly
  run_git(c("-C", repo_dir, "checkout", "-q", ref))

  fmt <- "%H%x09%ad%x09%an%x09%s"

  needles <- c(
    sprintf("export(%s", function_name),
    sprintf('export("%s"', function_name),
    sprintf("export('%s'", function_name)
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
      date = as.Date(parts[2]),
      author = parts[3],
      message = parts[4]
    )
  }

  hits <- Filter(Negate(is.null), lapply(needles, search_one))
  if (!length(hits)) return(NULL)

  hit <- hits[[which.min(vapply(hits, function(x) as.numeric(x$date), numeric(1)))]]

  if (isTRUE(date_only)) return(hit$date)

  data.frame(
    date    = hit$date,
    author  = hit$author,
    message = hit$message,
    url     = sprintf("https://github.com/%s/%s/commit/%s", owner, repo, hit$sha),
    file    = file,
    stringsAsFactors = FALSE
  )
}


