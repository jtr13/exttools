# Find the first commit where a symbol is explicitly exported on GitHub

Identifies the earliest Git commit in which a given symbol becomes
**explicitly exported** in a repository file (by default, the package
`NAMESPACE`). The function clones the repository locally (once) into a
cache directory and then searches *local* Git history for export
directives.

## Usage

``` r
get_first_export_github(
  owner,
  repo,
  fname,
  date_only = FALSE,
  branch = NULL,
  cache_dir = getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache")),
  file = "NAMESPACE"
)
```

## Arguments

- owner:

  Character. GitHub account name (e.g. `"tidyverse"`).

- repo:

  Character. GitHub repository name (e.g. `"ggplot2"`).

- fname:

  Character. Symbol/function name to search for (e.g. `"geom_point"`).

- date_only:

  Logical. If `TRUE`, return only the commit date as a `Date`. If
  `FALSE` (default), return a one-row `data.frame` with commit metadata.

- branch:

  Character. Optional ref to check out before searching (branch, tag, or
  commit-ish). If `NULL`, uses `origin/HEAD` when available; otherwise
  uses `HEAD`.

- cache_dir:

  Character. Directory used to cache cloned repositories. Defaults to
  `getOption("ggext.git_cache", file.path(tempdir(),"gh_repo_cache"))`.

- file:

  Character. Path (within the repo) to the file to search. Defaults to
  `"NAMESPACE"`.

## Value

If `date_only = TRUE`, a `Date` (or `as.Date(NA)` on failure). Otherwise
a one-row `data.frame` with columns: `package`, `fname`, `first_gh`,
`author`, `message`, `url`, `file`; or `NULL` on failure.

## Details

After the initial clone, all operations are local and do **not** use the
GitHub API. This avoids rate limits and allows the function to work
without Wi-Fi **as long as the repository has already been cloned into
the cache**.

The search looks for these explicit forms:

- `export(fname)`

- `export("fname")`

- `export('fname')`

Notes:

- This detects **explicit exports only**. It does not interpret
  `exportPattern()` semantics.

- The history search uses `git log -S` on the target file (default:
  `NAMESPACE`); if the file never existed at the chosen ref, the
  function returns `NULL` (or `as.Date(NA)` with `date_only = TRUE`).

- No fetch/pull is performed. If the remote repository updates, you must
  delete the cached clone to refresh it.

## Examples

``` r
if (FALSE) { # \dontrun{
# Earliest explicit export date (requires git; clones once, then local)
get_first_export_github("tidyverse", "ggplot2", "geom_point", date_only = TRUE)

# Full commit metadata
get_first_export_github("tidyverse", "ggplot2", "geom_point")

# Search a different ref or file
get_first_export_github("tidyverse", "ggplot2", "geom_point", branch = "main")
get_first_export_github("someowner", "somerepo", "foo", file = "path/to/NAMESPACE")
} # }
```
