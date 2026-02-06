# Find the first commit where a function is exported

Identifies the earliest Git commit in which a given symbol becomes
*explicitly exported* in a package’s `NAMESPACE` file (e.g. via
`export(foo)` or `export("foo")`).

## Usage

``` r
get_first_export_github(
  owner,
  repo,
  fname,
  date_only = TRUE,
  branch = NULL,
  cache_dir = getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache")),
  file = "NAMESPACE"
)
```

## Arguments

- owner:

  GitHub account name (e.g. `"YuLab-SMU"`).

- repo:

  GitHub repository name (e.g. `"ggtree"`).

- fname:

  Name of the symbol to search for (e.g. `"geom_aline"`).

- date_only:

  Logical; if `TRUE` (default), return only the commit date. If `FALSE`,
  return a one-row data frame with commit metadata.

- branch:

  Optional branch or ref to search. Defaults to the repository’s default
  branch.

- cache_dir:

  Directory used to cache cloned repositories. If not supplied
  explicitly, the function uses the value of the `ggext.git_cache`
  option, falling back to a temporary directory.

- file:

  Path to the NAMESPACE file within the repository. Defaults to
  `"NAMESPACE"`.

## Value

If `date_only = TRUE`, a `Date` giving the commit date when the symbol
was first explicitly exported, or `NULL` if no explicit export is found.

If `date_only = FALSE`, a one-row `data.frame` with columns:

- first_gh:

  Commit date

- author:

  Commit author name

- message:

  Commit message

- url:

  URL of the commit on GitHub

- file:

  File searched (usually `NAMESPACE`)

## Details

The function works by cloning the package repository locally (once, into
a cache directory) and searching the Git history of the `NAMESPACE`
file. After the initial clone, all operations are local and do not use
the GitHub API, avoiding rate-limit issues.

This function detects **explicit exports only**. It does not interpret
`exportPattern()` semantics; packages that rely solely on pattern-based
exports may return `NULL`.

Prints a progress message via
[`message()`](https://rdrr.io/r/base/message.html).

The initial call for a given repository may be slow due to cloning.
Subsequent calls are fast as long as the cached clone is reused.

For best performance across sessions, set a persistent cache location:

    options(ggext.git_cache = "~/Library/Caches/ggext_git")

## See also

get_first_commit

## Examples

``` r
if (FALSE) { # \dontrun{
get_first_export_github("YuLab-SMU", "ggtree", "geom_aline")
get_first_export_github("YuLab-SMU", "ggtree", "geom_aline", date_only = FALSE)
} # }
```
