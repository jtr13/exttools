# Find the first (root) commit in a GitHub repository

Clones (or refreshes) a GitHub repository into a shared local cache and
determines the earliest (root) commit using `git` history, without
relying on the GitHub REST API.

## Usage

``` r
get_first_repo_commit(
  owner,
  repo,
  branch = NULL,
  date_only = FALSE,
  cache_dir = getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))
)
```

## Arguments

- owner:

  Character scalar. GitHub username or organization.

- repo:

  Character scalar. GitHub repository name.

- branch:

  Optional character scalar. Git ref to use (e.g. `"main"`). If `NULL`
  or empty, the function uses `origin/HEAD` when available, otherwise
  `HEAD`.

- date_only:

  Logical. If `TRUE`, return only the `Date` of the first commit.
  Defaults to `FALSE`.

- cache_dir:

  Directory used to cache cloned repositories. Defaults to
  `getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))`.

## Value

If a root commit is found:

- When `date_only = TRUE`, a `Date`.

- Otherwise, a one-row `data.frame` with columns: `first_repo`,
  `author`, `message`, and `url`.

If the repository cannot be accessed or inspected:

- Returns `NA` when `date_only = TRUE`

- Returns `NULL` otherwise

## Details

This function uses the same on-disk Git repository cache as related
helpers:

- [`get_first_commit()`](https://jtr13.github.io/exttools/reference/get_first_commit.md)

- `get_first_export()`

- [`get_first_call_github()`](https://jtr13.github.io/exttools/reference/get_first_call_github.md)

The cache location is controlled by the `ggext.git_cache` option.
Repositories cloned or refreshed by any of these functions are reused
across calls, avoiding redundant network access.

## See also

[`get_first_commit()`](https://jtr13.github.io/exttools/reference/get_first_commit.md),
`get_first_export()`,
[`get_first_call_github()`](https://jtr13.github.io/exttools/reference/get_first_call_github.md)

## Examples

``` r
if (FALSE) { # \dontrun{
get_first_repo_commit("YuLab-SMU", "ggtree")
get_first_repo_commit("tidyverse", "ggplot2", date_only = TRUE)

options(ggext.git_cache = "~/Library/Caches/ggext_git")
} # }
```
