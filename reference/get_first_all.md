# Get “first seen” dates for a symbol across GitHub + CRAN

Computes a small one-row summary of earliest known dates across:

- first commit in the repo (any file),

- first commit mentioning/introducing `fname` (per your
  [`get_first_commit()`](https://jtr13.github.io/exttools/reference/get_first_commit.md)
  logic),

- first GitHub release/export date for `fname` (per
  `get_first_export_github()`),

- first CRAN release date for the package (via
  [`get_first_release()`](https://jtr13.github.io/exttools/reference/get_first_release.md)),

- first CRAN version that explicitly exports `fname` (via
  [`get_first_export_cran()`](https://jtr13.github.io/exttools/reference/get_first_export_cran.md)).

## Usage

``` r
get_first_all(user, repo, fname, cran_db = NULL, archive_df = NULL)
```

## Arguments

- user:

  Character scalar. GitHub owner/user (e.g. `"tidyverse"`).

- repo:

  Character scalar. GitHub repo name (and assumed CRAN package name).

- fname:

  Character scalar. Symbol to look up (e.g. `"pivot_longer"`).

- cran_db:

  Optional `data.frame` returned by
  [`tools::CRAN_package_db()`](https://rdrr.io/r/tools/CRANtools.html).
  If `NULL`, it is retrieved internally.

- archive_df:

  Optional archive lookup `data.frame` with columns `package` and
  `archive_date`, typically from
  [`build_archive_df()`](https://jtr13.github.io/exttools/reference/build_archive_df.md).
  If `NULL`, it is retrieved internally.

## Value

A one-row `data.frame` with columns:

- first_repo:

  Earliest commit date in the repository (`Date` or `NA`).

- first_commit:

  Earliest commit date returned by
  [`get_first_commit()`](https://jtr13.github.io/exttools/reference/get_first_commit.md)
  for `fname` (`Date` or `NA`).

- first_gh:

  Earliest GitHub export/release date for `fname` per
  `get_first_export_github()` (`Date` or `NA`).

- first_release:

  Earliest CRAN release date for the package per
  [`get_first_release()`](https://jtr13.github.io/exttools/reference/get_first_release.md)
  (`Date` or `NA`).

- first_cran:

  Earliest CRAN export date for `fname` per
  [`get_first_export_cran()`](https://jtr13.github.io/exttools/reference/get_first_export_cran.md)
  (`Date` or `NA`).

## Details

All returned values are `Date` scalars (or `NA`) and the function
returns a one-row `data.frame`.

This function assumes the CRAN package name matches `repo`. If that’s
not always true in your workflow, add a separate `pkg` argument and pass
that to
[`get_first_release()`](https://jtr13.github.io/exttools/reference/get_first_release.md)
and
[`get_first_export_cran()`](https://jtr13.github.io/exttools/reference/get_first_export_cran.md).

## Examples

``` r
if (FALSE) { # \dontrun{
get_first_all("tidyverse", "tidyr", "pivot_longer")
} # }
```
