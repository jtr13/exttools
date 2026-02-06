# Get CRAN location and first release date for an R package

Determines whether a package is currently on CRAN, only in the CRAN
archive, or in neither location, and returns the earliest known release
date.

## Usage

``` r
get_first_release(
  pkg,
  cran_package_db = NULL,
  archive_df = NULL,
  date_only = FALSE
)
```

## Arguments

- pkg:

  Character scalar. Package name.

- cran_package_db:

  A data frame returned by
  [`tools::CRAN_package_db()`](https://rdrr.io/r/tools/CRANtools.html).

- archive_df:

  A data frame with columns `package` and `archive_date`, typically
  created by
  [`build_archive_df()`](https://jtr13.github.io/exttools/reference/build_archive_df.md).

- date_only:

  Logical. If TRUE, return only the release date as a scalar.

## Value

If `date_only = FALSE` (default), a one-row data frame with columns:

- `package`

- `first_release`

If `date_only = TRUE`, a single `Date` (or `NA`).
