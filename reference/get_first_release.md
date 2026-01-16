# Get CRAN location and first release date for an R package

Determines whether a package is currently on CRAN, only in the CRAN
archive, or in neither location, and returns the earliest known release
date.

## Usage

``` r
get_first_release(pkg, cran_package_db, archive_df)
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

## Value

A one-row data frame with columns:

- `package` – package name

- `location` – `"on CRAN"`, `"CRAN archive only"`, or `"neither"`

- `first_release` – earliest known release date (`Date`) or `NA`

## Details

The function performs constant-time lookups using precomputed inputs and
is intended to be called repeatedly over many packages.

## Examples

``` r
if (FALSE) { # \dontrun{
cran_db <- tools::CRAN_package_db()
archive_df <- build_archive_df()

get_first_release("ggplot2", cran_db, archive_df)
} # }
```
