# Build a lookup table of CRAN archive first appearance dates

Constructs a data frame mapping package names to their earliest known
appearance in the CRAN archive, inferred from file modification times.

## Usage

``` r
build_archive_df(cran_archive_db = NULL)
```

## Arguments

- cran_archive_db:

  Optional result of
  [`tools::CRAN_archive_db()`](https://rdrr.io/r/tools/CRANtools.html).
  If not supplied, it is retrieved internally.

## Value

A data frame with columns:

- `package` – package name

- `archive_date` – earliest archive date (`Date`) or `NA`

## Details

The CRAN archive database is a named list of data frames, one per
package. For each package, the earliest file `mtime` is used as a proxy
for the archive entry date. Packages with empty archive entries return
`NA`.

This function is intended to be called once and the result reused for
efficient per-package lookups.

## Examples

``` r
if (FALSE) { # \dontrun{
archive_db <- tools::CRAN_archive_db()
archive_df <- build_archive_df(archive_db)
} # }
```
