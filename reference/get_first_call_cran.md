# Find the first CRAN release containing a call to a function in an R file

Scans a package's CRAN source tarballs (current + archive) in
chronological order and returns the earliest release where a call to
`fname()` appears in any `R/` source file.

## Usage

``` r
get_first_call_cran(
  package,
  fname,
  date_only = FALSE,
  repos = "https://cloud.r-project.org",
  cache_dir = getOption("ggext.cran_cache", file.path(tempdir(), "cran_export_cache"))
)
```

## Arguments

- package:

  Character. The name of the CRAN package.

- fname:

  Character. Function name to search for (e.g. "ggplot").

- date_only:

  Logical. If TRUE, return only the CRAN publication Date.

- repos:

  Character. CRAN mirror URL.

- cache_dir:

  Character. Cache root directory (shared with
  [`get_first_export_cran()`](https://jtr13.github.io/exttools/reference/get_first_export_cran.md)).

## Value

If `date_only = TRUE`, a Date or NA.

Otherwise, a one-row data.frame with: package, fname, version,
first_call, file

## Details

This function shares the same cache root and version index as
[`get_first_export_cran()`](https://jtr13.github.io/exttools/reference/get_first_export_cran.md).
Once a version has been scanned, its result is cached permanently and
never refreshed.
