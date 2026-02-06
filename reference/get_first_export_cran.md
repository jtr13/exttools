# Find the first CRAN release exporting a symbol

This function identifies the earliest version of a package on CRAN that
**explicitly** exports a given symbol in its `NAMESPACE` via `export()`.
It intentionally ignores `exportPattern()` to avoid false positives.

## Usage

``` r
get_first_export_cran(
  package,
  fname,
  date_only = FALSE,
  repos = "https://cloud.r-project.org",
  cache_dir = getOption("ggext.cran_cache", file.path(tempdir(), "cran_export_cache"))
)
```

## Arguments

- package:

  Character. The name of the CRAN package (e.g. `"ggplot2"`).

- fname:

  Character. The symbol to search for (e.g. `"geom_point"`).

- date_only:

  Logical. If `TRUE`, return only the publication `Date`. Defaults to
  `FALSE`.

- repos:

  Character. The CRAN mirror URL. Defaults to
  `"https://cloud.r-project.org"`.

- cache_dir:

  Character. Directory used to cache parsed CRAN metadata. Defaults to
  `getOption("ggext.cran_cache")`, falling back to a temporary
  directory.

## Value

If `date_only = TRUE`, a `Date` object or `NA`.

Otherwise, a one-row `data.frame` with columns:

- `package`: package name

- `fname`: function name

- `version`: CRAN version where the symbol is first exported

- `first_cran`: CRAN publication date (may be `NA` for older releases)

- `is_reexport`: logical indicating whether the symbol is imported via
  `importFrom()` (i.e., a re-export)

Returns `NULL` if the symbol is never exported.

## Details

Re-exports are detected by checking for a corresponding `importFrom()`
directive for the same symbol.

Results are cached per package and per version to avoid repeatedly
downloading and parsing CRAN tarballs.
