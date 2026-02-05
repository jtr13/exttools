# Extract ggplot2-extension components from a CRAN package

Downloads a CRAN source tarball for `pkg_name` (unless cached), extracts
the package `NAMESPACE`, and calls `analyze_pkg_source()` to identify
exported ggplot2 “grammar” components (e.g., functions named like
`geom_*`, `stat_*`, `scale_*`, etc., depending on what
`analyze_pkg_source()` detects).

## Usage

``` r
get_components_cran(pkg_name, cran_db = NULL, cache_root = "~/CRAN")
```

## Arguments

- pkg_name:

  Character scalar. CRAN package name.

- cran_db:

  Optional CRAN package database as returned by
  [`tools::CRAN_package_db()`](https://rdrr.io/r/tools/CRANtools.html).
  If `NULL`, it is computed on demand.

- cache_root:

  Character scalar. Root directory used for on-disk caching of extracted
  `NAMESPACE` files. Default is `"~/CRAN"`.

## Value

A `data.frame` with columns:

- package:

  Package name.

- component:

  Component type/category identified by `analyze_pkg_source()`.

- fname:

  Function name.

If the package cannot be found on CRAN, downloaded, or its `NAMESPACE`
cannot be extracted, returns an empty data frame with the same columns.

## Details

The function caches the `NAMESPACE` file at
`cache_root/<pkg>/<version>/NAMESPACE` and keeps that directory “clean”
by removing any other files that might be present.

Network access is required on the first run for a given package version.
The cache is keyed by package version from `cran_db`, so updates on CRAN
will be cached separately.

This function assumes `analyze_pkg_source(pkg_name, cache_dir)` exists
and can operate given a directory that contains (at minimum) a
`NAMESPACE` file.

## See also

[`tools::CRAN_package_db()`](https://rdrr.io/r/tools/CRANtools.html),
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html),
[`utils::untar()`](https://rdrr.io/r/utils/untar.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Using a precomputed CRAN DB speeds up repeated calls
db <- tools::CRAN_package_db()
out <- get_components_cran("ggrepel", cran_db = db, cache_root = tempdir())
head(out)

# Use the default cache under ~/CRAN
out2 <- get_components_cran("ggridges")
} # }
```
