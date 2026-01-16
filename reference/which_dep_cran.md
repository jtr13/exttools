# Classify ggplot2 dependency type for CRAN packages

Classify ggplot2 dependency type for CRAN packages

## Usage

``` r
which_dep_cran(pkgs, cran_db)
```

## Arguments

- pkgs:

  Character vector of package names.

- cran_db:

  A data frame from tools::CRAN_package_db().

## Value

Character vector with values "depends", "imports", "suggests", or NA.
