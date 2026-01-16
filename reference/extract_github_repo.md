# Extract GitHub user and repository from text

Parses a character string (typically derived from CRAN `URL` and/or
`BugReports` fields) and extracts the first GitHub `user/repository`
pair it finds.

## Usage

``` r
extract_github_repo(url_text)
```

## Arguments

- url_text:

  A single character string containing one or more URLs or free-form
  text that may include a GitHub repository link.

## Value

A one-row data frame with two character columns:

- github_user:

  GitHub account name, or `NA_character_` if not found

- github_repo:

  GitHub repository name, or `NA_character_` if not found

## Details

The function looks for URLs of the form: `github.com/<user>/<repo>`
(optionally ending in `.git`).

If no GitHub repository can be identified, a one-row data frame with
`NA` values is returned. The function always returns a data frame,
making it safe to use with
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
followed by
[`purrr::list_rbind()`](https://purrr.tidyverse.org/reference/list_c.html).

## Examples

``` r
extract_github_repo("https://github.com/tidyverse/ggplot2")
#>   github_user github_repo
#> 1   tidyverse     ggplot2

extract_github_repo("Bug reports at https://github.com/r-lib/vctrs/issues")
#>   github_user github_repo
#> 1       r-lib       vctrs

extract_github_repo(NA_character_)
#>   github_user github_repo
#> 1        <NA>        <NA>
```
