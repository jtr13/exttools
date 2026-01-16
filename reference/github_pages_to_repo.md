# Convert GitHub Pages URLs to GitHub repository URLs

Transforms GitHub Pages URLs of the form
`https://<user>.github.io/<repo>/` into the corresponding GitHub
repository URLs: `https://github.com/<user>/<repo>`.

## Usage

``` r
github_pages_to_repo(url)
```

## Arguments

- url:

  A character vector of URLs. Each element may be a GitHub Pages URL or
  any other URL.

## Value

A character vector of URLs of the same length as `url`. GitHub Pages
URLs are converted to GitHub repository URLs; all other URLs are
returned as-is.

## Details

Non–GitHub Pages URLs are returned unchanged.

This function detects GitHub Pages URLs using the pattern
`https://<user>.github.io/<repo>/`. Only user-level Pages URLs that
include a repository path are converted. Root Pages URLs such as
`https://<user>.github.io/` are left unchanged, as they do not map
unambiguously to a single repository.

## Examples

``` r
github_pages_to_repo("https://hadley.github.io/httr/")
#> [1] "https://github.com/hadley/httr"

github_pages_to_repo(c(
  "https://hadley.github.io/httr/",
  "https://github.com/hadley/httr",
  "https://example.com"
))
#> [1] "https://github.com/hadley/httr" "https://github.com/hadley/httr"
#> [3] "https://example.com"           
```
