# Find the first commit in a GitHub repository

Finds the earliest (root) commit in a GitHub repository using the GitHub
REST API. Does not require a local clone.

## Usage

``` r
get_first_repo_commit(owner, repo, sha = NULL)
```

## Arguments

- owner:

  GitHub repository owner.

- repo:

  GitHub repository name.

- sha:

  Optional commit SHA or branch name to start from. Defaults to the
  repository’s default branch.

## Value

A one-row data frame with commit SHA, date, author, message, and URL.

## Details

The function follows pagination links to retrieve the oldest commit
reachable from the specified ref.

## Examples

``` r
if (FALSE) { # \dontrun{
get_first_repo_commit("tidyverse", "ggplot2")
} # }
```
