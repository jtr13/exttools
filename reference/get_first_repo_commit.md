# Find the first commit in a GitHub repository

Finds the earliest (root) commit in a GitHub repository using the GitHub
REST API. The function does not require a local clone and queries GitHub
directly via the commits endpoint.

## Usage

``` r
get_first_repo_commit(owner, repo, sha = NULL, date_only = FALSE)
```

## Arguments

- owner:

  Character. GitHub repository owner.

- repo:

  Character. GitHub repository name.

- sha:

  Optional character. Commit SHA or branch name to start from. Defaults
  to the repository's default branch.

- date_only:

  Logical. If TRUE, return only the commit date as a Date scalar.

## Value

When date_only = TRUE, a Date scalar giving the date of the first
commit, or NA if the repository is not found.

When date_only = FALSE, a one-row data frame with columns:

- first_repo:

  Date of the first commit

- author:

  Commit author name

- message:

  Commit message

- url:

  URL of the commit on GitHub

or NULL if the repository is not found.

## Details

Pagination is handled implicitly: the function follows the Link header
to retrieve the final page of commits and returns the oldest commit
listed.

If the repository does not exist, is inaccessible, or the request fails:

- Returns NA when date_only = TRUE

- Returns NULL otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
get_first_repo_commit("tidyverse", "ggplot2")
get_first_repo_commit("tidyverse", "ggplot2", date_only = TRUE)
} # }
```
