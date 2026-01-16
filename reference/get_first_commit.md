# Find the first GitHub commit introducing a symbol

Searches a GitHub repository for the earliest commit in which a given
symbol (typically a function name) is *assigned* in an R source file.
The search is performed via the GitHub API and does not require a local
clone.

## Usage

``` r
get_first_commit(
  owner,
  repo,
  function_name,
  date_only = TRUE,
  path = NULL,
  pattern = NULL,
  branch = NULL,
  max_commits = Inf
)
```

## Arguments

- owner:

  GitHub account name.

- repo:

  GitHub repository name.

- function_name:

  Name of the symbol to search for.

- date_only:

  Logical; if `TRUE` (default), return only the commit date. If `FALSE`,
  return a one-row data frame with commit metadata.

- path:

  Optional path to an R source file to search. If `NULL`, a suitable
  file is inferred automatically.

- pattern:

  Optional regular expression used to detect the symbol assignment.
  Defaults to matching `<name> <-` or `<name> =`.

- branch:

  Optional branch or ref to search. Defaults to the repository’s default
  branch.

- max_commits:

  Maximum number of commits to inspect before giving up.

## Value

If `date_only = TRUE`, a character string giving the commit date (ISO
8601), or `NULL` if no match is found. If `date_only = FALSE`, a one-row
data frame with commit metadata, or `NULL` if no match is found.

## Details

If `path` is not supplied, GitHub code search is used to identify
candidate `.R` files, and the function selects the first file whose
current contents actually assign the symbol before scanning its commit
history.
