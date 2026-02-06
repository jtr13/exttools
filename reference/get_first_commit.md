# Find the first commit that introduces a function definition in a GitHub repo

Clones (or refreshes) a GitHub repository into a local cache directory,
checks out a target ref (branch or default), then scans commits (oldest
to newest) that *touch* occurrences of `function_name` under `R/`. For
each candidate commit, it verifies the function is actually defined
(assigned via `<-` or `=`) somewhere in the tree at that commit using
`git grep`.

## Usage

``` r
get_first_commit(
  owner,
  repo,
  function_name,
  date_only = FALSE,
  pattern = NULL,
  branch = NULL,
  max_commits = Inf,
  cache_dir = getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))
)
```

## Arguments

- owner:

  Character scalar. GitHub username/organization.

- repo:

  Character scalar. GitHub repository name.

- function_name:

  Character scalar. Symbol to search for (e.g. `"geom_foo"`).

- date_only:

  Logical; if `TRUE`, return only the `Date` of the first verified
  defining commit.

- pattern:

  Optional character regex intended for verification. If `NULL`,
  defaults to a PCRE pattern approximating an assignment to
  `function_name`. Note: this argument is currently not used by the
  implementation (verification is done via `git grep -E` with an
  internally constructed ERE).

- branch:

  Optional character scalar. Ref to check out before searching. If
  `NULL`/empty, the function uses `origin/HEAD` when available,
  otherwise `HEAD`.

- max_commits:

  Maximum number of candidate commits to verify (in order). Use to cap
  runtime on very large histories. Defaults to `Inf` (no cap).

- cache_dir:

  Directory used to cache cloned repositories. Defaults to
  `getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache"))`.

## Value

If a defining commit is found:

- If `date_only = TRUE`, a `Date`.

- Otherwise, a one-row `data.frame` with columns: `first_commit`,
  `author`, `message`, `url`, `file`.

If not found (or `owner`/`repo` is `NA`), returns `NULL`.

## Details

The return value is either the commit date (when `date_only = TRUE`) or
a one-row `data.frame` with commit metadata and a GitHub URL.

Candidate commits are obtained with:

- `git log --reverse -S <function_name> <ref> -- R`

which searches for commits where the literal string `function_name`
appears in diffs under `R/` (additions or removals). Each candidate is
then validated by searching the repository *tree* at that commit with
`git grep` for an assignment-like pattern such as `foo <-` or `foo =`.

If the local cached clone is corrupted or fetch fails, the cache
directory for that repo is deleted and recloned.

## See also

[`processx::run`](http://processx.r-lib.org/reference/run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Return full metadata for first defining commit of "geom_ridgeline"
x <- get_first_commit("wilkelab", "ggridges", "geom_ridgeline")

# Date only
d <- get_first_commit("tidyverse", "ggplot2", "geom_point", date_only = TRUE)

# Limit verification work
x2 <- get_first_commit("someorg", "somerepo", "foo", max_commits = 200)
} # }
```
