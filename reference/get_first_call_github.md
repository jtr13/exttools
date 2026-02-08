# Find the first commit where a function is called in a GitHub repo

Clones (or refreshes) a GitHub repository into a local cache directory,
checks out a target ref (branch or default), then scans commits (oldest
to newest) whose diffs under `dir` match a call-like pattern for `fname`
(e.g. `foo(` or `pkg::foo(`). For each candidate commit, it verifies the
call is present somewhere in the repository tree at that commit using
`git grep`.

## Usage

``` r
get_first_call_github(
  owner,
  repo,
  fname,
  date_only = FALSE,
  branch = NULL,
  max_commits = Inf,
  cache_dir = getOption("ggext.git_cache", file.path(tempdir(), "gh_repo_cache")),
  dir = "R"
)
```

## Arguments

- owner:

  Character scalar. GitHub username/organization.

- repo:

  Character scalar. GitHub repository name.

- fname:

  Character scalar. Symbol to search for (e.g. `"ggplot"`).

- date_only:

  Logical; if `TRUE`, return only the `Date` of the first verified call
  commit. Defaults to `FALSE`.

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

- dir:

  Directory within the repo to search. Defaults to `"R"`.

## Value

If a call is found:

- If `date_only = TRUE`, a `Date`.

- Otherwise, a one-row `data.frame` with columns: `package`, `fname`,
  `first_call`, `author`, `message`, `url`, `file`.

If not found (or if the repo cannot be searched), returns `NA` when
`date_only = TRUE`, otherwise `NULL`.

## Details

The return value is either the commit date (when `date_only = TRUE`) or
a one-row `data.frame` with commit metadata and a GitHub URL.

This function emits diagnostic
[`message()`](https://rdrr.io/r/base/message.html) output when it cannot
determine an answer (e.g., repository not found/inaccessible, checkout
failed, git errors) or when `fname` is never called under `dir`.

Candidate commits are obtained with `git log --reverse -G <regex>` under
`dir`, using patterns that match `fname` calls like:

- `fname[[:space:]]*\\(`

- `::fname[[:space:]]*\\(`

Each candidate is then validated by searching the repository *tree* at
that commit with `git grep -E` for the same call-like pattern.

If the local cached clone is corrupted or fetch fails, the cache
directory for that repo is deleted and recloned.

This function shares the same on-disk Git clone cache as
[`get_first_commit()`](https://jtr13.github.io/exttools/reference/get_first_commit.md)
via the `ggext.git_cache` option. Repositories cloned by either function
are reused by the other.

For best performance across sessions, set a persistent cache location:

    options(ggext.git_cache = "~/Library/Caches/ggext_git")

## See also

[`processx::run`](http://processx.r-lib.org/reference/run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
get_first_call_github("giabaio", "BCEA", "ggplot")
get_first_call_github("giabaio", "BCEA", "ggplot", date_only = TRUE)
} # }
```
