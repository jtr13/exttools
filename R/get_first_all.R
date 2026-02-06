#' Get “first seen” dates for a symbol across GitHub + CRAN
#'
#' Computes a small one-row summary of earliest known dates across:
#' - first commit in the repo (any file),
#' - first commit mentioning/introducing `fname` (per your `get_first_commit()` logic),
#' - first GitHub release/export date for `fname` (per `get_first_export_github()`),
#' - first CRAN release date for the package (via [get_first_release()]),
#' - first CRAN version that explicitly exports `fname` (via [get_first_export_cran()]).
#'
#' All returned values are `Date` scalars (or `NA`) and the function returns a
#' one-row `data.frame`.
#'
#' @param user Character scalar. GitHub owner/user (e.g. `"tidyverse"`).
#' @param repo Character scalar. GitHub repo name (and assumed CRAN package name).
#' @param fname Character scalar. Symbol to look up (e.g. `"pivot_longer"`).
#' @param cran_db Optional `data.frame` returned by [tools::CRAN_package_db()].
#'   If `NULL`, it is retrieved internally.
#' @param archive_df Optional archive lookup `data.frame` with columns
#'   `package` and `archive_date`, typically from [build_archive_df()].
#'   If `NULL`, it is retrieved internally.
#'
#' @return A one-row `data.frame` with columns:
#' \describe{
#'   \item{first_repo}{Earliest commit date in the repository (`Date` or `NA`).}
#'   \item{first_commit}{Earliest commit date returned by [get_first_commit()] for `fname` (`Date` or `NA`).}
#'   \item{first_gh}{Earliest GitHub export/release date for `fname` per [get_first_export_github()] (`Date` or `NA`).}
#'   \item{first_release}{Earliest CRAN release date for the package per [get_first_release()] (`Date` or `NA`).}
#'   \item{first_cran}{Earliest CRAN export date for `fname` per [get_first_export_cran()] (`Date` or `NA`).}
#' }
#'
#' @details
#' This function assumes the CRAN package name matches `repo`. If that’s not
#' always true in your workflow, add a separate `pkg` argument and pass that to
#' [get_first_release()] and [get_first_export_cran()].
#'
#' @examples
#' \dontrun{
#' get_first_all("tidyverse", "tidyr", "pivot_longer")
#' }
#'
#' @export
get_first_all <- function(user, repo, fname, cran_db = NULL, archive_df = NULL) {
  if (is.null(cran_db)) cran_db <- tools::CRAN_package_db()
  if (is.null(archive_df)) archive_df <- build_archive_df()

  data.frame(
    first_repo    = get_first_repo_commit(user, repo, date_only = TRUE),
    first_release = get_first_release(repo, cran_db, archive_df, date_only = TRUE),
    first_commit  = get_first_commit(user, repo, fname, date_only = TRUE),
    first_gh      = get_first_export_github(user, repo, fname, date_only = TRUE),
    first_cran    = get_first_export_cran(repo, fname, date_only = TRUE),
    stringsAsFactors = FALSE
  )
}
