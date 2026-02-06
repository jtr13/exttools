#' Build a lookup table of CRAN archive first appearance dates
#'
#' Constructs a data frame mapping package names to their earliest known
#' appearance in the CRAN archive, inferred from file modification times.
#'
#' @param cran_archive_db Optional result of `tools::CRAN_archive_db()`.
#'   If not supplied, it is retrieved internally.
#'
#' @return A data frame with columns:
#' * `package` – package name
#' * `archive_date` – earliest archive date (`Date`) or `NA`
#'
#' @details
#' The CRAN archive database is a named list of data frames, one per package.
#' For each package, the earliest file `mtime` is used as a proxy for the
#' archive entry date. Packages with empty archive entries return `NA`.
#'
#' This function is intended to be called once and the result reused for
#' efficient per-package lookups.
#'
#' @examples
#' \dontrun{
#' archive_db <- tools::CRAN_archive_db()
#' archive_df <- build_archive_df(archive_db)
#' }
#'
#' @export
build_archive_df <- function(cran_archive_db = NULL) {
  if (is.null(cran_archive_db)) {
    cran_archive_db <- tools::CRAN_archive_db()
  }

  data.frame(
    package = names(cran_archive_db),
    archive_date = vapply(
      cran_archive_db,
      \(x) {
        if (length(x$mtime) == 0) {
          as.Date(NA)
        } else {
          as.Date(min(x$mtime))
        }
      },
      as.Date(NA)
    )
  )
}

#' Get CRAN location and first release date for an R package
#'
#' Determines whether a package is currently on CRAN, only in the CRAN archive,
#' or in neither location, and returns the earliest known release date.
#'
#' @param pkg Character scalar. Package name.
#' @param cran_package_db A data frame returned by `tools::CRAN_package_db()`.
#' @param archive_df A data frame with columns `package` and `archive_date`,
#'   typically created by [build_archive_df()].
#' @param date_only Logical. If TRUE, return only the release date as a scalar.
#'
#' @return
#' If `date_only = FALSE` (default), a one-row data frame with columns:
#' * `package`
#' * `first_release`
#'
#' If `date_only = TRUE`, a single `Date` (or `NA`).
#'
#' @export
get_first_release <- function(pkg,
                              cran_package_db = NULL,
                              archive_df = NULL,
                              date_only = FALSE) {
  if (is.null(cran_package_db)) cran_package_db <- tools::CRAN_package_db()
  if (is.null(archive_df)) archive_df <- build_archive_df()

  idx_cran <- match(pkg, cran_package_db$Package)
  idx_arch <- match(pkg, archive_df$package)

  cran_date <- if (!is.na(idx_cran)) {
    as.Date(cran_package_db$Published[idx_cran])
  } else {
    as.Date(NA)
  }

  archive_date <- if (!is.na(idx_arch)) {
    archive_df$archive_date[idx_arch]
  } else {
    as.Date(NA)
  }

  first_release <- pmin(cran_date, archive_date, na.rm = TRUE)
  if (is.infinite(first_release)) first_release <- as.Date(NA)

  if (date_only) {
    return(first_release)
  }

  data.frame(
    package = pkg,
    first_release = first_release
  )
}
