#' Get CRAN location and first release date for an R package
#'
#' Determines whether a package is currently on CRAN, only in the CRAN archive,
#' or in neither location, and returns the earliest known release date.
#'
#' @param pkg Character scalar. Package name.
#' @param cran_package_db Optional result of `tools::CRAN_package_db()`.
#'   Recreated internally if `NULL`.
#' @param cran_archive_db Optional result of `tools::CRAN_archive_db()`.
#'   Recreated internally if `NULL`.
#'
#' @return A one-row tibble with columns:
#' * `package` – package name
#' * `location` – `"on CRAN"`, `"CRAN archive only"`, or `"neither"`
#' * `first_release` – earliest known release date (`Date`) or `NA`
#'
#' @details
#' Archive dates are inferred from the earliest file `mtime` in the CRAN
#' archive. Empty archive entries are treated as missing. When both CRAN and
#' archive dates are available, the earlier of the two is returned.
#'
#' @examples
#' \dontrun{
#' get_first_release("ggplot2")
#' get_first_release("thispackagedoesnotexist")
#' }
#' @export
get_first_release <- function(pkg,
                              cran_package_db = NULL,
                              cran_archive_db = NULL) {
  if (is.null(cran_package_db)) {
    cran_package_db <- tools::CRAN_package_db()
  }

  if (is.null(cran_archive_db)) {
    cran_archive_db <- tools::CRAN_archive_db()
  }

  archive_df <- data.frame(
    package = names(cran_archive_db),
    archive_date = vapply(
      cran_archive_db,
      \(x) if (length(x$mtime) == 0) NA else as.Date(min(x$mtime)),
      as.Date(NA)
    ),
    stringsAsFactors = FALSE
  )

  on_cran    <- pkg %in% cran_package_db$Package
  in_archive <- pkg %in% archive_df$package

  location <- if (on_cran) {
    "on CRAN"
  } else if (in_archive) {
    "CRAN archive only"
  } else {
    "neither"
  }


  # CRAN publish date
  idx <- match(pkg, cran_package_db$Package)
  cran_date <- if (is.na(idx)) {
    NA
  } else {
    as.Date(cran_package_db$Published[idx])
  }

  # Archive date
  idx <- match(pkg, archive_df$package)
  archive_date <- if (is.na(idx)) {
    NA
  } else {
    archive_df$archive_date[idx]
  }

  first_release <- pmin(cran_date, archive_date, na.rm = TRUE)
  if (is.infinite(first_release)) first_release <- NA

  data.frame(
    package = pkg,
    location = location,
    first_release = first_release
  )
}

