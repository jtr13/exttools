#' Convert GitHub Pages URLs to repository URLs
#'
#' @param url A URL
#' @return A GitHub repository URL
#' @export
github_pages_to_repo <- function(url) {

  is_pages <- grepl("^https://[^.]+\\.github\\.io/", url)

  url[is_pages] <- sub(
    "^https://([^.]+)\\.github\\.io/([^/]+)/?$",
    "https://github.com/\\1/\\2",
    url[is_pages]
  )

  url
}
