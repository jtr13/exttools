# December 6, 2025
# Assistance from Google Gemini
# This script creates "in_out_grammar.csv"
# It does not capture the actual functions
# so I created the classify_in_grammar.R script to
# go back and get the functions for the packages with
# in grammar components only
#
# Ideally this should be done in one pass but it takes several
# hours to go through 4000+ packages (but it could be done )

# ----------------------------------------------------------------------
library(stringr)
library(data.table)

cran_db <- tools::CRAN_package_db()

imports_depends <- cran_db |>
  filter(grepl("\\bggplot2\\b", Depends) | grepl("\\bggplot2\\b", Imports))

packages_to_check <- imports_depends$Package
CRAN_BASE_URL <- "https://cran.r-project.org/src/contrib/"

# Safely fetch the CRAN package database once
message("Fetching CRAN package list...")
CRAN_DB <- tryCatch({
  utils::available.packages(repos = "https://cran.r-project.org/", type = "source")
}, error = function(e) {
  stop("Could not access CRAN package list. Check your internet connection or CRAN repository setting.")
})
message("CRAN list fetched successfully.")

# ----------------------------------------------------------------------
# STEP 1: Core Analysis Function (Collects All In-Grammar Components)
# ----------------------------------------------------------------------

classify_in_grammar_multiple <- function(pkg_name, pkg_db) {

  # 1. Package existence check and URL construction
  if (!(pkg_name %in% rownames(pkg_db))) {
    return(list(Category = "Error", Component = NA_character_, Note = "Package not found on CRAN."))
  }

  version <- pkg_db[pkg_name, "Version"]
  tarball_name <- paste0(pkg_name, "_", version, ".tar.gz")
  tarball_url <- paste0(CRAN_BASE_URL, tarball_name)

  # 2. Setup Temporary Directories, Download, and Cleanup Preparation
  temp_dir <- file.path(tempdir(), paste0("pkg_analysis_", pkg_name, "_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE)
  temp_tarball_path <- file.path(temp_dir, tarball_name)

  # Set up guaranteed cleanup for the temporary directory even if an error occurs
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Attempt download using Base R's download.file with a timeout
  dl_status <- tryCatch({
    dl_method <- "auto"

    # download.file returns 0 on success, non-zero on error
    status <- utils::download.file(
      url = tarball_url,
      destfile = temp_tarball_path,
      method = dl_method,
      quiet = TRUE,
      timeout = 60 # Using Base R's built-in timeout
    )
    status == 0
  }, error = function(e) {
    message(paste("Failed to download or timed out:", pkg_name, "Error:", e$message))
    FALSE
  })

  if (!dl_status) {
    return(list(Category = "error", Component = NA_character_, Note = "Download failed or timed out"))
  }

  # 3. UNPACK the package source and find NAMESPACE file
  pkg_source_dir <- file.path(temp_dir, pkg_name)
  utils::untar(tarfile = temp_tarball_path, exdir = temp_dir)

  namespace_path <- file.path(pkg_source_dir, "NAMESPACE")
  exported_functions <- character(0)

  if (file.exists(namespace_path)) {
    namespace_lines <- readLines(namespace_path, warn = FALSE)
    # Regex to extract the function name from 'export(function_name)'
    exported_functions <- str_extract(namespace_lines, "(?<=^export\\()\\w+(?=\\))")
    exported_functions <- exported_functions[!is.na(exported_functions)]
  }

  # --- 4. CLASSIFICATION LOGIC (COLLECT ALL MATCHES) ---

  components_found <- character(0)

  # 1. LAYER EXTENSIONS (Geom/Stat)
  if (any(str_detect(exported_functions, "^(geom_|stat_)"))) {
    components_found <- c(components_found, "Layer (Geom/Stat)")
  }

  # 2. SCALES
  if (any(str_detect(exported_functions, "^scale_"))) {
    components_found <- c(components_found, "Scale")
  }

  # 3. COORDS, FACETS, THEMES
  if (any(str_detect(exported_functions, "^coord_"))) {
    components_found <- c(components_found, "Coordinate")
  }
  if (any(str_detect(exported_functions, "^facet_"))) {
    components_found <- c(components_found, "Facet")
  }
  if (any(str_detect(exported_functions, "^theme_"))) {
    components_found <- c(components_found, "Theme")
  }

  # --- 5. REPORT RESULTS ---

  if (length(components_found) > 0) {
    # Combine all found components into a single comma-separated string
    component_string <- paste(components_found, collapse = ", ")
    return(list(Category = "in grammar", Component = component_string))
  } else {
    # If nothing was found
    return(list(Category = "other", Component = NA))
  }
}

# ----------------------------------------------------------------------
# STEP 2: Execution and Output
# ----------------------------------------------------------------------

results_dt <- data.table(
  package = character(),
  category = character(),
  component = character(),
  stringsAsFactors = FALSE
)

# Loop through the list
for (pkg in packages_to_check) {
  cat(paste("Processing:", pkg, "\n"))

  classification <- classify_in_grammar_multiple(pkg, CRAN_DB)

  results_dt <- rbindlist(list(results_dt, data.table(
    package = pkg,
    category = classification$Category,
    component = classification$Component
  )))
}

# Save the results
# write_csv(results_dt, "in_out_grammar.csv")
# write_csv(results_dt, "in_out_grammar_backup.csv")

# Next see cleanup.R
