# December 7, 2025
# Assistance from Google Gemini
# This script creates "in_grammar_all.csv"
# See comments at the top of classify_imports_depends.R
# Adding ggplot2 even though it doesn't import or depend on ggplot2 :-)
# January 17, 2026 There are newer versions of some or maybe even all of this
# I think but not sure where, maybe in R/ or in extensions-review
# get_components.R has better ways to find in grammar components
#


# ----------------------------------------------------------------------
# LIBRARIES AND DEPENDENCY SETUP
# ----------------------------------------------------------------------
library(tidyverse)
library(data.table)

# --- INPUT DEFINITIONS ---
# Load the in-grammar list and filter it
in_grammar <- read_csv("in_out_grammar.csv") |>
  dplyr::filter(category == "in grammar")

# Define the packages to process (tail of the filtered list)
packages_to_check <- in_grammar$package

CRAN_BASE_URL <- "https://cran.r-project.org/src/contrib/"

# Safely fetch the CRAN package database once
message("Fetching CRAN package list...")
CRAN_DB <- tryCatch({
  utils::available.packages(repos = "https://cran.r-project.org/", type = "source")
}, error = function(e) {
  stop("Could not access CRAN package list. Check your internet connection or CRAN repository setting.")
})
message("CRAN list fetched successfully.")

analyze_functions_by_row_final <- function(pkg_name, pkg_db) {

  pkg_results <- data.table(
    package = character(),
    component = character(),
    fname = character(),
    dep_type = character()
  )

  # 1. Setup, Download, and Cleanup Preparation
  if (!(pkg_name %in% rownames(pkg_db))) {
    message(paste("Error: Package", pkg_name, "not found on CRAN."))
    return(pkg_results)
  }

  version <- pkg_db[pkg_name, "Version"]
  tarball_name <- paste0(pkg_name, "_", version, ".tar.gz")
  tarball_url <- paste0(CRAN_BASE_URL, tarball_name)

  temp_dir <- file.path(tempdir(), paste0("pkg_analysis_", pkg_name, "_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE)
  temp_tarball_path <- file.path(temp_dir, tarball_name)

  on.exit(unlink(temp_dir, recursive = TRUE))

  dl_status <- tryCatch({
    status <- utils::download.file(url = tarball_url, destfile = temp_tarball_path, method = "auto", quiet = TRUE, timeout = 60)
    status == 0
  }, error = function(e) {
    message(paste("Failed to download or timed out:", pkg_name))
    FALSE
  })

  if (!dl_status) {
    return(pkg_results)
  }

  # 2. UNPACK the source and read DESCRIPTION and NAMESPACE
  pkg_source_dir <- file.path(temp_dir, pkg_name)
  utils::untar(tarfile = temp_tarball_path, exdir = temp_dir)

  # --- CHECK GGPLOT2 DEPENDENCY ---
  desc_path <- file.path(pkg_source_dir, "DESCRIPTION")
  dependency_type <- NA_character_ # Default to NA

  if (file.exists(desc_path)) {
    desc_content <- readLines(desc_path, warn = FALSE)

    # Check for Imports: ggplot2 first (more common modern practice)
    if (any(str_detect(desc_content, regex("^Imports:.*ggplot2", ignore_case = TRUE)))) {
      dependency_type <- "imports"
    }
    # Check for Depends: ggplot2 second (older practice)
    else if (any(str_detect(desc_content, regex("^Depends:.*ggplot2", ignore_case = TRUE)))) {
      dependency_type <- "depends"
    }
  }

  # --- GET EXPORTED FUNCTIONS ---
  namespace_path <- file.path(pkg_source_dir, "NAMESPACE")
  exported_functions <- character(0)

  if (file.exists(namespace_path)) {
    namespace_lines <- readLines(namespace_path, warn = FALSE)
    exported_functions <- str_extract(namespace_lines, "(?<=^export\\()\\w+(?=\\))")
    exported_functions <- exported_functions[!is.na(exported_functions)]
  }

  # --- 3. CLASSIFICATION AND ROW GENERATION ---

  patterns <- list(
    "geom"       = "^geom_",
    "stat"       = "^stat_",
    "scale"      = "^scale_",
    "coordinate" = "^coord_",
    "facet"      = "^facet_",
    "theme"      = "^theme_"
  )

  new_rows <- list()

  for (component_label in names(patterns)) {
    pattern <- patterns[[component_label]]

    matching_functions <- unique(exported_functions[str_detect(exported_functions, pattern)])

    if (length(matching_functions) > 0) {
      new_rows[[component_label]] <- data.table(
        package = pkg_name,
        component = component_label,
        fname = matching_functions,
        dep_type = dependency_type # New column mapped here
      )
    }
  }

  if (length(new_rows) > 0) {
    pkg_results <- rbindlist(new_rows)
  }

  return(pkg_results)
}

# ----------------------------------------------------------------------
# EXECUTION AND FINAL OUTPUT
# ----------------------------------------------------------------------

# Initialize the final table
final_results_dt <- data.table(
  package = character(),
  component = character(),
  fname = character(),
  dep_type = character() # Initialized as character
)

# Loop through the list of "In Grammar" packages
for (pkg in packages_to_check) {
  cat(paste("Processing:", pkg, "\n"))
  classification_dt <- analyze_functions_by_row_final(pkg, CRAN_DB)

  final_results_dt <- rbindlist(list(final_results_dt, classification_dt))
}

#write_csv(final_results_dt, "data/in_grammar_all.csv")

# Problems with dep_type field, attempt to fix:
#

library(tidyverse)
df <- read_csv("in_grammar_all.csv") |>
  select(-dep_type)
crandate <- format(file.mtime("in_grammar_all.csv"), "%Y-%m-%d")

u <- url(paste0("https://packagemanager.posit.co/cran/",
                crandate, "/src/contrib/PACKAGES"))

ap <- read.dcf(u)
close(u)

cran_archive <- as.data.frame(ap)

imports_depends <- cran_archive |>
  mutate(
    depends = grepl("\\bggplot2\\b", Depends),
    imports = grepl("\\bggplot2\\b", Imports)
  ) |>
  filter(depends | imports) |>
  pivot_longer(depends:imports, names_to = "dep_type", values_to = "tf") |>
  filter(tf) |>
  dplyr::select(package = Package, dep_type)

df2 <- left_join(df, imports_depends)
write_csv(df2, "in_grammar_all.csv")

