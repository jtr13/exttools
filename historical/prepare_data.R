# R/prepare_data.R
# ------------------------------------------------------------------------------
# Data preparation for ggplot2 extension package analysis
#
# PURPOSE
#   Deterministic data construction only. Intended to be sourced from a cached
#   Quarto chunk. No plotting, no file output, no side effects beyond objects
#   created in the R session.
#
# INPUT FILES
#   - data/in_out_grammar_copy.csv
#   - data/in_grammar_all_copy.csv
#   - CRAN package database (snapshot by date)
#   - ggplot2 extensions gallery configuration (YAML)
#
# KEY OUTPUT OBJECTS AND COLUMNS
#
# cran_packages
#   - package
#   - dep_type            (imports | depends | suggests | NA)
#
# cran_ext_pkgs
#   - package
#   - dep_type
#   - first_release       (Date)
#
# in_out_grammar
#   - package
#   - category            ("in grammar" | "other")
#   - dep_type
#   - first_release
#   - location            ("on CRAN" | "not on CRAN")
#
# in_grammar_all
#   - package
#   - fname               (exported function name)
#   - component           (geom, stat, scale, coord, facet, theme, ...)
#   - first_release
#
# igas
#   - package
#   - first_release
#   - component
#
# scale_breakdown
#   - scale_type
#   - n_functions
#   - n_packages
#
# gal_on_cran / gal_not_cran / all_gal
#   - package
#   - url
#   - on_cran
#   - name_type           ("starts with gg" | "other names")
#   - dep_type
#   - category
#
# all_pks
#   - package
#   - category
#   - cran                (logical)
#   - gallery             (logical)
#   - starts_with_gg      (logical)
#
# CACHING NOTES
#   Safe to source inside a cached Quarto chunk. Outputs are deterministic given
#   the input files and CRAN snapshot date.
# ------------------------------------------------------------------------------

library(tidyverse)
library(exttools)
library(lubridate)
library(yaml)

# ---- Files and dates ---------------------------------------------------------

in_out_file <- "data/in_out_grammar_copy.csv"
in_grammar_all_file <- "data/in_grammar_all_copy.csv"

crandate <- format(file.mtime(in_out_file), "%Y-%m-%d")

# ---- CRAN package metadata ---------------------------------------------------

cran_db <- tools::CRAN_package_db()

pkg_url <- url(
  paste0(
    "https://packagemanager.posit.co/cran/",
    crandate,
    "/src/contrib/PACKAGES"
  )
)

cran_archive <- read.dcf(pkg_url) |>
  as.data.frame(stringsAsFactors = FALSE)

close(pkg_url)

cran_packages <- tibble(
  package = unique(cran_archive$Package)
) |>
  mutate(
    dep_type = which_dep_cran(package, cran_db = cran_archive)
  )

cran_ext_pkgs <- cran_packages |>
  filter(!is.na(dep_type))

# ---- First release dates -----------------------------------------------------

archive_df <- build_archive_df()

first_release <- map(
  cran_ext_pkgs$package,
  get_first_release,
  cran_package_db = cran_db,
  archive_df = archive_df,
  .progress = TRUE
) |>
  bind_rows()

cran_ext_pkgs <- cran_ext_pkgs |>
  left_join(first_release, by = "package")

# ---- Grammar classification -------------------------------------------------

in_out_grammar <- read_csv(in_out_file) |>
  left_join(cran_ext_pkgs, by = "package") |>
  filter(category != "error")

# ---- Grammar components ------------------------------------------------------

in_grammar_all <- read_csv(in_grammar_all_file) |>
  left_join(first_release, by = "package")

igas <- in_grammar_all |>
  distinct(package, first_release, component)

# ---- Scale breakdown ---------------------------------------------------------

scale_breakdown <- in_grammar_all |>
  filter(component == "scale") |>
  mutate(scale_type = str_extract(fname, "^scale_[^_]+")) |>
  summarize(
    n_functions = n(),
    n_packages = n_distinct(package),
    .by = scale_type
  ) |>
  arrange(desc(n_functions))

# ---- Gallery data ------------------------------------------------------------

cfg <- yaml.load(
  readLines(
    "https://raw.githubusercontent.com/ggplot2-exts/gallery/refs/heads/gh-pages/_config.yml"
  )
)

widgets <- cfg[["widgets"]]

gal <- tibble(
  package = map_chr(widgets, \(x) x$name),
  url     = map_chr(widgets, \(x) x$url)
) |>
  filter(package != "ggterror") |>
  mutate(
    on_cran = package %in% cran_db$Package,
    name_type = if_else(
      str_detect(package, "^[Gg][Gg]"),
      "starts with gg",
      "other names"
    )
  )

gal_on_cran <- gal |>
  filter(on_cran) |>
  mutate(
    dep_type = map_chr(package, which_dep_cran, cran_db = cran_db)
  ) |>
  left_join(
    in_out_grammar |> select(package, category),
    by = "package"
  )

idx <- gal_on_cran$dep_type == "suggests"
gal_on_cran$category[idx] <- vapply(
  gal_on_cran$package[idx],
  in_grammar_check_cran,
  character(1),
  cran_db = cran_db
)

gal_not_cran <- gal |>
  filter(!on_cran) |>
  mutate(
    repo_url = github_pages_to_repo(url),
    category = map2_chr(package, repo_url, in_grammar_check_github),
    dep_type = map_chr(repo_url, which_dep_github)
  )

gal <- gal |> filter(package != "ggterror")

gal$url[gal$package == "ggasym"] <- "https://jhrcook.github.io/ggasym/"

gal$url[gal$package == "ggautothemes"] <- "https://github.com/lukastay/ggautothemes"

# clean up
gal_not_cran$repo_url[gal_not_cran$package == "ggseas"] <- "https://github.com/ellisp/ggseas/tree/master/pkg"

gal_not_cran$repo_url[gal_not_cran$package == "econocharts"] <- "https://github.com/R-CoderDotCom/econocharts"

gal_not_cran$repo_url[gal_not_cran$package == "ichimoku"] <- "https://github.com/shikokuchuo/ichimoku"

all_gal <- bind_rows(
  gal_on_cran,
  gal_not_cran |> select(-repo_url)
)

# ---- Combined package universe ----------------------------------------------

in_out_grammar2 <- in_out_grammar |>
  filter(location == "on CRAN") |>
  select(package, category) |>
  mutate(cran = TRUE, gallery = package %in% all_gal$package)

gal_not_cran2 <- gal_not_cran |>
  select(package, category) |>
  mutate(cran = FALSE, gallery = TRUE)

all_pks <- bind_rows(in_out_grammar2, gal_not_cran2) |>
  mutate(
    starts_with_gg = str_detect(package, "^[Gg][Gg]")
  )

