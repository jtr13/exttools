# create in_grammar_all.csv
# replaces exttools/scripts/classify_in_grammar_OLD.R
# and exttools/scripts/classify_imports_depends.R

library(tidyverse)

cran_db <- tools::CRAN_package_db()

imports_depends_suggests <- data.frame(
  package = cran_db$Package,
  which_dep = map(cran_db$Package, which_dep_cran, cran_db) |>
    unlist()
)

packages_to_check <- imports_depends_suggests |>
  filter(!is.na(which_dep)) |>
  pull(package)


in_out_grammar <- map(packages_to_check, get_components_cran, cran_db) |>
  list_rbind()

write_csv(in_out_grammar, "in_out_grammar_v2025_12_07_b.csv")

