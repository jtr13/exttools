# create in_grammar_all.csv
# replaces exttools/scripts/classify_in_grammar_OLD.R
# and exttools/scripts/classify_imports_depends.R

library(tidyverse)
library(exttools) # install first!

cran_db <- tools::CRAN_package_db()

imports_depends_suggests <- data.frame(
  package = cran_db$Package,
  which_dep = map(cran_db$Package, which_dep_cran, cran_db) |>
    unlist()
)

packages_to_check <- imports_depends_suggests |>
  filter(!is.na(which_dep)) |>
  pull(package)


in_grammar_all <- map(packages_to_check, get_components_cran, cran_db) |>
  list_rbind()

in_grammar_all <- left_join(in_grammar_all, imports_depends_suggests)

write_csv(in_grammar_all, "~/extensions-shiny/in_grammar_all_v2026_01_24.csv")

