# create in_grammar_cran.csv
# replaces exttools/scripts/classify_in_grammar_OLD.R
# and exttools/scripts/classify_imports_depends.R

library(tidyverse)
library(exttools) # install first!

cran_db <- tools::CRAN_package_db()

imports_depends_suggests <- data.frame(
  package = cran_db$Package,
  dep_type = map(cran_db$Package, which_dep_cran, cran_db) |>
    unlist()
)

packages_to_check <- imports_depends_suggests |>
  filter(!is.na(dep_type)) |>
  pull(package)

# Manually add ggplot2
packages_to_check <-sort(c("ggplot2", packages_to_check))


in_grammar_cran <- map(packages_to_check, get_components_cran, cran_db) |>
  list_rbind()

in_grammar_cran <- left_join(in_grammar_cran, imports_depends_suggests)



write_csv(in_grammar_cran, "~/extensions-shiny/in_grammar_cran_v2026_01_25.csv")

