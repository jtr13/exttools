# exttools

Tools for analyzing ggplot2-related extension packages on CRAN and GitHub.

## What this package does
- Detects ggplot2 dependency type (Depends / Imports / Suggests)
- Extracts grammar-of-graphics components from package sources
- Classifies packages as “in grammar” or “other”

## Example

```r
which_dep_cran("cowplot")
get_components_cran("cowplot")

in_grammar_github_check(
  "ggseas",
  "https://github.com/ellisp/ggseas/tree/master/pkg"
)
```
