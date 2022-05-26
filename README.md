
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- https://devguide.ropensci.org/building.html?q=testing#readme -->

# litExplor

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ransomts/litExplor/workflows/R-CMD-check/badge.svg)](https://github.com/ransomts/litExplor/actions)
[![GPL
Licence](https://badges.frapsoft.com/os/gpl/gpl.svg?v=103)](https://opensource.org/licenses/GPL-3.0/)
[![Codecov test
coverage](https://codecov.io/gh/ransomts/litExplor/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ransomts/litExplor?branch=master)
<!-- badges: end -->

litExplor is made to help researchers get an feeling for the amount of
literature already out there for a literature review.

The context that led to this package: 1. Decide to start a literature
review 2. Meet with collaborators to brainstorm search terms 3. Manually
put the same search terms into many databases - Some terms were more
fruitful than others - Some terms had almost nothing written and were
removed - Some terms dominated results and needed removed

## Installation

You can install the development version of litExplor from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ransomts/litExplor")
```

## Example

Start the dialog either from the Addins menu in rstudio or with a call
to `litExplor()`

## Screenshots

![example search screen](man/figures/search-example.png) ![example
heatmap screen](man/figures/heatmap-example.png) ![example summary
screen](man/figures/summary-example.png)

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
