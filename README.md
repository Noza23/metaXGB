
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaBoost

<!-- badges: start -->

[![R-CMD-check](https://github.com/Noza23/metaXGB/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Noza23/metaXGB/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of metaBoost is to solve computationally expensive
HyperParameter Tunning for XGBoost algorithm by a Meta Learning
approach.

## Installation

You can install the development version of metaBoost from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("git@github.com:Noza23/metaBoost.git")
```

## Instruction

As a first step, following code should be executed to set up directories
and download meta_data:

``` r
library(metaBoost)
setup_project(dir = "Directory where the Project should be set up", timeout = 300)
```

Afterwards follow the **main.R** script in the Project directory
metaXGB, created by setup_project() function.
