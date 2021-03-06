
<!-- README.md is generated from README.Rmd. Please edit that file -->

# garchmodels

<img src="vignettes/logo-garchmodels.png" width="147" height="170" align="right" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/garchmodels)](https://CRAN.R-project.org/package=garchmodels)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/garchmodels?color=brightgreen)](https://cran.r-project.org/package=garchmodels)
![](http://cranlogs.r-pkg.org/badges/garchmodels?color=brightgreen)
[![Codecov test
coverage](https://codecov.io/gh/AlbertoAlmuinha/garchmodels/branch/master/graph/badge.svg)](https://codecov.io/gh/AlbertoAlmuinha/garchmodels?branch=master)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/AlbertoAlmuinha/garchmodels/workflows/R-CMD-check/badge.svg)](https://github.com/AlbertoAlmuinha/garchmodels/actions)
<!-- badges: end -->

> A parsnip backend for `GARCH` models in the `tidymodels` framework.

## Tutorials

-   [**Getting Started with
    Garchmodels**](https://albertoalmuinha.github.io/garchmodels/articles/getting-started.html):
    A walkthrough of the tidy modeling approach with the package.

-   [**Tuning Univariate Garch
    Models**](https://albertoalmuinha.github.io/garchmodels/articles/tuning_univariate_algorithms.html):
    Learn how to tune parameters of univariate garch models.

## Installation

CRAN version:

``` r
install.packages("garchmodels")
```

Development version:

``` r
# install.packages("devtools")
devtools::install_github("AlbertoAlmuinha/garchmodels")
```

## Why Garchmodels?

> Garchmodels unlocks univariate and multivariate GARCH models in one
> framework.

<img src="vignettes/univariate_multivariate.png" width="100%" style="display: block; margin: auto;" />

In a single framework you will be able to find what you need:

-   **Univariate Methods**: `garchmodels` connects to the `rugarch`
    package.

-   **Multivariate Methods**: `garchmodels` connects to the `rugarch`
    and `rmgarch` packages. Available methods include DCC-Garch (Dynamic
    Conditional Correlation Garch), Copula Garch and GO-Garch models.
