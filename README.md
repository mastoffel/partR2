
<!-- badges: start -->

![Build
Status](https://travis-ci.org/mastoffel/partR2.svg?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Codecov test
coverage](https://codecov.io/gh/mastoffel/partR2/branch/master/graph/badge.svg)](https://codecov.io/gh/mastoffel/partR2?branch=master)
<!-- badges: end -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partR2

The goal of partR2 is to estimate R<sup>2</sup> in GLMMs (sensu Nakagawa
& Schielzeth 2013) and to partition the R<sup>2</sup> into the variance
explained by the predictors.

The package takes a fitted lme4 model as input and gives you:

  - R<sup>2</sup> (marginal or conditional)
  - Semi-partial R<sup>2</sup>, the variance explained uniquely by each
    predictor and combinations of predictors
  - Inclusive R<sup>2</sup>, the variance explained by a predictor
    independent of all other predictors
  - Structure coefficients, the correlation between a predictor and the
    fitted response
  - Beta weights, standardised model estimates

All estimates can be combined with parametric bootstrapping to get
confidence intervals.

partR2 is in an early phase of development and despite a lot of testing
might still contain bugs, so be careful and thoughtful when using it.

## Installation

You can install partR2 from github with:

``` r
# install.packages("remotes")
remotes::install_github("mastoffel/partR2", build_vignettes = TRUE, dependencies = TRUE) 
# check vignette
browseVignettes("partR2")
```

## Example

``` r
library(partR2)
library(lme4)

?`partR2-package`

# load data
data(biomass)
# fit lme4 model
mod <- lmer(Biomass ~  Year + Temperature + SpeciesDiversity + (1|Population),
            data = biomass)
# R2s and partial R2s
(R2 <- partR2(mod,  partvars = c("SpeciesDiversity", "Temperature", "Year"),
              R2_type = "marginal", nboot = 100, CI = 0.95))
#> 
#> 
#> R2 (marginal) and 95% CI for the full model: 
#> # A tibble: 1 x 5
#>      R2 CI_lower CI_upper nboot   ndf
#>   <dbl>    <dbl>    <dbl> <int> <dbl>
#> 1 0.513    0.432    0.618   100     4
#> 
#> ----------
#> 
#> Part (semi-partial) R2:
#> # A tibble: 8 x 6
#>   `Predictor(s)`                       R2 CI_lower CI_upper nboot   ndf
#>   <chr>                             <dbl>    <dbl>    <dbl> <int> <dbl>
#> 1 Model                             0.513   0.432     0.618   100     4
#> 2 SpeciesDiversity                  0.165   0.084     0.270   100     3
#> 3 Temperature                       0.304   0.222     0.408   100     3
#> 4 Year                              0.013   0         0.118   100     3
#> 5 SpeciesDiversity+Temperature      0.491   0.410     0.596   100     2
#> 6 SpeciesDiversity+Year             0.178   0.0971    0.283   100     2
#> 7 Temperature+Year                  0.325   0.244     0.430   100     2
#> 8 SpeciesDiversity+Temperature+Year 0.513   0.432     0.618   100     1
```

And to plot the results:

``` r
forestplot(R2, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)
```

![](README-plot-1.png)<!-- -->

### Citation

The package is still in development, but will hopefully be uploaded to
CRAN soon.

When using it, please cite our
[preprint](https://www.biorxiv.org/content/10.1101/2020.07.26.221168v1.abstract)
for now:

Stoffel, MA, Nakagawa, S, & Schielzeth, H (2020). partR2: Partitioning
R2 in generalized linear mixed models. bioRxiv.
[![](https://img.shields.io/badge/doi-10.1101/2020.07.26.221168-green.svg)](https://doi.org/10.1101/2020.07.26.221168)
