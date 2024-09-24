
<!-- badges: start -->

[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/partR2?color=blue)](https://cran.r-project.org/package=partR2)
[![Monthly downloads
badge](https://cranlogs.r-pkg.org/badges/last-month/partR2?color=blue)](https://CRAN.R-project.org/package=partR2)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![Codecov test
coverage](https://codecov.io/gh/mastoffel/partR2/branch/master/graph/badge.svg)](https://app.codecov.io/gh/mastoffel/partR2?branch=master)

<!-- badges: end -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partR2

The goal of `partR2` is to estimate R<sup>2</sup> in GLMMs (sensu
Nakagawa & Schielzeth 2013) and to partition the R<sup>2</sup> into the
variance explained by the predictors.

The package takes a fitted lme4 model as input and gives you:

- R<sup>2</sup> (marginal or conditional)
- Part (semi-partial) R<sup>2</sup>, the variance explained uniquely by
  each predictor and combinations of predictors
- Inclusive R<sup>2</sup>, the variance explained by a predictor
  independent of all other predictors
- Structure coefficients, the correlation between a predictor and the
  fitted response
- Beta weights, standardised model estimates

All estimates can be combined with parametric bootstrapping to get
confidence intervals.

## Installation

You can install the stable version of `partR2` from CRAN with:

``` r
install.packages("partR2")
```

Or the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("mastoffel/partR2", build_vignettes = TRUE, dependencies = TRUE) 
```

Access the vignette with:

``` r
# check vignette
browseVignettes("partR2")
```

`partR2` is still in an early phase of development and might contain
bugs. If you find one, please report a minimal reproducible example in
the [issues](https://github.com/mastoffel/partR2/issues).

### Citation

When using `partR2`, please cite our
[paper](https://peerj.com/articles/11414/):

Stoffel MA, Nakagawa S, Schielzeth H. 2021. partR2: partitioning R2 in
generalized linear mixed models. *PeerJ* **9**:e11414
<https://doi.org/10.7717/peerj.11414>

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
#>  R2     CI_lower CI_upper nboot ndf
#>  0.5133 0.4383   0.5848   100   4  
#> 
#> ----------
#> 
#> Part (semi-partial) R2:
#>  Predictor(s)                      R2     CI_lower CI_upper nboot ndf
#>  Model                             0.5133 0.4383   0.5848   100   4  
#>  SpeciesDiversity                  0.1729 0.0682   0.2721   100   3  
#>  Temperature                       0.3058 0.2128   0.3917   100   3  
#>  Year                              0.0140 0.0000   0.1354   100   3  
#>  SpeciesDiversity+Temperature      0.4916 0.4147   0.5638   100   2  
#>  SpeciesDiversity+Year             0.1862 0.0827   0.2835   100   2  
#>  Temperature+Year                  0.3276 0.2364   0.4117   100   2  
#>  SpeciesDiversity+Temperature+Year 0.5133 0.4383   0.5848   100   1
```

And to plot the results:

``` r
forestplot(R2, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)
```

![](README-plot-1.png)<!-- -->
