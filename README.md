
![Build Status](https://travis-ci.org/mastoffel/partR2.svg?branch=master) <!-- README.md is generated from README.Rmd. Please edit that file -->

partR2
======

The goal of partR2 is to implement methods for LMMs and GLMMs to estimate the R2 along with it's confidence intervals as well as partition the variation explained unique to and common among certain predictors, which is useful in the case of multicollinearity.

partR2 is at the very beginning of its development.

Installation
------------

You can install partR2 from github with:

``` r
# install.packages("devtools")
devtools::install_github("mastoffel/partR2")
```

Example
-------

Currently, just Gaussian models are implemented.

``` r
library(partR2)
library(lme4)
#> Loading required package: Matrix
data(BeetlesBody)

mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
            data = BeetlesBody)

R2 <- partGaussian(mod, partvars = c("Sex", "Treatment", "Habitat"),
                   R2_type = "marginal", nboot = 50, CI = 0.95)
R2
```
