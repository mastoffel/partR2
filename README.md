
![Build Status](https://travis-ci.org/mastoffel/partR2.svg?branch=master) <!-- README.md is generated from README.Rmd. Please edit that file -->

partR2
======

The goal of partR2 is to implement methods for LMMs and GLMMs to estimate the R2 along with it's confidence intervals as well as partition the variation explained unique to and common among certain predictors, which is useful in the case of multicollinearity.

partR2 is at the beginning of its development.

Installation
------------

You can install partR2 from github with:

``` r
# install.packages("devtools")
devtools::install_github("mastoffel/partR2")
```

Example
-------

``` r
library(partR2)
library(lme4)
#> Loading required package: Matrix

?partR2

# load data
data(BeetlesBody)
# fit lme4 model
mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
            data = BeetlesBody)

# partition R2
R2 <- partR2(mod, partvars = c("Sex", "Treatment", "Habitat"), data = BeetlesBody,
                   R2_type = "marginal", nboot = 50, CI = 0.95)
R2
```
