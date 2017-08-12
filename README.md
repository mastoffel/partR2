
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

This is a basic example which shows you how to solve a common problem:

``` r
library(partR2)
library(lme4)
#> Loading required package: Matrix
data(BeetlesBody)

mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
            data = BeetlesBody)

R2 <- partGaussian(mod, partvars = c("Sex", "Treatment", "Habitat"),
                   R2_type = "marginal", nboot = 50, CI = 0.95)
#> Bootstrapping progress for the full model 
#> Bootstrapping progress for the structure coefficients: 
#> Bootstrap progress for Sex 
#> Bootstrap progress for Treatment 
#> Bootstrap progress for Habitat 
#> Bootstrap progress for Sex&Treatment 
#> Bootstrap progress for Sex&Habitat 
#> Bootstrap progress for Treatment&Habitat
R2
#> 
#> 
#> R2 (marginal) and CI (95%) for the full model: 
#>  R2    CI_lower CI_upper
#>  0.392 0.274    0.502   
#> 
#> ----------
#> 
#> Unique and common R2:
#>  Predictor(s)        R2       CI_lower CI_upper
#>  Sex                 0.385849  0.266   0.493   
#>  Treatment           0.005237 -0.146   0.183   
#>  Habitat             0.000316 -0.170   0.138   
#>  Sex & Treatment     0.391196  0.273   0.502   
#>  Sex & Habitat       0.386270  0.267   0.498   
#>  Treatment & Habitat 0.005552 -0.130   0.167   
#> 
#> ----------
#> 
#> Structure coefficients:
#>  Predictor    r(Yhat,x) CI_lower CI_upper
#>  SexMale      -0.7452   -0.70998 -0.5308 
#>  TreatmentExp  0.0877    0.04267  0.1088 
#>  HabitatB      0.0246   -0.00511  0.0553
```
