
![Build
Status](https://travis-ci.org/mastoffel/partR2.svg?branch=master)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partR2

The goal of partR2 is to provide a simple interface for commonality
analyses (CA) in mixed models. CA partitions the overall R2 of a model
into the components which are unique to each fixed effect and shared
among combinations of them, which is interesting in itself and
particularly useful in the case of multicollinearity.

In addition to partial R2’s the partR2 package calculates structure
coefficients (SC). SC show the correlation between a fixed effect and
the predicted response and give an intuition of the contribution of that
fixed effect to the model prediction, independent of all other
predictors. Finally, partR2 also reports the model estimates (slopes and
variances).

All estimates can be combined with parametric bootstrapping to get
confidence intervals.

partR2 is in an early phase of development and might still contain bugs.

## Installation

You can install partR2 from github with:

``` r
# install.packages("devtools")
devtools::install_github("mastoffel/partR2")
```

## Example

``` r
library(partR2)
library(lme4)

?partR2

# load data
data(biomass)
# fit lme4 model
mod <- lmer(Biomass ~  Year + Temperature + SpeciesDiversity + (1|Population),
            data = biomass)

(R2 <- partR2(mod,  partvars = c("SpeciesDiversity", "Temperature", "Year"),
                                 R2_type = "marginal", nboot = 100, CI = 0.95))
#> 
#> 
#> R2 (marginal) and CI (95%) for the full model: 
#>  R2    CI_lower CI_upper
#>  0.613 0.543    0.679   
#> 
#> ----------
#> 
#> Partitioned R2s:
#>  Predictor(s)                      R2     CI_lower CI_upper
#>  SpeciesDiversity                  0.2552  0.185   0.321   
#>  Temperature                       0.3076  0.237   0.374   
#>  Year                              0.0017 -0.068   0.068   
#>  SpeciesDiversity+Temperature      0.6060  0.536   0.672   
#>  SpeciesDiversity+Year             0.2570  0.187   0.323   
#>  Temperature+Year                  0.3135  0.243   0.380   
#>  SpeciesDiversity+Temperature+Year 0.6129  0.543   0.679
```

And to plot the results:

``` r
forestplot(R2, type = "R2")
```

![](README-plot-1.png)<!-- -->
