
![Build
Status](https://travis-ci.org/mastoffel/partR2.svg?branch=master)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partR2

The goal of partR2 is to provide an estimate of R<sup>2</sup> in GLMMs
and to partition the R<sup>2</sup> into the variance explained by each
model component and by their combinations.

In addition to R<sup>2</sup> and partial R<sup>2</sup>’s the partR2
package calculates structure coefficients (SC). SC show the correlation
between a fixed effect and the predicted response and give an intuition
of the contribution of that fixed effect to the model prediction,
independent of all other predictors. Finally, partR2 also reports the
model estimates, i.e. the slopes and variances.

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
#>  0.613 0.538    0.682   
#> 
#> ----------
#> 
#> Partitioned R2s:
#>  Predictor(s)                      R2     CI_lower CI_upper
#>  SpeciesDiversity                  0.2552  0.180   0.325   
#>  Temperature                       0.3076  0.232   0.377   
#>  Year                              0.0017 -0.073   0.071   
#>  SpeciesDiversity+Temperature      0.6060  0.531   0.675   
#>  SpeciesDiversity+Year             0.2570  0.182   0.326   
#>  Temperature+Year                  0.3135  0.238   0.383   
#>  SpeciesDiversity+Temperature+Year 0.6129  0.538   0.682
```

And to plot the results:

``` r
forestplot(R2, type = "R2")
```

![](README-plot-1.png)<!-- -->
