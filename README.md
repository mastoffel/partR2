
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
                                 R2_type = "marginal", nboot = 100, CI = 0.95,
                                 data = biomass))
#> 
#> 
#> R2 (marginal) and CI (95%) for the full model: 
#>  R2     CI_lower CI_upper
#>  0.5133 0.4225   0.63    
#> 
#> ----------
#> 
#> Partitioned R2s:
#>  Predictor(s)                      R2     CI_lower CI_upper
#>  SpeciesDiversity                  0.1653 0.0745   0.2820  
#>  Temperature                       0.3038 0.2129   0.4205  
#>  Year                              0.0130 0.0000   0.1297  
#>  SpeciesDiversity+Temperature      0.4914 0.4006   0.6081  
#>  SpeciesDiversity+Year             0.1784 0.0875   0.2951  
#>  Temperature+Year                  0.3250 0.2342   0.4417  
#>  SpeciesDiversity+Temperature+Year 0.5133 0.4225   0.6300
```

And to plot the
results:

``` r
forestplot(R2, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)
```

![](README-plot-1.png)<!-- -->
