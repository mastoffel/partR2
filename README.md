
![Build
Status](https://travis-ci.org/mastoffel/partR2.svg?branch=master)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# partR2

The goal of partR2 is to provide a simple interface for commonality
analyses (CA) in mixed models. CA partitions the overall R2 of a model
into the components which are unique to each fixed effect and shared
among combinations of them, which is interesting in itself and
particularly useful in the case of multicollinearity. In addition to
these partitioned R2’s the partR2 package calculates structure
coefficients (SC). SC show the correlation between a fixed effect and
the predicted response and give an intuition of the contribution of that
fixed effect to the model prediction, independent of all other
predictors. Finally, partR2 also reports the model estimates (slopes and
variances). All estimates can be combined with parametric bootstrapping
to get confidence intervals.

partR2 is in an early phase of development and only works with lme4
models at the moment.

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
#> Loading required package: Matrix

?partR2

# load data
data(BeetlesBody)
# fit lme4 model
mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
            data = BeetlesBody)

# partition R2
R2 <- partR2(mod, partvars = c("Sex", "Treatment", "Habitat"), 
                   R2_type = "marginal", nboot = 50, CI = 0.95)
#> Registered S3 methods overwritten by 'broom.mixed':
#>   method         from 
#>   augment.lme    broom
#>   augment.merMod broom
#>   glance.lme     broom
#>   glance.merMod  broom
#>   glance.stanreg broom
#>   tidy.brmsfit   broom
#>   tidy.gamlss    broom
#>   tidy.lme       broom
#>   tidy.merMod    broom
#>   tidy.rjags     broom
#>   tidy.stanfit   broom
#>   tidy.stanreg   broom
R2
#> 
#> 
#> R2 (marginal) and CI (95%) for the full model: 
#>  R2    CI_lower CI_upper
#>  0.392 0.324    0.5     
#> 
#> ----------
#> 
#> Partitioned R2s:
#>  Predictor(s)          R2      CI_lower CI_upper
#>  Sex                   0.38584  0.318   0.49    
#>  Treatment             0.00520 -0.063   0.11    
#>  Habitat               0.00031 -0.068   0.11    
#>  Sex+Treatment         0.39119  0.323   0.50    
#>  Sex+Habitat           0.38626  0.318   0.49    
#>  Treatment+Habitat     0.00573 -0.062   0.11    
#>  Sex+Treatment+Habitat 0.39161  0.324   0.50    
#> 
#> ----------
#> 
#> Structure coefficients:
#>  Predictor    r(Yhat,x) CI_lower CI_upper
#>  SexMale      -0.7452   -0.87230 -0.6655 
#>  TreatmentExp  0.0877    0.05897  0.1377 
#>  HabitatB      0.0246   -0.00759  0.0604 
#> 
#> ----------
#> 
#> Model estimates:
#>  effect   group      term            estimate CI_lower CI_upper
#>  fixed    <NA>       (Intercept)     15.2159  14.357   15.860  
#>  fixed    <NA>       SexMale         -2.6686  -2.837   -2.489  
#>  fixed    <NA>       TreatmentExp     0.3140   0.218    0.446  
#>  fixed    <NA>       HabitatB         0.0882  -0.025    0.213  
#>  ran_pars Container  sd__(Intercept)  0.4842   0.397    0.583  
#>  ran_pars Population sd__(Intercept)  1.1742   0.679    1.525  
#>  ran_pars Residual   sd__Observation  1.0940   1.057    1.131  
#> 
#> ----------
#> 
#> Parametric bootstrapping resulted in warnings or messages:
#> Check out$boot_warnings and out$boot_messages, where out is the partR2 output object.
```

And to plot the results:

``` r
forestplot(R2, type = "R2")
```

![](README-plot-1.png)<!-- -->
