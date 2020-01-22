context("Simple models")

data(biomass)
library(lme4)

# Gaussian data
mod1 <- lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
             data = biomass)

set.seed(923)
# only R2 pe
r2_mod1_1 <- partR2(mod1, data = biomass)
r2_mod1_2 <- partR2(mod1, nboot = 10, data = biomass)
r2_mod1_3 <- partR2(mod1, data = biomass, partvars = c("SpeciesDiversity"))
r2_mod1_4 <- partR2(mod1, data = biomass, partvars = c("SpeciesDiversity", "Year"))
r2_mod1_5 <- partR2(mod1, data = biomass, partvars = c("Temperature:Precipitation"))
r2_mod1_6 <- partR2(mod1, data = biomass, partvars = c("Temperature*Precipitation",
                                       "Year", "SpeciesDiversity"))
r2_mod1_7 <-  partR2(mod1, data = biomass, nboot = 4, parallel = TRUE)

#r2_mod1_6 <- partR2(mod1, partvars = c("Temperature*Precipitation"))
test_that("Gaussian models with increasing complexity do not throw errors", {
    # pe
    expect_equal(r2_mod1_1$R2_pe_ci$R2, 0.729, tolerance = 0.01)
    # ci
    expect_equal(r2_mod1_2$R2_pe_ci$CI_lower, 0.696, tolerance = 0.01)
    expect_equal(r2_mod1_2$R2_pe_ci$CI_upper, 0.771, tolerance = 0.01)
    # R2 one partvar
    expect_equal(r2_mod1_3$R2_pe_ci$R2, c(0.729, 0.262), tolerance = 0.01)
    # R2 two partvars
    expect_equal(r2_mod1_4$R2_pe_ci$R2, c(0.729, 0.262, 0.006, 0.267),
                 tolerance = 0.01)
    # interaction only
    expect_equal(r2_mod1_5$R2_pe_ci$R2, c(0.729, 0.031), tolerance = 0.01)
    # all together same as marginal
    expect_equal(r2_mod1_6$R2_pe_ci$R2[length(r2_mod1_6$R2_pe_ci$R2)],
                 r2_mod1_6$R2_pe_ci$R2[1], tolerance = 0.001)
    # parallel

})


