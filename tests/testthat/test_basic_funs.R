context("Basic functionality")

data(biomass)
library(lme4)

# scale everything dbl
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)

# Gaussian data
mod1 <- lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
             data = biomass)

R2_1 <- partR2(mod1, data = biomass)

R2_1$R2
R2_1$SC
R2_1$IR2
R2_1$BW

test_that("Full model output is correct", {
    # R2
    expect_equal(R2_1$R2$R2, 0.62, tolerance = 0.01)
    # SC
    expect_equal(R2_1$SC$SC, c(0.188,0.755,0.779,0.551,-0.195), tolerance = 0.01)
    # IR2
    expect_equal(R2_1$IR2$IR2, c(0.022,0.3534,0.3762,0.1879,0.0235), tolerance = 0.01)
    # BW
    expect_equal(R2_1$BW$estimate, c(-74.08, 0.104, 0.27, 0.40, 0.41, -0.13), tolerance = 0.01)
})
