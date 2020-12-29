context("Misc")

data(biomass)
# scale everything dbl
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)

fit1 <- lme4::lmer(Biomass ~ Temperature + Precipitation + (1 | Population) + (1 | Year),
                           data = biomass)


set.seed(12)
x <- rnorm(1000)
test_that("CI calc is correct", {
    expect_equal(calc_CI(x, 0.95)$CI_lower, c(-1.954981), tolerance = 0.001)
    expect_equal(calc_CI(x, 0.99)$CI_lower, c(-2.493074), tolerance = 0.001)
})



