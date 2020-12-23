context("summary partR2")

data(biomass)
# scale data
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
# Gaussian data
mod <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
  data = biomass
)
# Only R2 with CI
(R2 <- partR2(mod, data = biomass, R2_type = "marginal", nboot = 15, CI = 0.95))
# Partitioned R2
(R2 <- partR2(mod,
  data = biomass,
  partvars = c("SpeciesDiversity", "Temperature", "Precipitation"),
  R2_type = "marginal", nboot = 10, CI = 0.95
))

summary(R2)

test_that("summary does not throw an error", {

    expect_output(summary(R2))

})

