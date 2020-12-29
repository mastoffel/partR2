context("Summary, print and forestplot")

data(biomass)
# scale data
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
# Gaussian data
mod <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
  data = biomass
)
# Only R2 with CI
(R2_1 <- partR2(mod, data = biomass, R2_type = "marginal", CI = 0.95))
# Partitioned R2
(R2_2 <- partR2(mod,
  data = biomass,
  partvars = c("SpeciesDiversity", "Temperature", "Precipitation"),
  R2_type = "marginal", nboot = 10, CI = 0.95
))


test_that("print does not throw an error", {

  expect_output(print(R2_1))
  expect_output(print(R2_2))
  expect_output(print(R2_2, round_to = 2))

})

test_that("summary does not throw an error", {

    expect_output(summary(R2_1))
    expect_output(summary(R2_2))
    expect_output(summary(R2_2, ests = TRUE))

})

test_that("Forestplot does not throw an error", {

  expect_error(forestplot(R2_1), NA)
  expect_error(forestplot(R2_2), NA)
  expect_error(forestplot(R2_2, line_size = 2, point_size = 3, text_size = 5), NA)
  expect_error(forestplot(R2_2, type = "BW"), NA)
  expect_error(forestplot(R2_2, type = "SC"), NA)
  expect_error(forestplot(R2_2, type = "IR2"), NA)
  expect_error(forestplot(R2_2, type = "Ests"), NA)

})

