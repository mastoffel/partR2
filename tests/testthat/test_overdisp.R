context("Overdispersion")
data("biomass")

# biomass$overdisp <- 1:nrow(biomass)
biomass$olre <- 1:nrow(biomass)

biomass$YearC <- biomass$Year - mean(biomass$Year)
biomass$TemperatureS <- scale(biomass$Temperature)
biomass$PrecipitationS <- scale(biomass$Precipitation)
biomass$SpeciesDiversityC <- biomass$SpeciesDiversity - mean(biomass$SpeciesDiversity)

fit1 <- lme4::glmer(Extinction ~ YearC + TemperatureS + PrecipitationS +
  SpeciesDiversityC + (1 | Population),
data = biomass, family = "poisson"
)

fit2 <- lme4::glmer(Extinction ~ YearC + TemperatureS + PrecipitationS +
  SpeciesDiversityC + (1 | Population) + (1 | olre),
data = biomass, family = "poisson"
)

test_that("overdispersion is added", {
  fit_olre <- model_overdisp(fit1, biomass, olre = TRUE)
  expect_equal(fit_olre$mod@frame$overdisp, factor(1:nrow(biomass)))
  expect_equal(fit_olre$dat$overdisp, factor(1:nrow(biomass)))

  fit_noolre <- model_overdisp(fit1, biomass, olre = FALSE)
  expect_equal(fit_noolre$mod@frame$overdisp, NULL)
})


test_that("overdispersion is not added when present", {
  fit_olre <- model_overdisp(fit2, biomass, olre = TRUE)
  expect_equal(fit_olre$mod@frame$overdisp, NULL)
})
