context("Full partR2")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)

fit1 <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation * SpeciesDiversity + (1 | Population),
  data = biomass
)
fit2 <- suppressWarnings(lme4::lmer(Biomass ~ Precipitation + SpeciesDiversity + (SpeciesDiversity | Population),
  data = biomass
))


test_that("partR2 works for some special cases", {

  # make sure that partR2 for simple interaction works
  r2_1 <- partR2(fit1,
    partvars = c("Year", "Precipitation:SpeciesDiversity"),
    data = biomass
  )
  expect_equal(nrow(r2_1$R2), 4)
  expect_equal(r2_1$R2$estimate, c(0.599, 0.0125, 0, 0.0124), tolerance = 0.001)

  # partR2 works for random slope models if fixed effect which is part
  # of random slope is not in partvars
  expect_error(suppressWarnings(partR2(fit2, partvars = "Precipitation", data = biomass), NA))
  # and throws error if it is
  expect_error(partR2(fit2,
    partvars = "SpeciesDiversity",
    data = biomass
  ))
})

# to be continued...
