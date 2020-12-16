context("Variance components")
library(lme4)
data(biomass)
data(sim_dat)


# scale everything dbl
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)

# Gaussian data
fit1 <- lmer(Biomass ~ Year + (1 | Population),
  data = biomass
)
fit2 <- lmer(Biomass ~ Temperature + (1 | Population) + (1 | Year),
  data = biomass
)

# Poisson
data(BeetlesFemale)
BeetlesFemale$overdisp <- factor(1:nrow(BeetlesFemale))
fit3 <- glmer(Egg ~ Treatment + (1 | Container) + (1 | overdisp),
  data = BeetlesFemale, family = poisson
)

# random slope
fit4 <- lmer(y~time+(time|ID), sim_dat)

test_that("get_ran_var gives correct variances for random intercepts", {

  # gaussian
  vc1 <- as.data.frame(VarCorr(fit1))
  vc2 <- as.data.frame(VarCorr(fit2))
  expect_equal(get_ran_var(fit1)$estimate, vc1[1, "vcov"])
  expect_equal(get_ran_var(fit2)$estimate, vc2[1:2, "vcov"])
})

test_that("get_ran_var removes overdisp effect when present", {

  # poisson with overdisp
  vc3 <- as.data.frame(VarCorr(fit3))
  expect_equal(nrow(get_ran_var(fit3, overdisp_name = "overdisp")), 1)
})

# this needs more testing
test_that("get_ran_var gives correct random slope variances", {

    expect_equal(get_ran_var(fit4)$estimate, 25.40946, tolerance = 0.001)


})
