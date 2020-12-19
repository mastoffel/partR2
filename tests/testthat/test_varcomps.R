context("Variance components")
data(biomass)
data(BeetlesMale)
sim_dat <- readRDS("sim_data.RDS")

# scale everything dbl
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)

# Gaussian data
fit1 <- lme4::lmer(Biomass ~ Year + (1 | Population),
  data = biomass
)
fit2 <- lme4::lmer(Biomass ~ Temperature + (1 | Population) + (1 | Year),
  data = biomass
)

# Poisson
data(BeetlesFemale)
BeetlesFemale$overdisp <- factor(1:nrow(BeetlesFemale))
fit3 <- lme4::glmer(Egg ~ Treatment + (1 | Container) + (1 | overdisp),
  data = BeetlesFemale, family = poisson
)

# random slope
fit4 <- lme4::lmer(y ~ time + (time | ID), sim_dat)

# two fixed effects
fit5 <- lme4::lmer(Biomass ~ Temperature + Precipitation + (1 | Population),
  data = biomass
)

# binary
fit6 <- lme4::glmer(Colour ~ Habitat + Treatment + (1|Population),
                      data = BeetlesMale, family = binomial)


#### random effect variances ####
test_that("get_ran_var gives correct variances for random intercepts", {

  # gaussian
  vc1 <- as.data.frame(lme4::VarCorr(fit1))
  vc2 <- as.data.frame(lme4::VarCorr(fit2))
  expect_equal(get_ran_var(fit1)$estimate, vc1[1, "vcov"])
  expect_equal(get_ran_var(fit2)$estimate, vc2[1:2, "vcov"])
})

test_that("get_ran_var removes overdisp effect when present", {

  # poisson with overdisp
  vc3 <- as.data.frame(lme4::VarCorr(fit3))
  expect_equal(nrow(get_ran_var(fit3, overdisp_name = "overdisp")), 1)
})

# this needs more testing
test_that("get_ran_var gives correct random slope variances", {
  expect_equal(get_ran_var(fit4)$estimate, 34.47225, tolerance = 0.001)
})


#### all variance components ####

test_that("Gaussian variance components are correct", {
  var_comps1 <- var_comps_gaussian(fit1)
  expect_equal(var_comps1$var_fix, 0.02221368, tol = 0.00001)
  expect_equal(var_comps1$var_ran, 0.2272901, tol = 0.00001)
  expect_equal(var_comps1$var_res, 0.7646179, tol = 0.00001)

  var_comps2 <- var_comps_gaussian(fit5)
  expect_equal(var_comps2$var_fix, 0.4129966, tol = 0.00001)
  expect_equal(var_comps2$var_ran, 0.2048629, tol = 0.00001)
  expect_equal(var_comps2$var_res, 0.3723833, tol = 0.00001)
})

test_that("Poisson variance components are correct", {
  var_comps1 <- var_comps_poisson(fit3,
    expct = "meanobs",
    overdisp_name = "overdisp"
  )
  expect_equal(var_comps1$var_fix, 0.06499455, tol = 0.00001)
  expect_equal(var_comps1$var_ran, 0.3229846, tol = 0.00001)
  expect_equal(var_comps1$var_res, 0.2589387, tol = 0.00001)

  var_comps2 <- var_comps_poisson(fit3,
    expct = "latent",
    overdisp_name = "overdisp"
  )
  expect_equal(var_comps2$var_res, 0.2987275, tol = 0.00001)
})

test_that("Binary variance components are correct", {

  var_comps1 <- var_comps_binary(fit6, expct = "meanobs")
  expect_equal(var_comps1$var_fix, 0.370202, tol = 0.00001)
  expect_equal(var_comps1$var_ran, 1.109169, tol = 0.00001)
  expect_equal(var_comps1$var_res, 4.086918, tol = 0.00001)

  var_comps2 <- var_comps_binary(fit6, expct = "liability")
  expect_equal(var_comps2$var_res, pi^2/3, tol = 0.00001)

  var_comps3 <- var_comps_binary(fit6, expct = "latent")
  expect_equal(var_comps3$var_res, 5.125394, tol = 0.00001)
})
