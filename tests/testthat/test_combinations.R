context("Partvars/partbatch combinations")

var_set1 <- c("var1", "var2", "var3")
var_set2 <- c("var1:var2", "var3")

test_that("predictor combinations are correct", {
  expect_equal(length(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = NULL
  )), 7)
  expect_equal(length(unlist(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = NULL
  ))), 12)
  expect_equal(length(unique(unlist(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = NULL
  )))), 3)
  expect_equal(length(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = 2
  )), 6)


  expect_equal(length(make_combs(
    partvars = var_set2,
    partbatch = NULL, max_level = NULL
  )), 3)
})

batch1 <- list(b1 = c("var1", "var2"), b2 = c("var4", "var5"))
test_that("predictor combinations with partbatch are correct", {
  expect_equal(length(make_combs(
    partvars = NULL,
    partbatch = batch1, max_level = NULL
  )), 3)
  expect_equal(length(unlist(make_combs(
    partvars = NULL,
    partbatch = batch1, max_level = NULL
  ))), 8)
})


# test that full part R2 works
data(biomass)
modBM <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation +
  SpeciesDiversity + (1 | Population), data = biomass)

R2_BMc <- partR2(modBM,
  partbatch = list(c("Temperature", "Precipitation")),
  R2_type = "marginal", data = biomass, nboot = 10
)

R2_BMd <- partR2(modBM,
  partvars = c("SpeciesDiversity"),
  partbatch = list(ClimateVars = c("Temperature", "Precipitation")),
  R2_type = "marginal", data = biomass, nboot = 10
)

test_that("partbatch without partvars works", {
  expect_equal(nrow(R2_BMc$R2), 2)
  expect_equal(R2_BMc$R2$term, c("Full", "Temperature+Precipitation"))
  expect_equal(R2_BMc$R2$estimate, c(0.6005510, 0.3910285), tolerance = 0.001)
})


all_combs <- make_combs(
  partvars = c("SpeciesDiversity"),
  partbatch = list(ClimateVars = c("Temperature", "Precipitation")),
  max_level = NULL
)

