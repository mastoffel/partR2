context("Calculating R2 and part R2")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
fit1 <- lme4::lmer(Biomass ~ SpeciesDiversity + I(Temperature^2) + (1 | Population),
  data = biomass
)
fit2 <- lme4::lmer(Biomass ~ Year + SpeciesDiversity * I(Temperature^2) + (1 | Population),
  data = biomass
)


# R2 point estimate

r2_marg <- R2_pe(fit1,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)
r2_cond <- R2_pe(fit1,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "conditional"
)

test_that("R2 marginal and conditional are correct", {
  expect_equal(r2_marg$R2, 0.200, tolerance = 0.001)
  expect_equal(r2_cond$R2, 0.229, tolerance = 0.001)
})

# Reduced fixed effect variances

# simple term
fix_1a <- fixvar_of_red_mod(
  partvar = "SpeciesDiversity", mod = fit1, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)[[1]]
fix_1b <- var(stats::predict(lme4::lmer(Biomass ~ I(Temperature^2) + (1 | Population),
  data = biomass), re.form = NA))

# modified term
fix_2a <- fixvar_of_red_mod(
  partvar = "I(Temperature^2)", mod = fit1, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)[[1]]
fix_2b <- var(stats::predict(lme4::lmer(Biomass ~ SpeciesDiversity + (1 | Population),
                                       data = biomass), re.form = NA))

# two terms
fix_3a <- fixvar_of_red_mod(
  partvar = c("SpeciesDiversity", "I(Temperature^2)"), mod = fit1, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "conditional"
)[[1]]

fix_3b <- var(stats::predict(lme4::lmer(Biomass ~ 1 + (1 | Population), data = biomass),
                        re.form = NA))

# interaction
fix_4a <- fixvar_of_red_mod(
  partvar = c("SpeciesDiversity:I(Temperature^2)"), mod = fit2, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)[[1]]
fix_4b <- var(stats::predict(lme4::lmer(Biomass ~ Year + SpeciesDiversity + I(Temperature^2) + (1 | Population),
  data = biomass), re.form = NA))

# main effect which is part of interaction (not sure how legit this is)
fix_5a <- fixvar_of_red_mod(
  partvar = c("SpeciesDiversity"), mod = fit2, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)[[1]]
fix_5b <- var(stats::predict(lme4::lmer(Biomass ~ Year + I(Temperature^2) + SpeciesDiversity:I(Temperature^2) + (1 | Population),
                                       data = biomass), re.form = NA))

test_that("Reduced model R2s are correct in R2_of_red_mod", {
  expect_equal(fix_1a, fix_1b)
  expect_equal(fix_2a, fix_2b)
  expect_equal(fix_3a, fix_3b)
  expect_equal(fix_4a, fix_4b)
  expect_equal(fix_5a, fix_5b)
})



# part R2s
all_combs1 <- make_combs(
  partvars = c("Year"),
  partbatch = NULL, max_level = NULL
)
all_combs2 <- make_combs(
  partvars = c("Year", "SpeciesDiversity"),
  partbatch = NULL, max_level = NULL
)
all_combs3 <- make_combs(
  partvars = c(
    "Year", "SpeciesDiversity",
    "I(Temperature^2)",
    "SpeciesDiversity:I(Temperature^2)"
  ),
  partbatch = NULL, max_level = NULL
)

r2_6 <- part_R2s(fit2,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal", data_mod = biomass, partition = TRUE,
  allow_neg_r2 = FALSE, all_comb = all_combs1
)
r2_7 <- part_R2s(fit2,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal", data_mod = biomass, partition = TRUE,
  allow_neg_r2 = FALSE, all_comb = all_combs2
)
r2_8 <- part_R2s(fit2,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal", data_mod = biomass, partition = TRUE,
  allow_neg_r2 = FALSE, all_comb = all_combs3
)

test_that("part R2s are correct with part_R2s()", {
  expect_equal(r2_6$term, c("Full", "Year"))
  expect_equal(r2_6$estimate, c(0.219, 0.0200), tolerance = 0.001)

  expect_equal(nrow(r2_7), 4)
  expect_equal(sum(r2_7$term %in% "Year+SpeciesDiversity"), 1)

  # some random checks
  expect_equal(nrow(r2_8), 16)
  expect_equal(sum(r2_8$term %in% "SpeciesDiversity:I(Temperature^2)"), 1)
  expect_equal(r2_8[r2_8$term == "Year+SpeciesDiversity:I(Temperature^2)", "estimate"][[1]], 0.0210613, tolerance = 0.0001)
})
