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

# Reduced R2

# simple term
r2_1a <- R2_of_red_mod(
  partvar = "SpeciesDiversity", mod = fit1, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)
r2_1b <- R2_pe(lme4::lmer(Biomass ~ I(Temperature^2) + (1 | Population),
  data = biomass
),
expct = "meanobs", overdisp_name = "overdisp", R2_type = "marginal"
)

# modified term
r2_2a <- R2_of_red_mod(
  partvar = "I(Temperature^2)", mod = fit1, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)
r2_2b <- R2_pe(lme4::lmer(Biomass ~ SpeciesDiversity + (1 | Population),
  data = biomass
),
expct = "meanobs", overdisp_name = "overdisp", R2_type = "marginal"
)

# two terms
r2_3a <- R2_of_red_mod(
  partvar = c("SpeciesDiversity", "I(Temperature^2)"), mod = fit1, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "conditional"
)

r2_3b <- R2_pe(lme4::lmer(Biomass ~ 1 + (1 | Population), data = biomass),
  expct = "meanobs", overdisp_name = "overdisp", R2_type = "conditional"
)

# interaction
r2_4a <- R2_of_red_mod(
  partvar = c("SpeciesDiversity:I(Temperature^2)"), mod = fit2, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)
r2_4b <- R2_pe(lme4::lmer(Biomass ~ Year + SpeciesDiversity + I(Temperature^2) + (1 | Population),
  data = biomass
),
expct = "meanobs", overdisp_name = "overdisp", R2_type = "marginal"
)

# main effect which is part of interaction (not sure how legit this is)
r2_5a <- R2_of_red_mod(
  partvar = c("SpeciesDiversity"), mod = fit2, dat = biomass,
  expct = "meanobs", overdisp_name = "overdisp",
  R2_type = "marginal"
)
r2_5b <- R2_pe(lme4::lmer(Biomass ~ Year + I(Temperature^2) + SpeciesDiversity:I(Temperature^2) + (1 | Population),
  data = biomass
),
expct = "meanobs", overdisp_name = "overdisp", R2_type = "marginal"
)

test_that("Reduced model R2s are correct in R2_of_red_mod", {
  expect_equal(r2_1a, r2_1b)
  expect_equal(r2_2a, r2_2b)
  expect_equal(r2_3a, r2_3b)
  expect_equal(r2_4a, r2_4b)
  expect_equal(r2_5a, r2_5b)
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
  expect_equal(r2_8[r2_8$term == "Year+SpeciesDiversity:I(Temperature^2)", "estimate"][[1]], 0.0195, tolerance = 0.0001)
})
