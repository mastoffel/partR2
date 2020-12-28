context("part R2 calculation")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
fit1 <- lme4::lmer(Biomass ~  SpeciesDiversity + I(Temperature^2) + (1 | Population),
                  data = biomass)
fit2 <- lme4::lmer(Biomass ~  Year + SpeciesDiversity * I(Temperature^2) + (1 | Population),
                   data = biomass)

# R2 point estimate
r2_marg <- R2_pe(fit1, expct = "meanobs", overdisp_name = "overdisp",
                 R2_type = "marginal")
r2_cond <- R2_pe(fit1, expct = "meanobs", overdisp_name = "overdisp",
                 R2_type = "conditional")

test_that("R2 marginal and conditional are correct", {
    expect_equal(r2_marg$R2, 0.200, tolerance = 0.001)
    expect_equal(r2_cond$R2, 0.229, tolerance = 0.001)
})

# Reduced R2

# simple term
r2_1a <- R2_of_red_mod(partvar = "SpeciesDiversity", mod = fit1, dat = biomass,
              expct = "meanobs", overdisp_name = "overdisp",
              R2_type = "marginal")
r2_1b <-R2_pe(lme4::lmer(Biomass ~ I(Temperature^2) + (1 | Population),
                         data = biomass),
              expct = "meanobs", overdisp_name = "overdisp", R2_type = "marginal")

# modified term
r2_2a <- R2_of_red_mod(partvar = "I(Temperature^2)", mod = fit1, dat = biomass,
                      expct = "meanobs", overdisp_name = "overdisp",
                      R2_type = "marginal")
r2_2b <- R2_pe(lme4::lmer(Biomass ~ SpeciesDiversity + (1 | Population),
                          data = biomass),
               expct = "meanobs", overdisp_name = "overdisp",R2_type = "marginal")

# two terms
r2_3a <- R2_of_red_mod(partvar = c("SpeciesDiversity","I(Temperature^2)"), mod = fit1, dat = biomass,
                       expct = "meanobs", overdisp_name = "overdisp",
                       R2_type = "conditional")

r2_3b <- R2_pe(lme4::lmer(Biomass ~ 1 + (1 | Population), data = biomass),
               expct = "meanobs", overdisp_name = "overdisp",R2_type = "conditional")

# interaction
r2_4a <- R2_of_red_mod(partvar = c("SpeciesDiversity:I(Temperature^2)"), mod = fit2, dat = biomass,
                       expct = "meanobs", overdisp_name = "overdisp",
                       R2_type = "marginal")
r2_4b <- R2_pe(lme4::lmer(Biomass ~  Year + SpeciesDiversity + I(Temperature^2) + (1 | Population),
                          data = biomass),
               expct = "meanobs", overdisp_name = "overdisp",R2_type = "marginal")

# main effect which is part of interaction (not sure how legit this is)
r2_5a <- R2_of_red_mod(partvar = c("SpeciesDiversity"), mod = fit2, dat = biomass,
                       expct = "meanobs", overdisp_name = "overdisp",
                       R2_type = "marginal")
r2_5b <- R2_pe(lme4::lmer(Biomass ~  Year + I(Temperature^2) + SpeciesDiversity:I(Temperature^2) + (1 | Population),
                          data = biomass),
               expct = "meanobs", overdisp_name = "overdisp",R2_type = "marginal")

test_that("Reduced model R2s are correct in R2_of_red_mod", {
    expect_equal(r2_1a, r2_1b)
    expect_equal(r2_2a, r2_2b)
    expect_equal(r2_3a, r2_3b)
    expect_equal(r2_4a, r2_4b)
    expect_equal(r2_5a, r2_5b)
})

