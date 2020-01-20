context("Simple models")

data(biomass)
library(lme4)

# Gaussian data
mod1 <- lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
             data = biomass)

set.seed(923)
# only R2 pe
r2_mod1_1 <- partR2(mod1)
r2_mod1_2 <- partR2(mod1, nboot = 10)
r2_mod1_3 <- partR2(mod1, partvars = c("SpeciesDiversity"))
r2_mod1_4 <- partR2(mod1, partvars = c("SpeciesDiversity", "Year"))
r2_mod1_5 <- partR2(mod1, partvars = c("Temperature:Precipitation"))
r2_mod1_6 <- partR2(mod1, partvars = c("Temperature*Precipitation",
                                       "Year", "SpeciesDiversity"))

#r2_mod1_6 <- partR2(mod1, partvars = c("Temperature*Precipitation"))
test_that("Gaussian models with increasing complexity do not throw errors", {
    # pe
    expect_equal(r2_mod1_1$R2_pe_ci$R2, 0.729, tolerance = 0.01)
    # ci
    expect_equal(r2_mod1_2$R2_pe_ci$CI_lower, 0.696, tolerance = 0.01)
    expect_equal(r2_mod1_2$R2_pe_ci$CI_upper, 0.771, tolerance = 0.01)
    # R2 one partvar
    expect_equal(r2_mod1_3$R2_pe_ci$R2, c(0.729, 0.262), tolerance = 0.01)
    # R2 two partvars
    expect_equal(r2_mod1_4$R2_pe_ci$R2, c(0.729, 0.262, 0.006, 0.267),
                 tolerance = 0.01)
    # interaction only
    expect_equal(r2_mod1_5$R2_pe_ci$R2, c(0.729, 0.031), tolerance = 0.01)
    # all together same as marginal
    expect_equal(r2_mod1_6$R2_pe_ci$R2[length(r2_mod1_6$R2_pe_ci$R2)],
                 r2_mod1_6$R2_pe_ci$R2[1], tolerance = 0.001)

})

# data(BeetlesFemale)
# #BeetlesFemale$overdisp <- factor(1:nrow(BeetlesFemale))
# mod2 <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population),
#               data = BeetlesFemale, family = poisson)
#
# test_that("Poisson models with increasing complexity do not throw errors", {
#     expect_error(partR2(mod2), NA)
#     expect_error(partR2(mod2, nboot = 2), NA)
#     expect_error(partR2(mod2, partvars = c("Treatment")), NA)
#     expect_error(partR2(mod2, partvars = c("Treatment"), nboot = 2), NA)
#     expect_error(partR2(mod2, partvars = c("Habitat", "Treatment")), NA)
#     expect_error(partR2(mod2, partvars = c("Habitat", "Treatment"), nboot = 2), NA)
# })
#
# data(BeetlesMale)
# mod <- glmer(Colour ~ Treatment + (1|Container) + (1|Population),
#               data = BeetlesMale, family = "binomial")
#
#
# data(BeetlesMale)
# BeetlesMale$Dark <- BeetlesMale$Colour
# BeetlesMale$Reddish <- (BeetlesMale$Colour-1)*-1
# BeetlesColour <- aggregate(cbind(Dark, Reddish) ~ Treatment + Population + Container,
#       data=BeetlesMale, FUN=sum)
#
# mod3 <- glmer(cbind(Dark,Reddish) ~ Treatment + (1|Population),
#        data = BeetlesColour, family = binomial)
#
# test_that("Binomial (proportion) models with increasing complexity do not throw errors", {
#     expect_error(partR2(mod3), NA)
#     expect_error(partR2(mod3, nboot = 2), NA)
#     expect_error(partR2(mod3, partvars = c("Treatment")), NA)
#     expect_error(partR2(mod3, partvars = c("Treatment"), nboot = 2), NA)
# })


# mod4 <- glmer.nb(Egg ~ Treatment + Habitat + (1|Container) + (1|Population),
#               data = BeetlesFemale)
#
# test_that("Negative binomial models with increasing complexity do not throw errors", {
#     expect_error(partR2(mod4), NA)
#     expect_error(partR2(mod4, nboot = 2), NA)
#     expect_error(partR2(mod4, partvars = c("Treatment")), NA)
#     expect_error(partR2(mod4, partvars = c("Treatment"), nboot = 2), NA)
# })

# insight::get_variance cannot calculate all varcomps for gamma distribution yet
# mod5 <- glmer(BodyL ~ Treatment + Habitat + (1 | Population) +
#             (1 | Container), family = Gamma(link = log), data = BeetlesBody)


