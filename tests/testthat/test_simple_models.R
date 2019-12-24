context("Simple models")
data(BeetlesBody)
library(lme4)
mod1 <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
             data = BeetlesBody)

test_that("Gaussian models with increasing complexity do not throw errors", {
    expect_error(partR2(mod1, nboot = 2), NA)
    expect_error(partR2(mod1, nboot = 2, parallel = TRUE, ncores = 2), NA)
    expect_error(partR2(mod1, partvars = c("Sex")), NA)
    expect_error(partR2(mod1, partvars = c("Sex"), nboot = 2), NA)
    expect_error(partR2(mod1, partvars = c("Sex", "Treatment")), NA)
    expect_error(partR2(mod1, partvars = c("Sex", "Treatment"), nboot = 2), NA)
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


