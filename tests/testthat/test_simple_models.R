context("Simple models")
data(BeetlesBody)
library(lme4)
library(partR2)
mod1 <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
             data = BeetlesBody)

test_that("Gaussian model with increasing complexity do not throw errors", {
    expect_error(partR2(mod1), NA)
    expect_error(partR2(mod1, nboot = 2), NA)
    expect_error(partR2(mod1, partvars = c("Sex")), NA)
    expect_error(partR2(mod1, partvars = c("Sex"), nboot = 2), NA)
    expect_error(partR2(mod1, partvars = c("Sex", "Treatment")), NA)
    expect_error(partR2(mod1, partvars = c("Sex", "Treatment"), nboot = 2), NA)
})

mod <- lmer(BodyL ~ Sex + Treatment + (1 + Treatment|Population),
              data=BeetlesBody)
#' (R2 <- partR2(mod, partvars = c("Sex"), R2_type = "marginal"))




#' # Random slopes (fixed effect Treatment can't be removed separately)
#' mod <- lmer(BodyL ~ Sex + Treatment + (1 + Treatment|Population),
#'              data=BeetlesBody)
#' (R2 <- partR2(mod, partvars = c("Sex"), R2_type = "marginal"))
#'
#'
#' # poisson data
#' data(BeetlesFemale)
#' BeetlesFemale$olre <- factor(1:nrow(BeetlesFemale))
#' mod <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population) + (1|olre),
#'                   data = BeetlesFemale, family = poisson)
#' (R2 <- partR2(mod, partvars = c("Treatment", "Habitat"),
#'               R2_type = "marginal", nboot = 5, CI = 0.95))
#'
#' # binomial data
#'
#' # binary
#' data(BeetlesMale)
#' mod <- glmer(Colour ~ Treatment + (1|Container) + (1|Population), data = BeetlesMale,
#'              family = binomial)
#' (R2 <- partR2(mod, partvars = c("Treatment"), R2_type = "marginal",
#'              nboot = 5, CI = 0.95))
#' (R2 <- partR2(mod, R2_type = "conditional", nboot = 10))
#'
#' # proportion
#' BeetlesMale$Dark <- BeetlesMale$Colour
#' BeetlesMale$Reddish <- (BeetlesMale$Colour-1)*-1
#' BeetlesColour <- aggregate(cbind(Dark, Reddish) ~ Treatment + Population + Container,
#'      data=BeetlesMale, FUN=sum)
#'
#' mod <- glmer(cbind(Dark,Reddish) ~ Treatment + (1|Population),
#'              data = BeetlesColour, family = binomial)
#' (R2 <- partR2(mod, partvars = c("Treatment"), R2_type = "marginal",
#'              nboot = 5, CI = 0.95))
#'
#'
#'


