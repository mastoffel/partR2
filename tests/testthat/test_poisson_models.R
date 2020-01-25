context("Poisson models")
library(lme4)

data(BeetlesFemale)
BeetlesFemale$overdisp <- factor(1:nrow(BeetlesFemale))
mod1 <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population) + (1|overdisp),
              data = BeetlesFemale, family = poisson)

set.seed(927)
# only R2 pe
r2_mod1_1 <- partR2(mod1, data = BeetlesFemale)
r2_mod1_2 <- partR2(mod1, data = BeetlesFemale, nboot = 10)
r2_mod1_3 <- partR2(mod1, data = BeetlesFemale, partvars = c("Treatment"))
r2_mod1_4 <- partR2(mod1, data = BeetlesFemale, partvars = c("Treatment", "Habitat"))
r2_mod1_5 <- partR2(mod1, data = BeetlesFemale, nboot = 10, parallel = TRUE)

#r2_mod1_6 <- partR2(mod1, partvars = c("Temperature*Precipitation"))
test_that("Poisson models with increasing complexity do not throw errors", {
    # pe
    expect_equal(r2_mod1_1$R2_pe_ci$R2, 0.103, tolerance = 0.01)
    # ci
    expect_equal(r2_mod1_2$R2_pe_ci$CI_lower, 0.0721, tolerance = 0.01)
    expect_equal(r2_mod1_2$R2_pe_ci$CI_upper, 0.162, tolerance = 0.01)
    # R2 one partvar
    expect_equal(r2_mod1_3$R2_pe_ci$R2, c(0.103, 0.099), tolerance = 0.01)
    # R2 two partvars
    expect_equal(r2_mod1_4$R2_pe_ci$R2, c(0.103, 0.099, 0.003, 0.103),
                 tolerance = 0.01)
    # all together same as marginal
    expect_equal(r2_mod1_4$R2_pe_ci$R2[length(r2_mod1_4$R2_pe_ci$R2)],
                 r2_mod1_4$R2_pe_ci$R2[1], tolerance = 0.001)
})

