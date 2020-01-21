context("Binomial models")
library(lme4)

data(BeetlesMale)
mod1 <- glmer(Colour ~ Habitat + Treatment + (1|Population),
              data = BeetlesMale, family = binomial)

set.seed(124)
# only R2 pe
r2_mod1_1 <- partR2(mod1)
r2_mod1_2 <- partR2(mod1, nboot = 10)
r2_mod1_3 <- partR2(mod1, partvars = c("Treatment"))
r2_mod1_4 <- partR2(mod1, partvars = c("Treatment", "Habitat"))
r2_mod1_5 <-  partR2(mod1, nboot = 10, parallel = TRUE)

test_that("Binary models with increasing complexity do not throw errors", {
    # pe
    expect_equal(r2_mod1_1$R2_pe_ci$R2, 0.067, tolerance = 0.01)
    # ci
    expect_equal(r2_mod1_2$R2_pe_ci$CI_lower, 0.044, tolerance = 0.01)
    expect_equal(r2_mod1_2$R2_pe_ci$CI_upper, 0.093, tolerance = 0.01)
    # R2 one partvar
    expect_equal(r2_mod1_3$R2_pe_ci$R2, c(0.067, 0.047), tolerance = 0.01)
    # R2 two partvars
    expect_equal(r2_mod1_4$R2_pe_ci$R2, c(0.067, 0.047, 0.021, 0.067),
                 tolerance = 0.01)
    # all together same as marginal
    expect_equal(r2_mod1_4$R2_pe_ci$R2[length(r2_mod1_4$R2_pe_ci$R2)],
                 r2_mod1_4$R2_pe_ci$R2[1], tolerance = 0.001)
})

