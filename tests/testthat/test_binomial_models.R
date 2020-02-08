context("Binomial models")
library(lme4)

data(BeetlesMale)
mod1 <- glmer(Colour ~ Habitat + Treatment + (1|Population),
              data = BeetlesMale, family = binomial)

set.seed(124)
# only R2 pe
r2_mod1_1 <- partR2(mod1, data = BeetlesMale)
r2_mod1_2 <- partR2(mod1, data = BeetlesMale, nboot = 10)
r2_mod1_3 <- partR2(mod1, data = BeetlesMale, partvars = c("Treatment"))
r2_mod1_4 <- partR2(mod1, data = BeetlesMale, partvars = c("Treatment", "Habitat"))
#r2_mod1_5 <- partR2(mod1, data = BeetlesMale, nboot = 10, parallel = TRUE)

test_that("Binary models with increasing complexity give correct answers", {
    # pe
    expect_equal(r2_mod1_1$R2_pe_ci$R2, 0.067, tolerance = 0.01)
    # ci
    expect_equal(r2_mod1_2$R2_pe_ci$CI_lower, 0.044, tolerance = 0.01)
    expect_equal(r2_mod1_2$R2_pe_ci$CI_upper, 0.126, tolerance = 0.01)
    # R2 one partvar
    expect_equal(r2_mod1_3$R2_pe_ci$R2, c(0.067, 0.047), tolerance = 0.01)
    # R2 two partvars
    expect_equal(r2_mod1_4$R2_pe_ci$R2, c(0.067, 0.047, 0.021, 0.067),
                 tolerance = 0.01)
    # all together same as marginal
    expect_equal(r2_mod1_4$R2_pe_ci$R2[length(r2_mod1_4$R2_pe_ci$R2)],
                 r2_mod1_4$R2_pe_ci$R2[1], tolerance = 0.001)
})

# proportion
data(BeetlesMale)

# prepare proportion data
BeetlesMale$Dark <- BeetlesMale$Colour
BeetlesMale$Reddish <- (BeetlesMale$Colour-1)*-1
BeetlesColour <- aggregate(cbind(Dark, Reddish) ~ Treatment + Population + Container,
     data=BeetlesMale, FUN=sum)

mod2 <- glmer(cbind(Dark, Reddish) ~ Treatment + (1|Container) + (1|Population),
              family = "binomial", data = BeetlesColour)

set.seed(134)

# only R2 pe
r2_mod2_1 <- partR2(mod2, data = BeetlesColour)
r2_mod2_2 <- partR2(mod2, data = BeetlesColour, nboot = 10)
r2_mod2_3 <- partR2(mod2, data = BeetlesColour, partvars = c("Treatment"))
#r2_mod2_4 <- partR2(mod2, data = BeetlesColour, nboot = 10, parallel = TRUE)

test_that("Proportion models with increasing complexity give correct answers", {
    # pe
    expect_equal(r2_mod2_1$R2_pe_ci$R2, 0.0457, tolerance = 0.01)
    # ci
    expect_equal(r2_mod2_2$R2_pe_ci$CI_lower, 0.013, tolerance = 0.01)
    expect_equal(r2_mod2_2$R2_pe_ci$CI_upper, 0.071, tolerance = 0.01)
    # R2 one partvar
    expect_equal(r2_mod2_3$R2_pe_ci$R2, c(0.0455, 0.0455), tolerance = 0.01)
    # all together same as marginal
    expect_equal(r2_mod2_3$R2_pe_ci$R2[length(r2_mod1_3$R2_pe_ci$R2)],
                 r2_mod2_3$R2_pe_ci$R2[1], tolerance = 0.001)
})





