context("Basic functionality")

data(biomass)
library(lme4)

# scale everything dbl
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)

# Gaussian data
mod1 <- lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
             data = biomass)

R2_1 <- partR2(mod1, data = biomass)

test_that("Full model output is correct", {
    # R2
    expect_equal(R2_1$R2$R2, 0.62, tolerance = 0.01)
    # SC
    expect_equal(R2_1$SC$SC, c(0.188,0.755,0.779,0.551,-0.195), tolerance = 0.01)
    # IR2
    expect_equal(R2_1$IR2$IR2, c(0.022,0.3534,0.3762,0.1879,0.0235), tolerance = 0.01)
    # BW
    expect_equal(R2_1$BW$estimate, c(-74.1, 0.104, 0.27, 0.40, 0.41, -0.1094), tolerance = 0.01)
})

R2_2 <- partR2(mod1, partvars = c("Temperature", "Precipitation", "Temperature:Precipitation",
                                  "SpeciesDiversity"), data = biomass)

test_that("partvars works", {
    expect_equal(nrow(R2_2$R2), 16)
    # check that everything has a point estimate
    expect_false(any(is.na(R2_2$R2$R2)))
})

R2_3 <- partR2(mod1, partvars = c("Temperature", "Precipitation", "Temperature:Precipitation",
                                  "SpeciesDiversity"), data = biomass, max_level = 1)

test_that("max_level works for partvars", {
    expect_equal(nrow(R2_3$R2), 5)
})

R2_4 <- partR2(mod1, partvars = c("SpeciesDiversity"),
               partbatch = list(b1 = c("Temperature","Temperature:Precipitation"), b2 = c("Precipitation", "Temperature:Precipitation")),
               data = biomass)

#R2_4

test_that("partvars and partbatch combine properly", {
    expect_equal(R2_4$R2$parts, c("Full", "b1", "b2", "SpeciesDiversity",
                                  "SpeciesDiversity+b1", "SpeciesDiversity+b2",
                                  "SpeciesDiversity+b1+Precipitation"))
})


# check olre
data(BeetlesFemale)
BeetlesFemale$overdisp <- factor(1:nrow(BeetlesFemale))
mod2a <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population),
              data = BeetlesFemale, family = poisson)

mod2b <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population) + (1|overdisp),
               data = BeetlesFemale, family = poisson)

test_that("olre is fitted and can be removed", {

    expect_message(partR2(mod2a, data = BeetlesFemale), "An observational level random-effect has been fitted")
    # compare autofit vs. prefit
    expect_equal(partR2(mod2a, data = BeetlesFemale)$R2$R2, partR2(mod2b, data = BeetlesFemale)$R2$R2)
    # check that removed olre changes result
    expect_true(partR2(mod2a, data = BeetlesFemale, olre = FALSE)$R2$R2 != partR2(mod2b, data = BeetlesFemale)$R2$R2)

})

