context("partR2")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
mod <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
                  data = biomass)




partvars <- c("Temperature", "Precipitation", "SpeciesDiversity")
data <- biomass
R2_type <- "marginal"
max_level <- NULL
nboot <- 10
CI <- 0.95
parallel <- FALSE
expct <- "meanobs"
olre <- TRUE
partbatch <- NULL
allow_neg_r2 <- FALSE

