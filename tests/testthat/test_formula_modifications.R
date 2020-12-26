context("Formula modifications")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
mod <- lme4::lmer(Biomass ~ Year + Temperature^2 + log(Precipitation) + SpeciesDiversity + (1 | Population),
                  data = biomass)

partvars <- c("Temperature^2", "poly(Precipitation, 2)")
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

r2s_pe <- part_R2s(
    mod = mod, expct = expct, overdisp_name = overdisp_name,
    R2_type = R2_type, all_comb = all_comb,
    partition = partition, data_mod = data_mod, allow_neg_r2 = allow_neg_r2
)
