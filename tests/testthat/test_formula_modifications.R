context("Formula modifications")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
mod <- lme4::lmer(Biomass ~ Year + I(Temperature^2) + Precipitation + SpeciesDiversity + (1 | Population),
                  data = biomass)

partvars <- c("I(Temperature^2)")
data_mod <- biomass
R2_type <- "marginal"
max_level <- NULL
nboot <- 10
CI <- 0.95
parallel <- FALSE
expct <- "meanobs"
olre <- TRUE
partbatch <- NULL
allow_neg_r2 <- FALSE
partition <- TRUE
all_comb <-  make_combs(partvars, partbatch, max_level)

r2s_pe <- part_R2s(
    mod = mod, expct = expct, overdisp_name = overdisp_name,
    R2_type = R2_type, all_comb = all_comb,
    partition = partition, data_mod = data_mod, allow_neg_r2 = allow_neg_r2
)
