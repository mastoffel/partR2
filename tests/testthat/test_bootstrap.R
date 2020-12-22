context("Bootstrapping")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
mod <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
            data = biomass)

partvars = c("SpeciesDiversity", "Temperature", "Precipitation")
all_comb <- make_combs(partvars, partbatch = NULL, max_level = NULL)
expct <- "meanobs"
overdisp_name <- "overdisp"
R2_type <- "marginal"
partition <- TRUE
data_mod <- biomass
allow_neg_r2 <- TRUE
nboot <- 10
parallel = FALSE

boot_all <- bootstrap_all(
    nboot, mod, R2_type, all_comb, partition,
    data_mod, allow_neg_r2, parallel,
    expct, overdisp_name
)
