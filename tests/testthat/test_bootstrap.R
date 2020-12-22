context("Bootstrapping")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
mod <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
            data = biomass)


partvars = c("SpeciesDiversity", "Temperature", "Precipitation")
