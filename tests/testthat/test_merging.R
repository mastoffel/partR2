context("Merge partR2 objects")

data(biomass)
# scale data
#biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
biomass2 <- biomass %>%
            dplyr::mutate(dplyr::across(c(Temperature,Precipitation,SpeciesDiversity,Year),
                                        function(x) (x-mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE)))

# Full model
mod_full <- lme4::lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
            data = biomass2)

# Semi-partial R2 for interaction and all other predictors of interest
R2_full <- partR2(mod_full, partvars = c("Temperature:Precipitation", "SpeciesDiversity", "Year"),
                  data = biomass2)

# model without interaction to get main effect semi-partial R2s
mod_noIA <- lme4::lmer(Biomass ~  Year + Temperature + Precipitation + SpeciesDiversity + (1|Population),
            data = biomass2)
R2_noIA <- partR2(mod_noIA, partvars = c("Temperature", "Precipitation"),
                  data = biomass2)

# combine both
R2_comb <- mergeR2(R2_full, R2_noIA)

test_that("mergeR2 results in correct number of terms", {

    expect_true(all(R2_comb$R2$term %in% unique(c(R2_full$R2$term, R2_noIA$R2$term))))
    expect_true(all(R2_comb$R2_boot$term %in% unique(c(R2_full$R2_boot$term, R2_noIA$R2_boot$term))))

})


