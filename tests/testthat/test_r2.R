context("part R2 calculation")

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
data_mod <- data <- biomass
allow_neg_r2 <- TRUE

r2s_pe <- part_R2s(
    mod = mod, expct = expct, overdisp_name = overdisp_name,
    R2_type = R2_type, all_comb = all_comb,
    partition = partition, data_mod = data_mod, allow_neg_r2 = allow_neg_r2
)

r2s_pe2 <- part_R2s(
    mod = mod, expct = expct, overdisp_name = overdisp_name,
    R2_type = "conditional", all_comb = all_comb,
    partition = partition, data_mod = data_mod, allow_neg_r2 = allow_neg_r2
)

test_that("Part R2 point estimates work", {
    expect_equal(nrow(r2s_pe), length(all_comb) + 1)
    expect_equal(r2s_pe[r2s_pe$term == "Full", "estimate", drop = TRUE], 0.600551)
    expect_equal(r2s_pe[r2s_pe$term == "SpeciesDiversity+Temperature", "estimate",
                        drop = TRUE], 0.223, tolerance = 0.001)

    expect_equal(nrow(r2s_pe2), length(all_comb) + 1)
    expect_equal(r2s_pe2[r2s_pe2$term == "Full", "estimate", drop = TRUE], 0.644,
                 tolerance = 0.001)
    expect_equal(r2s_pe2[r2s_pe2$term == "SpeciesDiversity+Temperature", "estimate",
                        drop = TRUE], 0.0564, tolerance = 0.001)
})




