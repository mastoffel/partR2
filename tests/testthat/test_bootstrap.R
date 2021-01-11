context("Bootstrapping")

data(biomass)
biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
fit1 <- lme4::lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
  data = biomass
)

nboot <- 5
mod <- fit1
R2_type <- "marginal"
partvars <- c("SpeciesDiversity", "Temperature", "Precipitation")
all_comb <- make_combs(partvars, partbatch = NULL, max_level = NULL)
partition <- TRUE
data_mod <- biomass
allow_neg_r2 <- TRUE
parallel <- FALSE
expct <- "meanobs"
overdisp_name <- "overdisp"

set.seed(12321)
boot_all <- bootstrap_all(
  nboot, mod, R2_type, all_comb, partition,
  data_mod, allow_neg_r2, parallel,
  expct, overdisp_name
)

boot_out <- purrr::map_dfr(boot_all, "result", .id = "iter") %>%
  dplyr::mutate(ir2s = purrr::map2(.data$scs, .data$r2s, function(sc, r2) {
    tidyr::tibble(term = sc$term, estimate = sc$estimate^2 * unlist(r2[r2$term == "Full", "estimate"]))
  })) %>%
  dplyr::mutate(
    warnings = purrr::map(boot_all, "warnings"),
    messages = purrr::map(boot_all, "messages")
  )

test_that("bootstrap_all creates the correct data structure", {
  expect_equal(boot_out$iter, paste0("sim_", 1:nboot))
  expect_true(all(c("r2s", "ests", "scs", "bws", "ir2s") %in% names(boot_out)))
  # check if for some reason some of the bootstraps are the same
  expect_false(all(purrr::map2_lgl(
    1:nrow(boot_out), c(2:nrow(boot_out), 1),
    function(x, y) identical(boot_out$r2s[x], boot_out$r2s[y])
  )))
  # check that all data.frames have the correct dimensions
  expect_equal(
    purrr::map_int(boot_out$r2s, function(x) nrow(x)) %>% mean(),
    length(all_comb) + 1
  )
})

# test parallel / 1 worker so that CRAN build doesn't fail
library(future)
plan(multisession, workers = 1)

parallel <- TRUE
set.seed(12321)
boot_all2 <- bootstrap_all(
  nboot, mod, R2_type, all_comb, partition,
  data_mod, allow_neg_r2, parallel,
  expct, overdisp_name
)

boot_out2 <- purrr::map_dfr(boot_all2, "result", .id = "iter") %>%
  dplyr::mutate(ir2s = purrr::map2(.data$scs, .data$r2s, function(sc, r2) {
    tidyr::tibble(term = sc$term, estimate = sc$estimate^2 * unlist(r2[r2$term == "Full", "estimate"]))
  })) %>%
  dplyr::mutate(
    warnings = purrr::map(boot_all, "warnings"),
    messages = purrr::map(boot_all, "messages")
  )

test_that("parallel bootstraps are the same as non-parallel bootstraps", {
  expect_true(identical(boot_out, boot_out2))
})
