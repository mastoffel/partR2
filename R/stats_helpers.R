#' Get tidy summary of fixed effect estimates
#'
#' @param mod merMod object
#' @param intcp include intercept in summary? Defaults to FALSE
#'
#' @return tibble with two columns: term and estimate
#' @keywords internal
#'
fixef_simple <- function(mod, intcp = FALSE) {
  if (!inherits(mod, "merMod")) stop("only merMod supported atm")
  out <- dplyr::as_tibble(lme4::fixef(mod), rownames = "term") %>%
    dplyr::rename(estimate = .data$value)
  if (!intcp) out <- out[!(out$term == "(Intercept)"), ]
  out
}


#' Adds an observational level random effect to a model, if applicable.
#'
#'
#' @param mod merMod object.
#' @param dat The underlying data.frame
#' @return list with three elements: merMod object with added OLRE,
#' data.frame with added OLRE variable, and name of OLRE.
#' @keywords internal
#'
model_overdisp <- function(mod, dat, olre) {
  # family
  mod_fam <- stats::family(mod)[[1]]
  resp <- lme4::getME(mod, "y")
  overdisp_name <- "overdisp"

  # fit olre if not FALSE and family is either poisson or binomial but not binary
  if (olre & (mod_fam == "poisson" | ((mod_fam == "binomial") & (length(table(resp)) > 2)))) {
    # check if OLRE already there
    overdisp_term <- lme4::getME(mod, "l_i") == nrow(dat)
    # if so, get variable name
    if (sum(overdisp_term) == 1) {
      overdisp_name <- names(overdisp_term)[overdisp_term]
      # rename OLRE to overdisp if not done so already
      if (!(overdisp_name == "overdisp")) {
        # names(dat[overdisp]) <- "overdisp"
        message(paste0("'", overdisp_name, "' has been recognized as observational
level random effect and is used to quantify overdispersion"))
      }
    } else if ((sum(overdisp_term) == 0)) {
      dat[[overdisp_name]] <- as.factor(1:nrow(dat))
      mod <- stats::update(mod, . ~ . + (1 | overdisp), data = dat)
      message("An observational level random-effect has been fitted
to account for overdispersion.")
    }
  }
  out <- list(mod = mod, dat = dat, overdisp_name = overdisp_name)
}

#' Calculates CI from bootstrap replicates
#'
#'
#' @param x numeric vector
#' @param CI CI level, e.g. 0.95
#' @return data.frame with two columns for lower and upper CI
#' @keywords internal
#'
#
# CI function
calc_CI <- function(x, CI) {
  out <- stats::quantile(x, c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE)
  out <- as.data.frame(t(out))
  names(out) <- c("CI_lower", "CI_upper")
  rownames(out) <- NULL
  out
}

#' Get numerator dfs for reduced models
#'
#' @param partvar One or more fixed effect variables which are taken out
#' of the model.
#' @param mod merMod object.
#' @param dat Data.frame to fit the model
#' @keywords internal
#' @return Numeric vector, giving numerator degrees of freedom
#'
get_ndf <- function(partvar, mod, dat) {
  if (("Full" %in% partvar) & (length(partvar) == 1)) {
    return(ncol(stats::model.matrix(mod)))
  }
  # which variables to reduce?
  to_del <- paste(paste("-", partvar, sep = ""), collapse = " ")
  # reduced formula
  formula_red <- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep = ""))
  # fit reduced model
  mod_red <- stats::update(mod, formula. = formula_red, data = dat)
  ncol(stats::model.matrix(mod_red))
}

#' Structure coefficients
#'
#' @param mod merMod object.
#' @keywords internal
#' @return data.frame with structure coefficients
#'
#'
SC_pe <- function(mod) {
  Yhat <- stats::predict(mod, re.form = NA)
  mod_mat <- stats::model.matrix(mod)
  mod_mat <- mod_mat[, colnames(mod_mat) != "(Intercept)", drop = FALSE]
  scs <- stats::cor(Yhat, mod_mat)
  out <- tidyr::tibble(term = colnames(scs), estimate = as.numeric(scs))
}

#' Get beta weights
#'
#' @param mod merMod object.
#' @keywords internal
#' @return data.frame with bw instead of raw estimates
#'
#'

get_bw <- function(mod) {
  mod_mat <- stats::model.matrix(mod) %>%
    as.data.frame() %>%
    dplyr::select(-"(Intercept)")
  bin_preds <- purrr::map_lgl(mod_mat, function(x) length(table(x)) <= 2)
  resp <- lme4::getME(mod, "y")
  sds <- purrr::map_dbl(mod_mat, stats::sd, na.rm = TRUE)
  # only for gaussian dividing by sd of response
  if (stats::family(mod)$family == "gaussian") sds <- sds / stats::sd(resp, na.rm = TRUE)
  # only standardise for non-factors
  sds[bin_preds] <- 1
  # simple posthoc standardisation. Doesn't work for interactions and should
  # be turned of when standardised before
  ests <- fixef_simple(mod, intcp = FALSE)
  ests[ests$term %in% names(sds), "estimate"] <-
    purrr::map_dbl(names(sds), function(x) {
      unlist(ests[ests$term %in% x, "estimate"] * sds[x])
    })
  ests
}


#' Parametric bootstrapping
#'
#' @param mod merMod object, lme4 fit
#' @param all_comb list of predictor combinations
#' @param partition TRUE or FALSE
#' @param data_mod Data for model
#' @param overdisp_name Name of overdispersion term
#' @inheritParams partR2
#'
#' @keywords internal
#' @return Bootstrap samples for all statistics, plus associated warnings
#'
bootstrap_all <- function(nboot, mod, R2_type, all_comb, partition,
                          data_mod, allow_neg_r2, parallel,
                          expct, overdisp_name) {

  # simulating new responses for param. bootstraps
  if (nboot > 0) Ysim <- as.data.frame(stats::simulate(mod, nsim = nboot))
  # main bootstrap function
  bootstr <- function(y, mod, expct, overdisp_name, R2_type, all_comb,
                      partition, data_mod, allow_neg_r2) {
    mod_iter <- lme4::refit(mod, newresp = y)
    out_r2s <- part_R2s(
      mod = mod_iter, expct = expct, overdisp_name = overdisp_name,
      R2_type = R2_type, all_comb = all_comb, partition = partition,
      data_mod = data_mod, allow_neg_r2 = allow_neg_r2
    )
    out_scs <- SC_pe(mod_iter)
    out_ests <- fixef_simple(mod_iter, intcp = FALSE)
    out_bw <- get_bw(mod_iter)
    out <- tidyr::tibble(
      r2s = list(out_r2s), ests = list(out_ests), scs = list(out_scs),
      bws = list(out_bw)
    )
  }
  # capture warnings and messages
  bootstr_quiet <- purrr::quietly(bootstr)

  # refit model with new responses
  if (!parallel) {
    boot_r2s_scs_ests <- pbapply::pblapply(
      Ysim, bootstr_quiet,
      mod, expct, overdisp_name,
      R2_type, all_comb,
      partition, data_mod,
      allow_neg_r2
    )
  }

  if (parallel) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      stop("Package \"furrr\" needed for parallelisation. Please install it.",
        call. = FALSE
      )
    }
    # if (is.null(ncores)) ncores <- parallel::detectCores()-1
    # let the user plan
    # future::plan(future::multiprocess, workers = ncores)
    boot_r2s_scs_ests <- furrr::future_map(Ysim, bootstr_quiet,
      mod, expct, overdisp_name,
      R2_type, all_comb,
      partition, data_mod,
      allow_neg_r2,
      .options = furrr::furrr_options(packages = "lme4"),
      .progress = TRUE
    )
  }

  boot_r2s_scs_ests
}
