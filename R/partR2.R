#' Partition the R2 for mixed models
#'
#' R2, semi-partial R2 for predictors and their combinations, inclusive R2,
#' structure coefficients and beta weights for Gaussian, Poisson and binomial
#' mixed models.
#'
#' @param mod Fitted lme4 model (a merMod object).
#' @param partvars Character vector specifying the predictors (fixed effects) for which to partition the R2.
#'        Can be main effects like c("Var1", "Var2") and interactions ("Var1:Var2"). Predictors
#'        specified in partvars have to be named precisely like the terms in the formula to
#'        fit the model.
#' @param data data.frame used to fit the lme4 model. Has to be provided because
#'        the model is refitted to calculate semi-partial R2s.
#' @param R2_type "marginal" or "conditional" R2. With "marginal", the variance explained
#'        by fixed effects is calculated. With "conditional", the variance explained by
#'        both fixed and random effects is calculated.
#' @param max_level Level up to which shared semi-partial R2s are calculated.
#'        The number of sets for which to calculate R2 increases exponantially,
#'        i.e. for 10 variables 2^10 - 1 R2s  can be calculated. If you are
#'        only interested in the unique but not the shared effects, use max_level = 1.
#'        If interested in unique effects and combinations of two terms,
#'        use max_level = 2 etc.
#' @param nboot Number of parametric bootstraps for interval estimation
#'        (defaults to NULL). Larger numbers of bootstraps give a better
#'        asymtotic CI, but may be time-consuming. Bootstrapping can be switch on by setting
#'        \code{nboot = 1000}.
#' @param CI Width of the required confidence interval between 0 and 1 (defaults to
#'        0.95).
#' @param parallel If TRUE, computation uses \code{future} within \code{furrr::map} which allows
#'        parallelisation. However, it is necessary to specify a plan before running
#'        \code{partR2()}. To see which options you have, check \code{?future::plan} and have
#'        a look at our vignette for details. When running RStudio,
#'        usually \code{plan(multisession, workers = 4)} is a good choice,
#'        when you want to use 4 cores. To detect how many cores you have, use
#'        \code{parallel::detectCores()}. If no plan is specified, \code{partR2} will simply run
#'        sequentially.
#' @param expct A string specifying the method for estimating the expectation in Poisson models
#'        with log link and in Binomial models with logit link (in all other cases the agrument is ignored).
#'        The only valid terms are 'meanobs' and 'latent' (and 'liability for binary and proportion data).
#'        With the default 'meanobs', the expectation is estimated as the mean of the observations in the sample.
#'        With 'latent', the expectation is estimated from estiamtes of the intercept and variances on the link scale.
#'        While this is a preferred solution, it is susceptible to the distribution of fixed effect covariates and gives
#'        appropriate results typically only when all covariances are centered to zero. With 'liability'
#'        estimates follow formulae as presented in Nakagawa & Schielzeth (2010).
#' @param olre Logical, defaults to TRUE. This argument allows the user to prevent the automatic fitting of
#'        an obervation level random effect (by setting it to FALSE) in Poisson and binomial models.
#'        The OLRE is used to account for overdispersion.
#' @param partbatch List of character vectors with predictors that should be fitted and
#'        removed together. E.g. partbatch = list(batch1 = c("V1", "V2", "V3"),
#'        batch2 = c("V4", "V5", "V6")) would calculate part R2 only for combinations of
#'        predictors which contain the variables V1, V2, V3 together or/and V4,V5,V6 together.
#'        This is useful when the number of potential subsets gets too large to
#'        be computationally practical, for example when dummy coding is used.
#'        See our vignette for details. This feature is still experimental and
#'        should be used with caution.
#' @param allow_neg_r2 Calculating part R2 involves fitting two models, one with
#'        and one without the predictor of interest. In cases where the predictor
#'        has little association with the response, the resulting part R2 value
#'        can become negative. By default we set negative values to 0, but by
#'        setting this parameter to TRUE, R2 values can become negative.
#'
#'
#'
#' @return
#' Returns an object of class \code{partR2} that is a a list with the following elements:
#' \item{call}{model call}
#' \item{R2_type}{Marginal or conditional R2}
#' \item{R2}{R2 and confidence intervals for full model and semi-partial R2 for
#' predictors and their combinations}
#' \item{SC}{Structure coefficients and confidence intervals. SC are the
#' correlation between a predictor and the predicted response.}
#' \item{IR2}{Inklusive R2. This is SC^2 * R2_full.}
#' \item{BW}{Standardised model estimates (beta weights) for fixed effects and
#' variances for random effects and confidence intervals. Beta weights for Gaussian models
#' are calculated as beta * sd(x)/sd(y), with beta being the estimated
#' slope of a fixed effect for predictor x and response y. Beta weight for Non-Gaussian
#' models are calculated as beta * sd(x). Beta weights for interactions or polynomial
#' terms are not informative at the moment and we recommend users to standardise
#' variables themselves before fitting the model and to look at the model estimates (Ests)
#' instead of beta weights (BW) in the partR2 output. See vignette for details. }
#' \item{Ests}{Model estimates and confidence intervals. Point estimates
#' were extracted with broom.mixed::tidy}
#' \item{R2_boot}{Parametric bootstrap samples for R2 for full model and partitions}
#' \item{SC_boot}{Parametric bootstrap samples for structure coefficients}
#' \item{IR2_boot}{Parametric bootstrap samples for inklusive R2 values}
#' \item{BW_boot}{Parametric bootstrap samples for beta weights}
#' \item{Ests_boot}{Parametric bootstrap samples for model estimates}
#' \item{partvars}{predictors to partition}
#' \item{CI}{Coverage of the confidence interval as specified by the \code{CI} argument.}
#' \item{boot_warnings}{Potential warnings from estimating partial R2s during
#' parametric bootstrapping}
#' \item{boot_message}{Potential messages from estimating partial R2s
#' during parametric bootstrapping. Common are for example singularity messages
#' in lme4.}
#'
#' @references
#'
#' Nakagawa, S., & Schielzeth, H. (2013). \emph{A general and simple method for obtaining R2 from
#' generalized linear mixed‐effects models}. Methods in Ecology and Evolution, 4(2), 133-142.
#'
#' Newton, R. G., & Spurrell, D. J. (1967).  \emph{A development of multiple regression for the
#' analysis of routine data. Applied Statistics}. 51-64.
#'
#' @examples
#'
#' data(biomass)
#' library(lme4)
#'
#' # scale data
#' biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
#'
#' # Gaussian data
#' mod <- lmer(Biomass ~ Year + Temperature + Precipitation + SpeciesDiversity + (1 | Population),
#'   data = biomass
#' )
#'
#' # Only R2 with CI
#' (R2 <- partR2(mod, data = biomass, R2_type = "marginal", nboot = 15, CI = 0.95))
#'
#' # Partitioned R2
#' (R2 <- partR2(mod,
#'   data = biomass,
#'   partvars = c("SpeciesDiversity", "Temperature", "Precipitation"),
#'   R2_type = "marginal", nboot = 10, CI = 0.95
#' ))
#' @export


partR2 <- function(mod, partvars = NULL, data = NULL, R2_type = "marginal", max_level = NULL,
                   nboot = NULL, CI = 0.95, parallel = FALSE, expct = "meanobs",
                   olre = TRUE, partbatch = NULL, allow_neg_r2 = FALSE) {

  # initial checks
  if (!inherits(mod, "merMod")) stop("partR2 only supports merMod objects at the moment")
  partition <- ifelse(is.null(partvars) & is.null(partbatch), FALSE, TRUE)

  if (!is.null(nboot)) {
    if (nboot < 2) stop("nboot has to be greater than 1 or NULL")
  }

  if (!(R2_type %in% c("marginal", "conditional"))) {
    stop("R2_type has to be marginal or conditional")
  }

  # check if data is there
  if (is.null(data)) {
    dat_name <- deparse(mod@call$data)
    stop(paste0("data ", dat_name, " cannot be found. Please provide data argument"))
  }
  data_org <- data

  # make combinations of predictors from partvars and partbatch
  all_comb <- make_combs(partvars, partbatch, max_level)

  # names for partitions
  if (partition) {
    part_terms <- c("Full", unlist(lapply(all_comb, paste, collapse = "+")))
  } else if (!partition) {
    part_terms <- "Full"
  }

  # get family and response variable
  mod_fam <- stats::family(mod)[[1]]
  if (!(mod_fam %in% c("gaussian", "binomial", "poisson"))) {
    stop("partR2 only handles gaussian, binomial and poisson models at
              the moment")
  }
  resp <- lme4::getME(mod, "y")

  # overdispersion
  overdisp_out <- model_overdisp(mod = mod, dat = data_org, olre = olre)
  mod <- overdisp_out$mod
  data_mod <- overdisp_out$dat
  overdisp_name <- overdisp_out$overdisp_name

  # extract some essential info
  # we suppress messages here to avoid the notice that broom.mixed
  # overwrites the broom S3 methods.
  model_ests_full <- suppressMessages(
    broom.mixed::tidy(mod, effects = c("fixed"))
  )

  # beta weights
  model_bws_full <- get_bw(mod)

  # calculate R2 and partial R2s
  R2_org <- part_R2s(
    mod = mod, expct = expct, overdisp_name = overdisp_name,
    R2_type = R2_type, all_comb = all_comb,
    partition = partition, data_mod = data_mod, allow_neg_r2 = allow_neg_r2
  )

  # structure coefficients
  SC_org <- SC_pe(mod)

  # param. bootstrapping
  if (!is.null(nboot)) {

    # get bootstrap estimates
    boot_r2s_scs_ests <- bootstrap_all(nboot, mod, R2_type, all_comb, partition,
                                       data_mod, allow_neg_r2, parallel,
                                       expct, overdisp_name)

    # reshaping bootstrap output
    # put all commonality coefficients in one data.frame
    boot_r2s <- purrr::map(boot_r2s_scs_ests, function(x) x$result[["r2s"]]) %>%
      purrr::map_df(function(x) stats::setNames(as.data.frame(t(x[[1]])), part_terms))
    # put all structure coefficients in a data.frame
    boot_scs <- purrr::map_df(boot_r2s_scs_ests, function(x) x$result[["scs"]])
    # calculate inklusive R2
    # square the SC and multiply by full R2
    boot_ir2s <- purrr::map_df(boot_scs, function(sc) sc^2 * boot_r2s$Full)
    # put all model estimates in a data.frame
    boot_ests <- purrr::map(boot_r2s_scs_ests, function(x) x$result[["ests"]])
    boot_bws <- purrr::map(boot_r2s_scs_ests, function(x) x$result[["bws"]])
    # warnings and messages
    boot_warnings <- purrr::map(boot_r2s_scs_ests, function(x) x$warnings)
    boot_messages <- purrr::map(boot_r2s_scs_ests, function(x) x$messages)
  }

  # if no bootstrap return same data.frames only with NA
  if (is.null(nboot)) {
    boot_r2s <- rep(NA, length(part_terms)) %>%
      as.list() %>%
      as.data.frame() %>%
      stats::setNames(part_terms)
    boot_scs <- matrix(nrow = 1, ncol = ncol(SC_org)) %>%
      as.data.frame() %>%
      stats::setNames(names(SC_org))
    boot_ir2s <- matrix(nrow = 1, ncol = ncol(SC_org)) %>%
      as.data.frame() %>%
      stats::setNames(names(SC_org))
    boot_ests <- model_ests_full %>%
      dplyr::mutate(estimate = NA) %>%
      # cut off std.err and statistic from broom output
      dplyr::select(-.data$std.error, -.data$statistic) %>%
      list(sim1 = .)
    boot_bws <- boot_ests
    boot_warnings <- character(0)
    boot_messages <- character(0)
  }

  # calculate CIs
  r2_cis <- purrr::map_df(boot_r2s, calc_CI, CI, .id = "parts") %>%
    tibble::add_column(R2 = as.numeric(unlist(R2_org)), .after = "parts")

  ests_cis <- purrr::map_df(boot_ests, "estimate") %>%
    purrr::pmap_df(function(...) calc_CI(c(...), CI)) %>%
    cbind(model_ests_full[1:4], .)

  bws_cis <- purrr::map_df(boot_bws, "estimate") %>%
    purrr::pmap_df(function(...) calc_CI(c(...), CI)) %>%
    cbind(model_bws_full[1:4], .)

  sc_cis <- purrr::map_df(boot_scs, calc_CI, CI, .id = "parts") %>%
    tibble::add_column(SC = as.numeric(SC_org), .after = "parts")

  ir2_cis <- purrr::map_df(boot_ir2s, calc_CI, CI, .id = "parts") %>%
    tibble::add_column(IR2 = as.numeric(SC_org^2 * R2_org$R2[1]), .after = "parts") %>%
    as.data.frame()

  # calculate numerator degrees of freedom and add to partial R2 object
  if ((length(all_comb) == 1) & (any(is.na(all_comb)))) {
    ndf_terms <- "Full"
  } else {
    ndf_terms <- c("Full", all_comb)
  }
  ndf <- suppressWarnings(purrr::map_int(ndf_terms, get_ndf, mod, data_mod))
  r2_cis$ndf <- ndf

  # change partbatch with names, if present
  if (!is.null(names(partbatch))) {
    added_batches <- purrr::map(partbatch, function(x) {
      out <- paste(x, collapse = "+")
      out
    })

    part_names <- r2_cis$parts
    for (i in 1:length(added_batches)) {
      if (is.null(names(added_batches)[i])) next()
      part_names <- gsub(added_batches[[i]], names(added_batches)[i], part_names, fixed = TRUE)
    }
    r2_cis$parts <- part_names
    names(boot_r2s) <- part_names
  }

  res <- list(
    call = mod@call,
    # datatype = "gaussian",
    R2_type = R2_type,
    R2 = r2_cis,
    SC = sc_cis,
    IR2 = ir2_cis,
    BW = bws_cis,
    Ests = ests_cis,
    R2_boot = boot_r2s,
    SC_boot = boot_scs,
    IR2_boot = boot_ir2s,
    BW_boot = boot_bws,
    Ests_boot = boot_ests,
    partvars = partvars,
    partbatch = ifelse(is.null(partbatch), NA, partbatch),
    CI = CI,
    boot_warnings = boot_warnings,
    boot_messages = boot_messages
  )

  class(res) <- "partR2"
  return(res)
}
