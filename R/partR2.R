#' Partitioning R2 (R-square) in mixed models
#'
#' R2, semi-partial (part) R2 for predictors and their combinations as well as inclusive R2,
#' structure coefficients and beta weights for Gaussian, Poisson and binomial
#' mixed models.
#'
#' @param mod Fitted lme4 model (a merMod object).
#' @param partvars Character vector specifying the predictors (fixed effects) for which to partition the R2.
#'        Can be main effects like c("Var1", "Var2") and interactions ("Var1:Var2"). Predictors
#'        specified in partvars have to be named precisely like the terms in the formula to
#'        fit the model.
#' @param data The data.frame used to fit the lme4 model. If not provided,
#'        partR2 will try to fetch it.
#' @param R2_type "marginal" or "conditional" R2. With "marginal", the variance explained
#'        by fixed effects is calculated. With "conditional", the variance explained by
#'        both fixed and random effects is calculated.
#' @param max_level Level up to which shared semi-partial R2s are calculated.
#'        The number of sets for which to calculate R2 increases exponentially,
#'        i.e. for 10 variables 2^10 - 1 R2s  can be calculated. If you are
#'        only interested in the unique but not the shared effects, use max_level = 1.
#'        If interested in unique effects and combinations of two terms,
#'        use max_level = 2 etc.
#' @param nboot Number of parametric bootstrap iterations for confidence interval estimation
#'        (defaults to NULL, i.e. no bootstrapping). Larger numbers of bootstraps give a better
#'        asymptotic CI, but may be time-consuming. Bootstrapping can be switched on by setting
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
#'        with log link and in Binomial models with logit link (in all other cases the argument is ignored).
#'        The only valid terms are 'meanobs' and 'latent' (and 'liability for binary and proportion data).
#'        With the default 'meanobs', the expectation is estimated as the mean of the observations in the sample.
#'        With 'latent', the expectation is estimated from estimates of the intercept and variances on the link scale.
#'        While this is a preferred solution, it is susceptible to the distribution of fixed effect covariates and gives
#'        appropriate results typically only when all covariances are centered to zero. With 'liability'
#'        estimates follow formulae as presented in Nakagawa & Schielzeth (2010).
#' @param olre Logical, defaults to TRUE. This argument allows the user to prevent the automatic fitting of
#'        an observation level random effect (by setting it to FALSE) in Poisson and binomial models.
#'        The OLRE is used to account for overdispersion.
#' @param partbatch List of character vectors with predictors that should be fitted and
#'        removed together. For example, partbatch = list(batch1 = c("V1", "V2", "V3"),
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
#' \item{BW}{Standardised model estimates (beta weights) for fixed effects. Beta weights for Gaussian models
#' are calculated as beta * sd(x)/sd(y), with beta being the estimated
#' slope of a fixed effect for predictor x and response y. Beta weight for Non-Gaussian
#' models are calculated as beta * sd(x). Beta weights for interactions or polynomial
#' terms are not informative at the moment and we recommend users to standardise
#' variables themselves before fitting the model and to look at the model estimates (Ests)
#' instead of beta weights (BW) in the partR2 output. See vignette for details. }
#' \item{Ests}{Model estimates and confidence intervals.}
#' \item{R2_boot}{Parametric bootstrap samples for R2 for full model and partitions}
#' \item{SC_boot}{Parametric bootstrap samples for structure coefficients}
#' \item{IR2_boot}{Parametric bootstrap samples for inclusive R2 values}
#' \item{BW_boot}{Parametric bootstrap samples for beta weights}
#' \item{Ests_boot}{Parametric bootstrap samples for model estimates}
#' \item{partvars}{Predictors to partition}
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
#' generalized linear mixed-effects models}. Methods in Ecology and Evolution, 4(2), 133-142.
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
#'   data = biomass)
#'
#' # R2
#' (R2_1 <- partR2(mod))
#'
#' # R2 with CI
#' (R2_2 <- partR2(mod, R2_type = "marginal", nboot = 15, CI = 0.95))
#'
#' # Part (semi-partial) R2s with CIs
#' (R2_3 <- partR2(mod,
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

  # check if data is there and if not try to get it
  if (is.null(data)) {
    dat_name <- deparse(mod@call$data)
    data <- tryCatch(eval(mod@call$data), error=function(e) return(NULL))
    if (is.null(data)) stop(paste0("data ", dat_name,
                          " cannot be found. Please provide it with the data argument"))
  }

  # make combinations of predictors from partvars and partbatch
  all_comb <- make_combs(partvars, partbatch, max_level)

  # get family and response variable
  if (!(stats::family(mod)[[1]] %in% c("gaussian", "binomial", "poisson"))) {
    stop("partR2 currently only handles gaussian, binomial and poisson models")
  }

  # overdispersion, will only apply when poisson/proportion
  overdisp_out <- model_overdisp(mod = mod, dat = data, olre = olre)
  mod <- overdisp_out$mod
  data_mod <- overdisp_out$dat
  overdisp_name <- overdisp_out$overdisp_name

  # point estimates for all statistics
  # model estimates
  ests_pe <- fixef_simple(mod, intcp = FALSE)
  # beta weights
  bws_pe <- get_bw(mod)
  # calculate R2 and partial R2s
  r2s_pe <- part_R2s(
    mod = mod, expct = expct, overdisp_name = overdisp_name,
    R2_type = R2_type, all_comb = all_comb,
    partition = partition, data_mod = data_mod, allow_neg_r2 = allow_neg_r2
  )
  # structure coefficients
  scs_pe <- SC_pe(mod)
  # inclusive r2s
  ir2s_pe <- scs_pe %>%
    dplyr::mutate(estimate = c(.data$estimate)^2 * r2s_pe[r2s_pe$term == "Full",
                                     "estimate", drop = TRUE])

  # param. bootstrapping
  if (!is.null(nboot)) {

    # get bootstrap estimates
    boot_all <- bootstrap_all(
      nboot = nboot, mod = mod, R2_type = R2_type,
      all_comb = all_comb, partition = partition,
      data_mod = data_mod, allow_neg_r2 = allow_neg_r2,
      parallel = parallel, expct = expct, overdisp_name = overdisp_name
    )

    # all iterations in one df as list columns and calculating inclusive r2
    boot_out <- purrr::map_dfr(boot_all, "result", .id = "iter") %>%
      dplyr::mutate(ir2s = purrr::map2(.data$scs, .data$r2s, function(sc, r2) {
        tidyr::tibble(term = sc$term, estimate = sc$estimate^2 * unlist(r2[r2$term == "Full", "estimate"]))
      })) %>%
      dplyr::mutate(warnings = purrr::map(boot_all, "warnings"),
                    messages = purrr::map(boot_all, "messages"))
  }

  # if no bootstrap return same data structure with NA
  if (is.null(nboot)) {
    dfs_pe <- list(r2s_pe, ests_pe, bws_pe, scs_pe, ir2s_pe)
    boot_out <- purrr::map(dfs_pe, function(x){
      x$estimate <- NA
      list(x)
    }) %>%
    stats::setNames(c("r2s", "ests", "scs", "bws", "ir2s")) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(iter = NA, .before = 1) %>%
    dplyr::mutate(warnings = NA,
                  messages = NA)
  }

  # calculate CIs over list columns
  get_cis <- function(stc, df_pe) {
    # stc <- dplyr::enquo(stc)
    boot_out[[stc]] %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(.data$term) %>%
      dplyr::summarise(estimate = list(.data$estimate)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(calc_CI(unlist(.data$estimate), CI)) %>%
      dplyr::select(-.data$estimate) %>%
      dplyr::right_join(x = df_pe, by = "term")
  }

  stcs <- list("r2s", "ests", "bws", "scs", "ir2s")
  dfs_pe <- list(r2s_pe, ests_pe, bws_pe, scs_pe, ir2s_pe)
  pe_cis <- purrr::map2(stcs, dfs_pe, get_cis) %>%
    stats::setNames(stcs)

  # calculate numerator degrees of freedom and add to partial R2 object
  if ((length(all_comb) == 1) & (any(is.na(all_comb)))) {
    ndf_terms <- "Full"
  } else {
    ndf_terms <- c("Full", all_comb)
  }
  pe_cis$r2s$ndf <- suppressWarnings(purrr::map_int(
    ndf_terms, get_ndf,
    mod, data_mod
  ))

  # change partbatch with names, if present
  if (!is.null(names(partbatch))) {
    part_names <- mod_names_partbatch(partbatch, unname(pe_cis$r2s$term))
    pe_cis$r2s$term <- part_names
    boot_out$r2s <- purrr::map(boot_out$r2s, function(x) {
      x$term <- part_names
      x
    })
  }

  res <- list(
    call = mod@call,
    R2_type = R2_type,
    R2 = pe_cis$r2s,
    SC = pe_cis$scs,
    IR2 = pe_cis$ir2s,
    BW = pe_cis$bws,
    Ests = pe_cis$ests,
    R2_boot = boot_to_df(boot_out$r2s),
    SC_boot = boot_to_df(boot_out$scs),
    IR2_boot = boot_to_df(boot_out$ir2s),
    BW_boot = boot_to_df(boot_out$bws),
    Ests_boot = boot_to_df(boot_out$ests),
    partvars = partvars,
    partbatch = ifelse(is.null(partbatch), NA, partbatch),
    CI = CI,
    boot_warnings = boot_out$warnings,
    boot_messages = boot_out$messages
  )

  class(res) <- "partR2"
  return(res)
}
