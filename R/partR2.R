#' Partition the R2 for Gaussian mixed models
#'
#' R2, commonality coefficients and structure coefficients for gaussian lme4 models.
#' @param mod merMod object fitted with lme4.
#' @param partvars Character vector specifying the predictors for which to partition the R2.
#' @param R2_type "marginal" or "conditional" R2.
#' @param cc_level Level up to which commonality coefficients are calculated.
#'        The number of sets for which to calculate partial R2 increases exponantially,
#'        i.e. for 10 variables 2^10 - 1 commonality coefficients and potentially CIs
#'        can be calculated. To limit this number for models with many fixed effects,
#'        specify cc_level. cc_level = 3 would limit the number of sets to a maximum of
#'        triplets, i.e. calculating partial R2 for each individual fixed effect specified in
#'        partvars, for each pair and for each triplet.
#' @param nboot Number of parametric bootstraps for interval estimation
#'        (defaults to NULL). Larger numbers of bootstraps give a better
#'        asymtotic CI, but may be time-consuming. Bootstrapping can be switch on by setting
#'        \code{nboot = 1000}.
#' @param CI Width of the required confidence interval between 0 and 1 (defaults to
#'        0.95).
#' @param parallel If TRUE, computation runs in parallel, leaving one CPU free, except ncores is specified.
#' @param ncores number of cpus for parallel computation
#' @param expct A string specifying the method for estimating the expectation in Poisson models
#'        with log link and in Binomial models with logit link (in all other cases the agrument is ignored).
#'        The only valid terms are 'meanobs' and 'latent' (and 'liability for binary and proportion data).
#'        With the default 'meanobs', the expectation is estimated as the mean of the observations in the sample.
#'        With 'latent', the expectation is estimated from estiamtes of the intercept and variances on the link scale.
#'        While this is a preferred solution, it is susceptible to the distribution of fixed effect covariates and gives
#'        appropriate results typically only when all covariances are centered to zero. With 'liability'
#'        estimates follow formulae as presented in Nakagawa & Schielzeth (2010).
#'
#'
#' @return
#' Returns an object of class \code{partR2} that is a a list with the following elements:
#' \item{call}{model call}
#' \item{R2_type}{Marginal or conditional R2}
#' \item{R2_pe_ci}{R2 and confidence intervals for full model and partitions}
#' \item{SC_pe_ci}{Structure coefficients and confidence intervals}
#' \item{Ests_pe_ci}{Model estimates and confidence intervals. Point estimates
#' were extracted with broom.mixed::tidy}
#' \item{R2_boot}{Parametric bootstrap samples for R2 for full model and partitions}
#' \item{SC_boot}{Parametric bootstrap samples for structure coefficients}
#' \item{Ests_pe_ci}{Parametric bootstrap samples for model estimates}
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
#' # Gaussian data
#' mod <- lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
#'             data = biomass)
#' # Only R2 with CI
#' (R2 <- partR2(mod, R2_type = "marginal", nboot = 15, CI = 0.95))
#'
#' # Partitioned R2
#' (R2 <- partR2(mod,  partvars = c("SpeciesDiversity", "Temperature:Precipitation",
#'                                  "Temperature", "Precipitation"),
#'                                  R2_type = "marginal", nboot = 10, CI = 0.95))
#'
#'
#'
#' @export


partR2 <- function(mod, partvars = NULL, R2_type = "marginal", cc_level = NULL,
                   nboot = NULL, CI = 0.95, parallel = FALSE, ncores = NULL,
                   expct = "meanobs"){

    # initial checks
    if(!inherits(mod, "merMod")) stop("partR2 only supports merMod objects at the moment")
    partition <- ifelse(is.null(partvars), FALSE, TRUE)

    if (!is.null(nboot)) {
        if (nboot < 2) stop("nboot has to be greater than 1 or NULL")
    }

    if (!(R2_type %in% c("marginal", "conditional"))) {
        stop("R2_type has to be marginal or conditional")
    }

    if (parallel) {
        if (is.null(ncores)) ncores <- parallel::detectCores()-1
    }

    # check whether partvars are fixed effects
    # check fixed effect names
    # This is wrong
    fixed_terms <- names(lme4::fixef(mod))
    if (!(all(partvars[-grep(":", partvars)] %in% fixed_terms))) {
        stop("partvars have to be fixed effects")
    }

    # create list of all unique combinations except for the full model
    if (!is.null(partvars)) {
        if (length(partvars) > 1){
            all_comb <- unlist(lapply(1:(length(partvars)),
                        function(x) utils::combn(partvars, x, simplify = FALSE)),
                        recursive = FALSE)
        } else if (length(partvars) == 1) {
            all_comb <- as.list(partvars)
        }
    }  else {
        all_comb <- NA
    }

    # commonality coefficients up to cc_level (e.g. 3 for
    # the cc of 3 predictors)
    if (!is.null(cc_level)) {
        remove_combs <- purrr::map_lgl(all_comb, function(x) length(x) > cc_level)
        all_comb[remove_combs] <- NULL
    }

    # names for partitions
    if (partition) {
        part_terms <- c("Full", unlist(lapply(all_comb, paste, collapse = "+")))
    } else if (!partition) {
        part_terms <- "Full"
    }

    # get data, family and response variable
    data_original <- stats::model.frame(mod)
    mod_fam <- stats::family(mod)[[1]]
    resp <- lme4::getME(mod, "y")

    # check if cbind created a matrix as first data.frame column
    # this has to be checked
    any_mat <- purrr::map_lgl(data_original, is.matrix)
    if (any(any_mat)) {
        data_original <- cbind(as.data.frame(data_original[[which(any_mat)]]),
                               data_original[(which(any_mat)+1):ncol(data_original)])
    }

    # overdispersion
    overdisp_out <- model_overdisp(mod, dat = data_original)
    mod <- overdisp_out$mod
    data_original <- overdisp_out$dat

    # extract some essential info
    formula_full <- stats::formula(mod)
    model_ests_full <- broom.mixed::tidy(mod)

    # R2
    R2_pe <- function(mod, expct) {

        # get variance components
        var_comps <- get_var_comps(mod, expct)

        if (R2_type == "marginal") {
            R2_out <- var_comps %>%
                dplyr::mutate(R2 = .data$var_fix /
                             (.data$var_fix + .data$var_ran + .data$var_res)) %>%
                dplyr::select(.data$R2)
        } else if (R2_type == "conditional") {
            R2_out <- var_comps %>%
                dplyr::mutate(R2 = (.data$var_fix  + .data$var_ran) /
                             (.data$var_fix + .data$var_ran + .data$var_res)) %>%
                dplyr::select(.data$R2)
        }
        R2_out
    }


    # partition R2
    part_R2s <- function(mod, expct) {
        # calculate full model R2
        R2_full <- R2_pe(mod, expct)
        if (!partition) return(R2_full)
        # calculate R2s of reduced models and difference with full model
        R2s_red <- purrr::map_df(all_comb, R2_of_red_mod, mod, R2_pe, expct) %>%
                   dplyr::mutate(R2 = R2_full$R2 - .data$R2) %>%
                   dplyr::bind_rows(R2_full, .)
    }

    # calculate R2 and partial R2s
    R2_org <- part_R2s(mod, expct)

    # SC to be discussed
    # # structure coefficients function
    SC_pe <- function(mod) {
        # question: always return all structure coefficients?
        # if (is.null(partvars)) return(data.frame(no_partvars = NA))
        Yhat <- stats::predict(mod)
        mod_mat <-stats::model.matrix(mod)
        # only calculate SC for partvars
        mod_mat <- mod_mat[, colnames(mod_mat) != "(Intercept)", drop=FALSE]
        #message("SC for factors with more than two levels do not make sense at the moment")
        out <- data.frame(stats::cor(Yhat, mod_mat))
    }

    # structure coefficients
    SC_org <- SC_pe(mod)

    # parametric bootstrapping
    if (!is.null(nboot)) {

        # simulation new responses
        if (nboot > 0)  Ysim <- as.data.frame(stats::simulate(mod, nsim = nboot))
        # main bootstrap function
        bootstr <- function(y, mod, expct) {
            # at the moment lme4 specific, could be extended
            mod_iter <- lme4::refit(mod, newresp = y)
            out_r2s <- part_R2s(mod_iter, expct)
            out_scs <- SC_pe(mod_iter)
            out_ests <- broom.mixed::tidy(mod_iter)
            out <- list(r2s = out_r2s, ests = out_ests, scs = out_scs)
        }
        # capture warnings and messages
        bootstr_quiet <- purrr::quietly(bootstr)

        # refit model with new responses
        if (!parallel) {
            boot_r2s_scs_ests <- pbapply::pblapply(Ysim, bootstr_quiet, mod, expct)
           # boot_r2s_scs <- future_map(Ysim, bootstr_quiet, mod, .progress = TRUE)
        }

        if (parallel) {
            if (is.null(ncores)) ncores <- parallel::detectCores()-1
            future::plan(future::multiprocess, workers = ncores)
            boot_r2s_scs_ests <- furrr::future_map(Ysim, bootstr_quiet, mod, expct)
        }

        # reshaping bootstrap output
        # put all commonality coefficients in one data.frame
        boot_r2s <- purrr::map(boot_r2s_scs_ests , function(x) x$result[["r2s"]]) %>%
                    purrr::map_df(function(x) stats::setNames(as.data.frame(t(x[[1]])), part_terms))
        # put all structure coefficients in a data.frame
        boot_scs <- purrr::map_df(boot_r2s_scs_ests, function(x) x$result[["scs"]])
        # put all model estimates in a data.frame
        boot_ests <- purrr::map(boot_r2s_scs_ests, function(x) x$result[["ests"]])
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
        boot_ests <- model_ests_full %>%
                        dplyr::mutate(estimate = NA) %>%
                        # cut off std.err and statistic from broom output
                        dplyr::select(-.data$std.error, -.data$statistic) %>%
                        list(sim1 = .)
        boot_warnings <- character(0)
        boot_messages <- character(0)
    }

    # calculate CIs
    r2_cis <- purrr::map_df(boot_r2s, calc_CI, CI, .id = "parts") %>%
              tibble::add_column(R2 = as.numeric(unlist(R2_org)), .after = "parts")

    ests_cis <- purrr::map_df(boot_ests, "estimate") %>%
                purrr::pmap_df(function(...) calc_CI(c(...), CI)) %>%
                cbind(model_ests_full[1:4], .)

    sc_cis <- purrr::map_df(boot_scs, calc_CI, CI, .id = "parts") %>%
              tibble::add_column(SC = as.numeric(SC_org), .after = "parts")

    res <- list(call = mod@call,
                #datatype = "gaussian",
                R2_type = R2_type,
                R2_pe_ci =  r2_cis,
                SC_pe_ci =  sc_cis,
                Ests_pe_ci =  ests_cis,
                R2_boot =   boot_r2s,
                SC_boot = boot_scs,
                Ests_boot =   boot_ests,
                partvars = partvars,
                CI = CI,
                boot_warnings = boot_warnings ,
                boot_messages = boot_messages)

    class(res) <- "partR2"
    return(res)
}
