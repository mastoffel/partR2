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
#' @importFrom rlang .data
#' @importFrom dplyr `%>%`
#' @import tibble
#' @export


partR2 <- function(mod, partvars = NULL, R2_type = "marginal", cc_level = NULL,
                   nboot = NULL, CI = 0.95, parallel = FALSE, ncores = NULL){

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
    fixed_terms <- insight::find_terms(mod)$conditional
    # don't look at interactions for now
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
        remove_combs <- sapply(all_comb, function(x) length(x) > cc_level)
        all_comb[remove_combs] <- NULL
    }

    # names for partitions
    if (partition) {
        part_terms <- c("Full", unlist(lapply(all_comb, paste,   collapse = "+")))
    } else if (!partition) {
        part_terms <- "Full"
    }

    # extract some essential infor
    data_original <- insight::get_data(mod)
    formula_full <- stats::formula(mod)
    model_ests_full <- broom.mixed::tidy(mod)

    # check if cbind created a matrix as first data.frame column
    ## this has to be checked
    if (any(grepl("cbind", names(data_original)))) {
        data_original <- cbind(as.data.frame(data_original[[1]]),
                               data_original[2:ncol(data_original)])
    }

    # Give hint to OLRE when there is overdispersion
    mod_fam <- stats::family(mod)[[1]]
    resp <- insight::get_response(mod)
    # check if poisson on binomial and not binary
    if (mod_fam == "poisson" | ((mod_fam == "binomial") & (length(table(resp)) > 3))) {
       od <- (performance::check_overdispersion(mod))
       if (od$p_value < 0.05) {
           message(
               paste(
                   "Overdispersion detected with dispersion ratio = ",
                   round(od$dispersion_ratio, 2), " Pearson's Chi-Squared = ",
                   round(od$chisq_statistic, 2), " and p-val =", round(od$p_value, 2),
                   ". See check_overdispersion() from the performance package",
                   " for more infos. We suggest adding an Observational Level ",
                   " Random Effect or to use a different distribution.",
                   sep = ""
               )
           )
       }
    }

    # we probably shouldnt internally add OLRE to model Overdispersion anymore? -------
    # Overdispersion <- factor(1:stats::nobs(mod))
    # if (fam == "poisson") {
    #     mod <- model_overdisp(mod, data_original) # add OLRE and refit
    #     data <- cbind(data_original, Overdispersion)
    # }
    # binary_resp <- FALSE
    # if (fam == "binomial") {
    #     # check for binary, if not binary, add OLRE // double check whether this is sensible
    #     if ((length(unique(stats::na.omit(mod@resp$y))) < 3)) {
    #         binary_resp <- TRUE
    #     } else {
    #         mod <- model_overdisp(mod, data_original)
    #         data <- cbind(data_original, Overdispersion)
    #     }
    # }
    #-------------------------------------------------------------------------

    # R2
    R2_pe <- function(mod) {

        # get variance components
        var_comps <- as_tibble(insight::get_variance(mod))

        # check whether var comps have been calculated
        main_comps <- c("var.fixed", "var.random", "var.residual")
        var_comp_miss <- purrr::map_lgl(main_comps,
                                function(x) is.null(var_comps[[x]])) %>%
                         stats::setNames(main_comps)

        if (any(var_comp_miss)) {
            warning(paste0(
                "Variance component ",  main_comps[var_comp_miss],
                " could not be estimated"
            ))
            if (var_comp_miss["var.random"]) {
                var_comps$var.random <- 0
                warning("R2 estimated with ~0 random effect variance")
            }
            if (R2_type == "marginal") return(data.frame(R2_marginal = NA))
            if (R2_type == "conditional") return(data.frame(R2_conditional = NA))
        }

        if (R2_type == "marginal") {
            R2_out <- var_comps %>%
                mutate(R2_marginal = var.fixed /(var.fixed + var.random + var.residual)) %>%
                dplyr::select(R2_marginal)
        } else if (R2_type == "conditional") {
            R2_out <- var_comps %>%
                mutate(R2_conditional = (var.fixed + var.random) /
                           (var.fixed + var.random + var.residual)) %>%
                dplyr::select(R2_marginal)
        }

    # reduced model R2 (mod without partvar)
    R2_red_mods <- function(partvar, mod, formula_full) {

        # which variables to reduce?
        to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
        # reduced formula
        formula_red <- stats::update(formula_full, paste(". ~ . ", to_del, sep=""))
        # reduced model
        mod_red <-  stats::update(mod, formula. = formula_red)
        # reduced model R2
        R2_red <- R2_pe(mod_red)

    }

    # partition R2
    part_R2s <- function(mod) {
        # calculate full model R2
        R2_full <- R2_pe(mod)
        if (!partition) return(R2_full)
        # calculate R2s of reduced models
        R2s_red <- lapply(all_comb, R2_red_mods, mod, formula_full)
        # calculate partial R2 by substracting reduced R2s
        R2s <- do.call(rbind, c(list(R2_full),
                                lapply(R2s_red, function(x) R2_full - x)))
    }

    # calculate R2 and partial R2s
    R2_org <- part_R2s(mod)

    # structure coefficients function
    SC_pe <- function(mod) {
        # question: always return all structure coefficients?
        # if (is.null(partvars)) return(data.frame(NA))
        Yhat <- stats::predict(mod)
        mod_mat <- data.frame(stats::model.matrix(mod))
        # only return SCs specified in partvars
        #pred_ind <- unlist(sapply(partvars, function(x) grep(x, names(mod_mat))))

        if (length(grep("Intercept", names(mod_mat))) != 0){
            pred_ind <- names(mod_mat)[-c(grep("Intercept", names(mod_mat)))]
        } else {
            pred_ind <- names(mod_mat)
        }

        #names(mod_mat[, grep(x, names(mod_mat))])
        out <- data.frame(stats::cor(Yhat, mod_mat[pred_ind]))
        out

    }

    # structure coefficients
    SC_org <- SC_pe(mod)

    # parametric bootstrapping
    if (!is.null(nboot)) {

        # simulation new responses
        if (nboot > 0)  Ysim <- as.data.frame(stats::simulate(mod, nsim = nboot))
        # main bootstrap function
        bootstr <- function(y, mod) {
            # at the moment lme4 specific, could be extended
            mod_iter <- lme4::refit(mod, newresp = y)
            out_r2s <- part_R2s(mod_iter)
            out_scs <- SC_pe(mod_iter)
            out_ests <- broom.mixed::tidy(mod_iter)  # %>% mutate(term=ifelse(grepl("sd__(Int",term,fixed=TRUE),
            #                                                                 paste(group,term,sep="."),
            #                                                                 term))
            # maybe change to list columns here?
            out <- list(r2s = out_r2s, ests = out_ests, scs = out_scs)
        }
        # capture warnings and messages
        bootstr_quiet <- purrr::quietly(bootstr)

        # refit model with new responses
        if (!parallel) {
            boot_r2s_scs <- pbapply::pblapply(Ysim, bootstr_quiet, mod)
        }

        if (parallel) {
            cl <- parallel::makeCluster(ncores)
            parallel::clusterEvalQ(cl, list(library(lme4), library(insight)))
            parallel::clusterExport(cl, c("part_R2s", "R2_pe", "R2_red_mods",
                                          "all_comb", "formula_full", "data_original",
                                          "SC_pe", "partvars", "R2_type", "partition"),
                                          envir=environment())
            cat("Starting bootrapping in parallel (no progress bar): \n")
            boot_r2s_scs <- parallel::parLapply(cl, Ysim, bootstr_quiet, mod)
            parallel::stopCluster(cl)
        }

        # reshaping bootstrap output
        # put all commonality coefficients in one data.frame
        boot_r2s <- lapply(boot_r2s_scs, function(x) x$result[["r2s"]]) %>%
                        lapply(function(x) stats::setNames(as.data.frame(t(x[[1]])), part_terms)) %>%
                        dplyr::bind_rows()
        # put all structure coefficients in a data.frame
        boot_scs <-  dplyr::bind_rows(lapply(boot_r2s_scs, function(x) x$result[["scs"]]))
        # put all model estimates in a data.frame
        boot_ests <- lapply(boot_r2s_scs, function(x) x$result[["ests"]])
        # warnings and messages
        boot_warnings <- purrr::map(boot_r2s_scs, function(x) x$warnings)
        boot_messages <- purrr::map(boot_r2s_scs, function(x) x$messages)
    }

    # if no bootstrap return same data.frames only with NA
    if (is.null(nboot)) {
        boot_r2s <- rep(NA, length(part_terms)) %>%
                            as.list() %>%
                            as.data.frame() %>%
                            stats::setNames(part_terms)
        boot_scs <- matrix(nrow = 1, ncol = ncol(SC_org)) %>%
                     as.data.frame %>%
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
    r2_cis <- lapply(boot_r2s, calc_CI, 0.95) %>%
        dplyr::bind_rows(.id = "parts") %>%
        cbind(R2_org) %>%
        .[c(1,4,2,3)] # sort name and then point estimate first

    ests_cis <- lapply(boot_ests, function(x) x$estimate) %>%
                dplyr::bind_rows() %>%
                apply(1, calc_CI, 0.95) %>%
                dplyr::bind_rows() %>%
                cbind(model_ests_full[1:4], .)

    sc_cis <- lapply(boot_scs, calc_CI, 0.95) %>%
        dplyr::bind_rows(.id = "parts") %>%
        dplyr::mutate(SC = unlist(SC_org)) %>%
        .[c(1,4,2,3)]

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
