#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Captures and suppresses (still to find out why) warnings of an expression
#'
#' This function is used within partR2 to capture lme4 model fitting warnings in the
#' bootstrap and permutation procedures.
#'
#' @param expr An expression, such as the sequence of code used by rptR to calculate
#' bootstrap or permutation estimates
#' @keywords internal

with_warnings <- function(expr) {
    myWarnings <- NULL
    myMessages <- NULL
    wHandler <- function(w) {
        myWarnings <<- c(myWarnings, list(w))
        invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(expr, warning = wHandler)
    list(warnings = myWarnings)
}


#' Adds an observational level random effect to a model
#'
#'
#' @param mod merMod object.
#' @param dat The underlying data.frame
#' @keywords internal
#' @export
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
#' @keywords internal
#'
#
# CI function
calc_CI <- function(x, CI) {
    out <- stats::quantile(x, c((1 - CI)/2, 1 - (1 - CI)/2), na.rm = TRUE)
    out <- as.data.frame(t(out))
    names(out) <- c("CI_lower", "CI_upper")
    rownames(out) <- NULL
    out
}


# reduced model R2 (mod without partvar)

#' Calculate R2 from a reduced model
#'
#' @param partvar One or more fixed effect variables which are taken out
#' of the model.
#' @param mod merMod object.
#' @param R2_pe R2 function.
#' @param data Data.frame to fit the model
#' @param expct Expectation
#' @param overdisp_name Name of overdispersion term
#' @keywords internal
#' @return R2 of reduced model.
#' @export
#'
R2_of_red_mod <- function(partvar, mod, R2_pe, dat, expct, overdisp_name) {


    # which variables to reduce?
    to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
    # reduced formula
    formula_red <- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))

    # check if old and new formula are different and hence the partvar does
    # not exist
    formula_terms <- attr(stats::terms(stats::formula(mod)), "term.labels")
    formula_terms_red <- attr(stats::terms(formula_red), "term.labels")
    if (all(formula_terms %in% formula_terms_red)) {
        stop(paste0("partvar ", partvar, " not found in the model formula"))
    }

    # fit reduced model
    mod_red <-  stats::update(object = mod, formula. = formula_red, data = dat)
    # reduced model R2
    R2_red <- R2_pe(mod_red, expct, overdisp_name)

}


#' Get numerator dfs for reduced models
#'
#' @param partvar One or more fixed effect variables which are taken out
#' of the model.
#' @param mod merMod object.
#' @param data Data.frame to fit the model
#' @keywords internal
#' @return Numerator degrees of freedom
#' @export
#'
get_ndf <- function(partvar, mod, dat) {

    if (("Full" %in% partvar)&(length(partvar) == 1)) return(ncol(stats::model.matrix(mod)))

    # which variables to reduce?
    to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
    # reduced formula
    formula_red <- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))
    # fit reduced model
    mod_red <-  stats::update(mod, formula. = formula_red, data = dat)
    ncol(stats::model.matrix(mod_red))

}

#' Get beta weights
#'
#' @param ests tidy model output from broom.mixed
#' @param mod merMod object.
#' @keywords internal
#' @return tidy output with bw instead of raw estimates
#' @export
#'
#'

get_bw <- function(mod){

    mod_mat <- stats::model.matrix(mod) %>%
                    as.data.frame() %>%
                    dplyr::select(-"(Intercept)")
    bin_preds <- purrr::map_lgl(mod_mat, function(x) length(table(x))<=2)
    resp <- lme4::getME(mod, "y")
    sds <- purrr::map_dbl(mod_mat, stats::sd, na.rm = TRUE)
    # only for gaussian dividing by sd of response
    if (stats::family(mod)$family == "gaussian") sds <- sds/stats::sd(resp, na.rm = TRUE)
    # only standardise for non-factors
    sds[bin_preds] <- 1
    # simple posthoc standardisation. Doesn't work for interactions and should
    # be turned of when standardised before
    ests <- broom.mixed::tidy(mod, effects = "fixed")
    ests[ests$term %in% names(sds), "estimate"] <-
        purrr::map_dbl(names(sds), function(x) {
            unlist(ests[ests$term %in% x, "estimate"] * sds[x])
        })
    ests
}

#Adds an observational level random effect to a model
#
#
##' @param mod merMod object.
##' @param dat The underlying data.frame
##' @keywords internal
##' @export
##'
#model_overdisp2 <- function(mod) {
#    # family
#    mod_fam <- stats::family(mod)[[1]]
#    resp <- lme4::getME(mod, "y")
#
#    if (mod_fam == "poisson" | ((mod_fam == "binomial") & (length(table(resp)) > 2))) {
#        # check if OLRE already there
#        overdisp_term <- lme4::getME(mod, "l_i") == stats::nobs(mod)
#        # if so, get variable name
#        if (sum(overdisp_term) == 1) {
#            overdisp_name <- names(overdisp_term)[overdisp_term]
#            mod <- stats::update(mod, eval(paste(". ~ . ", paste("- (1 |", overdisp_name, ")"))))
#            overdisp <- as.factor(1:stats::nobs(mod))
#            mod <- stats::update(mod, . ~ . + (1 | overdisp))
#            # rename OLRE to overdisp if not done so already
#            message("The OLRE or overdispersion term has been renamed to 'overdisp',
#                    it is recommended to call it overdisp from the start.")
#        } else if ((sum(overdisp_term) == 0)) {
#            overdisp <- as.factor(1:stats::nobs(mod))
#            mod <- stats::update(mod, . ~ . + (1 | overdisp))
#            message("An observational level random-effect has been fitted
#to account for overdispersion.")
#        }
#    }
#    return(mod)
#}


