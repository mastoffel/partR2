#' Pipe operator
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


#' Calculates / extracts variance components from random effects and random slopes
#'
#' This function uses the method from Paul Johnson to compute the average group
#' variance across the levels of a covariate.
#'
#' @param grname Random effect term, accessed by looping over the RE list produced by lme4::VarCorr().
#' @param var_comps A list. Output of the lme4::VarCorr() function.
#' @param mod An lme4 model object.
#' @keywords internal
#'
group_vars <- function(grname, var_comps, mod){
    # check whether component is a matrix (--> random slopes)
    if (sum(dim(var_comps[[grname]])) > 2 ){
        sigma <- var_comps[[grname]]
        # design matrix subsetted for the elements of sigma
        if (sum(colnames(sigma) %in% colnames(stats::model.matrix(mod))) != 2) stop("cannot remove
            the fixed effect for which also random slopes are estimated")
        Z <- stats::model.matrix(mod)[, colnames(sigma)]
        # average variance across covariate
        var_grname <- sum(rowSums((Z %*% sigma) * Z))/stats::nobs(mod)
    } else {
        var_grname <- as.numeric(var_comps[[grname]])
    }
    var_grname
}


#' Adds an observational level random effect to a model
#'
#'
#' @param mod merMod object.
#' @param dat The underlying data.frame
#' @keywords internal
#' @export
#'
model_overdisp <- function(mod, dat) {
    # family
    mod_fam <- stats::family(mod)[[1]]
    resp <- lme4::getME(mod, "y")
    data_original <- dat
    if (mod_fam == "poisson" | ((mod_fam == "binomial") & (length(table(resp)) > 2))) {
        # check if OLRE already there
        overdisp_term <- lme4::getME(mod, "l_i") == nrow(data_original)
        # if so, get variable name
        if (sum(overdisp_term) == 1) {
            overdisp <- names(overdisp_term)[overdisp_term]
            names(data_original[overdisp]) <- "overdisp"
            message("As an overdispersion term has been fitted already,
it has been renamed to 'overdisp' for consistency")
        } else if ((sum(overdisp_term) == 0)) {
            data_original$overdisp <- as.factor(1:nrow(data_original))
            mod <- stats::update(mod, . ~ . + (1 | overdisp), data = data_original)
            message("An observational level random-effect has been fitted
to account for overdispersion.")
        }
    }
    out <- list(mod = mod, dat = data_original)
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
#' @keywords internal
#' @return R2 of reduced model.
#' @export
#'
R2_of_red_mod <- function(partvar, mod, R2_pe, expct) {

    # which variables to reduce?
    to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
    # reduced formula
    formula_red <- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))
    # fit reduced model
    mod_red <-  stats::update(mod, formula. = formula_red)
    # reduced model R2
    R2_red <- R2_pe(mod_red, expct)

}






