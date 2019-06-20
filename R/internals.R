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
#' @param mod The model.
#' @param data The data.frame which has been provided to fit the model in lme4.
#' @keywords internal
#'
#
# model_overdisp <- function(mod) UseMethod("overdisp")
# overdisp.merMod
model_overdisp <- function(mod, data) {
    # data <- model.frame(mod)
    mod_formula <- stats::formula(mod)

    # check if it's already there
    if (!any(sapply(mod@flist, nlevels) == stats::nobs(mod))) {
        Overdispersion <- factor(1:stats::nobs(mod))
        data_overdisp <- cbind(data, Overdispersion)
        formula_overdisp <- stats::update(mod_formula,  ~ . + (1|Overdispersion))
        new_mod <- stats::update(mod, formula. = formula_overdisp, data = data_overdisp)
    }
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


#'

#' Calculates Distribution specific and Residual variance
#'
#'
#' @param mod fitted model
#' @param expect expectation in poisson and binomial models, atm only "meanobs"
#' @keywords internal
#'
calc_var_r <- function(mod, expect = "meanobs") {

    fam <- stats::family(mod)$family
    link <- stats::family(mod)$link
    var_comps <- lme4::VarCorr(mod)

    if (fam == "gaussian") {
        var_r <- attr(var_comps, "sc")^2
    }

    if (fam == "binomial") {
            if (!link %in% c("logit", "probit")) stop("link function has to be logit or probit")
            # Helper functions
            # inverf based on posting by sundar on R-help
            # https://stat.ethz.ch/pipermail/r-help/2006-June/108153.html
            inverf <- function(x) stats::qnorm((x + 1)/2)/sqrt(2)

            # Distribution-specific and Residual variance // expect at the moment only meanobs
            if (link == "logit") {
               # if(expect=="latent") Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(sum(var_VarComps)+var_f))^-1)
                if(expect=="meanobs") Ep <- mean(mod@resp$y, na.rm=TRUE)
                # if(expect=="liability") Ep <- exp(beta0) / (1 + exp(beta0))
                #if(expect=="latent") estdv_link <- 1 / (Ep*(1-Ep))
                if(expect=="meanobs") estdv_link <- 1 / (Ep*(1-Ep))
                #if(expect=="liability") estdv_link <- pi^2/3
                # var_r <-  var_VarComps["Overdispersion"] + estdv_link # old version with overdisp
                var_r <-  estdv_link
            }
            if (link == "probit") {
                #if(expect=="latent") Ep <- stats::pnorm(beta0*sqrt(1+sum(VarComps[,"vcov"])+var_f)^-1)
                if(expect=="meanobs") Ep <- mean(mod@resp$y, na.rm=TRUE)
                #if(expect=="latent") estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
                if(expect=="meanobs") estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
                #if(expect=="liability") estdv_link <- 1
                # var_r <- VarComps["Overdispersion", "vcov"] + estdv_link
                # var_r <-  var_VarComps["Overdispersion"] + estdv_link
                var_r <- estdv_link
            }
        }

    # needs OLRE
    if (fam == "poisson") {
        if (link == "sqrt") {
            estdv_link = 0.25
            var_r <- estdv_link
        }
        if (link == "log") {
            if(expect=="meanobs") EY <- mean(mod@resp$y, na.rm=TRUE)
            # if(expect=="latent") EY <- exp(beta0 + (sum(var_VarComps) + var_f)/2)
            estdv_link = log(1/EY+1)
            var_r <- estdv_link
        }
    }

    var_r


}





