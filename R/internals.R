#' Calculates / extracts variance components from random effects and random slopes
#'
#' This function uses the method from Paul Johnson to compute the average group
#' variance across the levels of a covariate.
#'
#' @param grname The name of a grouping factor, usually accessed by looping over the
#' grname argument of the rptR functions.
#' @param VarComps A list. Output of the lme4::VarCorr function.
#' @param mod An lme4 model object.
#' @keywords internal
#'
group_vars <- function(grname, VarComps, mod){
    # check whether component is a matrix (--> random slopes)
    if (sum(dim(VarComps[[grname]])) > 2 ){
        sigma <- VarComps[[grname]]
        # design matrix subsetted for the elements of sigma
        Z <- stats::model.matrix(mod)[, colnames(sigma)]
        # average variance across covariate
        var_grname <- sum(rowSums((Z %*% sigma) * Z))/stats::nobs(mod)
    } else {
        var_grname <- as.numeric(VarComps[[grname]])
    }
    var_grname
}
