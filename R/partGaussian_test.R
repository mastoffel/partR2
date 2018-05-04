#' Partition the R2 for Gaussian mixed models
#'
#' R2, commonality coefficients and structure coefficients for gaussian lme4 models.
#'
#' @param mod Gaussian merMod object from lme4::lmer.
#' @param partvars Character vector specifying the predictors for which to partition the R2.
#' @param R2_type "marginal" or "conditional"
#' @param nboot Number of parametric bootstraps for interval estimation
#'        (defaults to NULL). Larger numbers of bootstraps give a better
#'        asymtotic CI, but may be time-consuming. Bootstrapping can be switch on by setting
#'        \code{nboot = 1000}.
#' @param CI Width of the required confidence interval between 0 and 1 (defaults to
#'        0.95).
#'
#'
#' @return
#' Returns an object of class \code{partR2} that is a a list with the following elements:
#' \item{call}{model call}
#' \item{datatype}{Response distribution (here: 'Gaussian').}
#' \item{R2_type}{Marginal or conditional R2}
#' \item{R2_df}{R2 and confidence interval}
#' \item{R2_boot}{Parametric bootstrap samples for R2}
#' \item{CC_df}{Commonality coefficients (unique and common R2) and confidence intervals}
#' \item{SC_df}{Structure coefficients (correlation between predicted response Yhat and the predictors specified in partvars) and confidence intervals}
#' \item{SC_boot}{Parametric bootstrap samples for the SCs}
#' \item{partvars}{predictors to partition}
#' \item{CI}{Coverage of the confidence interval as specified by the \code{CI} argument.}
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
#' data(BeetlesBody)
#' library(lme4)
#' mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
#'             data = BeetlesBody)
#'
#' R2 <- partGaussian(mod, partvars = c("Sex", "Treatment", "Habitat"),
#'                    R2_type = "marginal", nboot = 100, CI = 0.95)
#' R2
#'
#' @export

partGaussian_test <- function(mod, partvars = NULL, R2_type = "marginal", nboot = NULL, CI = 0.95, verbose = TRUE){

    if (is.null(partvars)) stop("partvars has to contain the variables for the commonality analysis")
    if (!is.null(nboot)) {
        if (nboot < 2) stop("nboot has to be greater than 1")
    }

    # create list of all unique combinations except for the full model
    if (length(partvars > 1)){
        all_comb <- unlist(lapply(1:(length(partvars)),
            function(x) utils::combn(partvars, x, simplify = FALSE)),
            recursive = FALSE)
    } else {
        all_comb <- as.list(partvars)
    }

    # CI function
    calc_CI <- function(x) {
        out <- stats::quantile(x, c((1 - CI)/2, 1 - (1 - CI)/2), na.rm = TRUE)
        names(out) <- c("lower", "upper")
        out <- data.frame(t(out))
    }

    # full model
    mod_full <- mod
    # formula
    formula_full <- stats::formula(mod_full)

    R2_pe <- function(mod) {
        # VarCorr() extracts variance components
        VarComps <- lme4::VarCorr(mod)
        # Residual variance
        var_e <- attr(VarComps, "sc")^2
        names(var_e) <- "Residual"
        # Calculation of the variance in fitted values
        var_f <- stats::var(stats::predict(mod, re.form=NA))
        # same
        # VarF <- var(as.vector(fixef(mod) %*% t(mod@pp$X)))

        # without random slopes
        # denominator variance
        var_p <- sum(as.numeric(VarComps)) + attr(VarComps, "sc")^2
        # with random slopes
        #var_VarComps <- unlist(lapply(names(VarComps), group_vars, VarComps, mod))
        #var_p <- sum(as.numeric(var_VarComps)) + attr(VarComps, "sc")^2

        # full denominator variance
        var_p <- var_p + var_f
        # R2
        R2 <- var_f / var_p
    }

    # calculate R2 for the full model
    if (!is.null(nboot)) {
        # bootstrap R2
        R2_full_boot <- lme4::bootMer(mod, R2_pe, nsim = nboot, type = "parametric",
                                      verbose = verbose)
        R2_bootlist <-data.frame(R2_full_boot$t)
        R2_full_df <- data.frame("R2" = R2_full_boot$t0, calc_CI(R2_full_boot$t))
        # calc R2 for the full model
        R2_full <- R2_full_boot$t0
    } else {
        R2_full <- R2_pe(mod)
        R2_full_df <- data.frame("R2" = R2_full, "lower" = NA, "upper" = NA)
        R2_bootlist <- NA
    }



    # Structure coefficients
    SC_pe <- function(mod) {
        Yhat <- stats::predict(mod)
        # model matrix
        mod_mat <- data.frame(stats::model.matrix(mod))
        # grep partvars from model.matrix names -- could lead to problems due to naming
        pred_ind <- sapply(partvars, function(x) grep(x, names(mod_mat)))
        # correlation between yhat and the model matrix column
        out <- data.frame(stats::cor(Yhat, mod_mat[pred_ind]))
        stats::setNames(as.numeric(out), names(out))
    }
    # bootstrap
    if (!is.null(nboot)) {
        SC_boot <- lme4::bootMer(mod, SC_pe, nsim = nboot, type = "parametric", verbose = TRUE)
        SC_bootlist <- data.frame(SC_boot$t)
        # tidy data
        SC_all_CIs <- do.call(rbind, apply(SC_boot$t, 2, calc_CI))
        SC_df <- data.frame("pred" = names(SC_boot$t0), "SC" = SC_boot$t0, SC_all_CIs)
        rownames(SC_df) <- NULL
    } else {
        SC <- SC_pe(mod)
        SC_df <- data.frame("pred" = names(SC), "SC" = SC, "lower" = NA, "upper" = NA)
        SC_bootlist <- data.frame(matrix(ncol = length(SC)))
        names(SC_bootlist) <- names(SC)
    }


    # unique and common effects
    diff_R2 <- function(partvar, mod) {
        # which variables to reduce?
        to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
        # reduced formula
        formula_red <- stats::update(formula_full, paste(". ~ . ", to_del, sep=""))
        # reduced model
        mod_red <- stats::update(mod, formula_red)
        # reduced model R2
        R2_red <- R2_pe(mod_red)

        # without bootstrapping
        if (is.null(nboot)){
            R2_diff <- R2_full - R2_red
            return(R2 = data.frame(R2_diff, "lower" = NA, "upper" = NA))
        }

        # with bootstrapping
        # bootstrap R2 red model
        cat(paste("Bootstrap progress for", paste(partvar, collapse = "&"), "\n"))
        R2_red_boot <- lme4::bootMer(mod_red, R2_pe, nsim = nboot, type = "parametric",
            verbose = verbose)

        # R2 difference
        R2_diff <- R2_full - R2_red
        # differences between bootstraps
        boot_R2_diff <- R2_full_boot$t - R2_red_boot$t
        # calculate CIs
        R2_diff_CI <- calc_CI(boot_R2_diff)
    }

    # CCs
    CC_CIs <- do.call(rbind, lapply(all_comb, diff_R2, mod))
    all_comb_names <- unlist(lapply(all_comb, function(x) paste(x, collapse = " & ")))
    CC_df <- data.frame("combinations" = all_comb_names,  CC_CIs)

    res <- list(call = mod@call,
        datatype = "gaussian",
        R2_type = R2_type,
        R2_df = R2_full_df,
        R2_boot = R2_bootlist ,
        CC_df = CC_df,
        SC_df = SC_df,
        SC_boot = SC_bootlist,
        partvars = partvars,
        CI = CI)
    class(res) <- "partR2"
    return(res)
}
