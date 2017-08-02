#' Partition the R2 for Gaussian mixed models
#'
#' Uses commonality analyses to partition the R2 into variation unique to variables and common
#' between variables.
#'
#' @return
#' Returns an object of class \code{partR2} that is a a list with the following elements:
#' \item{R2}{Partitioned R2}
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
#'
#' mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
#'             data = BeetlesBody)
#'
#' R2 <- partGaussian(mod, partvars = c("Sex", "Treatment", "Habitat"),
#'                    type = "marginal", nboot = 20, CI = 0.95)
#' R2
#'
#' @export

partGaussian <- function(mod, partvars = NULL, type = "marginal", nboot = NULL, CI = 0.95){

    if (is.null(partvars)) stop("partvars has to contain the variables for the commonality analysis")
    if (!is.null(nboot) & nboot < 2) stop("nboot has to be greater than 1")

    # create list of all unique combinations except for the full model
    if (length(partvars > 1)){
        all_comb <- unlist(lapply(1:(length(partvars)-1),
                    function(x) utils::combn(partvars, x, simplify = FALSE)),
                    recursive = FALSE)
    } else {
        all_comb <- as.list(partvars)
    }

    # full model
    mod_full <- mod
    # formula
    formula_full <- formula(mod_full)

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

    # calc R2 for the full model
    R2_full <- R2_pe(mod_full)

    # confidence interval estimation by parametric bootstrapping
    # simulate matrix from which to draw y
    if (!is.null(nboot))  Ysim_full <- as.matrix(stats::simulate(mod_full, nsim = nboot))

    # bootstrap R2 full model
    boot_R2_full <- apply(Ysim_full, 2, function(x) R2_pe(refit(mod_full, newresp = x)))

    # CI function
    calc_CI <- function(x) {
        out <- stats::quantile(x, c((1 - CI)/2, 1 - (1 - CI)/2), na.rm = TRUE)
    }

    diff_R2 <- function(partvar, mod) {
        # which variables to reduce?
        to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
        # reduced formula
        formula_red <- stats::update(formula_full, paste(". ~ . ", to_del, sep=""))
        # reduced model
        mod_red <- stats::update(mod, formula_red)

        R2_red <- R2_pe(mod_red)

        if (is.null(nboot)){
            R2_diff <- R2_full - R2_red
            return(R2 = R2_diff, data.frame("CI_low" = NA, "CI_high" = NA))
        }

        Ysim_red <- as.matrix(stats::simulate(mod_red, nsim = nboot))
        # bootstrap R2 red model
        boot_R2_red <- apply(Ysim_red, 2, function(x) R2_pe(refit(mod_red, newresp = x)))

        R2_diff <- R2_full - R2_red
        # difference
        boot_R2_diff <- boot_R2_full - boot_R2_red
        R2_diff_CI <- as.data.frame(t(calc_CI(boot_R2_diff)))
        names(R2_diff_CI) <- c("CI_low", "CI_high")
        out <- data.frame(R2_diff, R2_diff_CI)
    }


    R2_out <- do.call(rbind, lapply(all_comb, diff_R2, mod))
    # all_vars <- lapply(all_comb3, function(x) gsub('(a|e|i|o|u)', '', x))
    all_comb_names <- unlist(lapply(all_comb, function(x) paste(x, collapse = " & ")))

    out <- data.frame("combinations" = all_comb_names, R2_out)

    res <- list(R2 = out)
    class(res) <- "partR2"
    return(res)
}
