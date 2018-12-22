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
#' # load data
#' data(BeetlesFemale)
#' library(lme4)
#' # Note: nboot and npermut are set to 0 for speed reasons.
#'
#' # estimating adjusted repeatabilities for two random effects
#' mod <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population),
#'                   data = BeetlesFemale, family = poisson)
#'
#' R2 <- partPoisson_broom(mod, partvars = c("Treatment", "Habitat"),
#'                    R2_type = "marginal", nboot = 20, CI = 0.95,
#'                    link = "log", expect = "meanobs")
#' R2
#'
#' # unadjusted repeatabilities with  fixed effects and
#' # estimation of the fixed effect variance
#' rptPoisson(Egg ~ Treatment + (1|Container) + (1|Population),
#'                    grname=c("Container", "Population", "Fixed"),
#'                    data=BeetlesFemale, nboot=0, npermut=0, adjusted=FALSE)
#'
#'
#' # variance estimation of random effects, residual and overdispersion
#' rptPoisson(formula = Egg ~ Treatment + (1|Container) + (1|Population) ,
#'                    grname=c("Container","Population","Residual", "Overdispersion"),
#'                    data = BeetlesFemale, nboot=0, npermut=0, ratio = FALSE)
#'
#' @export

partPoisson_broom <- function(mod, partvars = NULL, R2_type = "marginal", nboot = NULL, CI = 0.95,
                              link = c("log", "sqrt"), expect="meanobs"){

    if (is.null(partvars)) stop("partvars has to contain the variables for the commonality analysis")
    if (!is.null(nboot)) {
        if (nboot < 2) stop("nboot has to be greater than 1")
    }

    # full model
   #  mod_full <- mod
    # formula
    formula_full <- stats::formula(mod)
    # extract data
    data <- model.frame(mod) ### check whats happening with NAs, probably already excluded
    # missing values
    no_NA_vals <- stats::complete.cases(data[all.vars(formula_full)])
    if (sum(!no_NA_vals ) > 0 ){
        warning(paste0(sum(!no_NA_vals), " rows containing missing values were removed"))
        data <- data[no_NA_vals, ]
    }

    # link
    if (length(link) > 1) link <- "log"
    if (!(link %in% c("log", "sqrt"))) stop("Link function has to be 'log' or 'sqrt'")

    # observational level random effect
    Overdispersion <- factor(1:nrow(data))
    data <- cbind(data, Overdispersion)
    # extend formula with obs-level rand-effect
    formula_ext <- stats::update(formula_full,  ~ . + (1|Overdispersion))
    mod_ext <- lme4::glmer(formula_ext, data = data, family = stats::poisson(link = link))

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
        out
    }

    R2_pe <- function(mod) {

        # complete model stats
        tidy_mod <- broom.mixed::tidy(mod)
        # random effect variances
        VarComps <- broom.mixed::tidy(mod, effects = "ran_pars", scales = "vcov")
        # variance of predicted fixed effects
        var_f <- stats::var(broom.mixed::augment(mod)[, ".fixed"])
        # intercept on link scale
        beta0 <- as.numeric(tidy_mod[tidy_mod$term == "(Intercept)", "estimate"])

        # Distribution-specific and residual variance
        if (link == "sqrt") {
            estdv_link = 0.25
            var_r <- VarComps[VarComps$group == "Overdispersion", "estimate"] + estdv_link
        }
        if (link == "log") {
            if(expect=="meanobs") EY <- mean(mod@resp$y, na.rm=TRUE) ### check whether this is legit
            if(expect=="latent") {
                message("When using 'latent', make sure predictors are centered or scaled.")
                EY <- exp(beta0 + (sum(VarComps$estimate) + var_f)/2)
            }
            estdv_link = log(1/EY+1)
            var_r <- VarComps[VarComps$group == "Overdispersion", "estimate"] + estdv_link
        }

        # summed up random effect variance
        var_p <- sum(VarComps$estimate)
        # full denominator variance
        var_d <- var_p + var_f + var_r
        # R2
        if  (R2_type == "marginal"){
            R2 <- var_f / var_d
        } else if (R2_type == "conditional") {
            R2 <- (var_f + var_p) / var_d
        }
        # from data.frame to numeric
        R2[["estimate"]]
        # R2c <- (var_f + var_p) / var_d

        # old way
        # VarCorr() extracts variance components
        # old way: VarComps_old <- lme4::VarCorr(mod)
        # Residual variance
        # var_e <- attr(VarComps, "sc")^2
        #var_e <-  VarComps[VarComps$group == "Residual", "estimate"]^2
        #names(var_e) <- "Residual"
        # Calculation of the variance in fitted values / Fixed effect variance
        # var_f <- stats::var(stats::predict(mod, re.form=NA))
        # intercept on link scale
        # old way: beta0 <- unname(lme4::fixef(mod)[1])

        # without random slopes
        # denominator variance
        # var_p <- sum(as.numeric(VarComps)) + attr(VarComps, "sc")^2

        # with random slopes
        #var_VarComps <- unlist(lapply(names(VarComps), group_vars, VarComps, mod))
        #var_p <- sum(as.numeric(var_VarComps)) + attr(VarComps, "sc")^2
    }

    # calc R2 for the full model
    R2 <- R2_pe(mod_ext)

    # confidence interval estimation by parametric bootstrapping
    # simulate matrix from which to draw y
    if (!is.null(nboot))  Ysim <- as.matrix(stats::simulate(mod_ext, nsim = nboot))

    # bootstrap R2 full model
    cat("Bootstrapping progress for the full model \n")
    R2_boot <- NA
    if (!is.null(nboot)){
        R2_boot <- pbapply::pbapply(Ysim, 2, function(x) R2_pe(lme4::refit(mod_ext, newresp = x)))
    }

    # R2 CI for full model
    R2_CI <- data.frame(t(calc_CI(R2_boot)))
    R2_df <- data.frame("R2" = R2, R2_CI)

    # predicted response / uses predict.merMod
    Yhat <- stats::predict(mod)
    # Structure coefficients
    SC_pe <- function(Yhat) {
        mod_mat <- data.frame(stats::model.matrix(mod))
        pred_ind <- unlist(sapply(partvars, function(x) grep(x, names(mod_mat))))
        # how to calc structure coefficient for categorical variables? Still correlation?
        out <- data.frame(stats::cor(Yhat, mod_mat[pred_ind]))
        out
    }

    SC <- SC_pe(Yhat)
    # prepare in case of no bootstrapping
    SC_boot <- NA
    SC_CI_temp <- matrix(ncol = length(names(SC)), nrow = 2, dimnames = list(c("lower", "upper")))

    cat("Bootstrapping progress for the structure coefficients: \n")
    if (!is.null(nboot)){
        SC_boot <- do.call(rbind, pbapply::pbapply(Ysim, 2, SC_pe))
        SC_CI_temp <- data.frame(apply(SC_boot, 2, calc_CI))
    }
    SC_df <- data.frame("pred" = names(SC),"SC" = t(SC), t(SC_CI_temp), row.names = NULL)


    # unique and common effects
    diff_R2 <- function(partvar, mod, R2) {
        # which variables to reduce?
        to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
        # reduced formula
        formula_red <- stats::update(formula_ext, paste(". ~ . ", to_del, sep=""))
        # reduced model
        mod_red <- stats::update(mod, formula_red)
        # reduced model R2
        R2_red <- R2_pe(mod_red)

        if (is.null(nboot)){
            R2_diff <- R2 - R2_red
            return(R2 = data.frame(R2_diff, "lower" = NA, "upper" = NA))
        }

        Ysim_red <- as.matrix(stats::simulate(mod_red, nsim = nboot))
        # bootstrap R2 red model
        cat(paste("Bootstrap progress for", paste(partvar, collapse = "&"), "\n"))
        R2_red_boot <- pbapply::pbapply(Ysim_red, 2, function(x) R2_pe(lme4::refit(mod_red, newresp = x)))
        #boot_R2_red <- pbapply::pbapply(Ysim_full, 2, function(x) R2_pe(lme4::refit(mod_red, newresp = x)))
        R2_diff <- R2 - R2_red
        # difference / question: inflated CI because of doubling the sampling variance (R2 original and reduced?)
        R2_boot_diff <- R2_boot - R2_red_boot
        R2_diff_CI <- as.data.frame(t(calc_CI(R2_boot_diff)))
        names(R2_diff_CI) <- c("lower", "upper")
        out <- data.frame(R2_diff, R2_diff_CI)
    }


    R2_out <- do.call(rbind, lapply(all_comb, diff_R2, mod, R2))
    # all_vars <- lapply(all_comb3, function(x) gsub('(a|e|i|o|u)', '', x))
    all_comb_names <- unlist(lapply(all_comb, function(x) paste(x, collapse = " & ")))

    out <- data.frame("combinations" = all_comb_names, R2_out)

    res <- list(call = mod@call,
        datatype = "gaussian",
        R2_type = R2_type,
        R2_df = R2_full_df,
        R2_boot = R2_full_boot,
        CC_df = out,
        SC_df = SC_df,
        SC_boot = SC_boot,
        partvars = partvars,
        CI = CI)
    class(res) <- "partR2"
    return(res)
}
