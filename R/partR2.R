#' Partition the R2 for Gaussian mixed models
#'
#' R2, commonality coefficients and structure coefficients for gaussian lme4 models.
#' @param mod merMod object fitted with lme4.
#' @param partvars Character vector specifying the predictors for which to partition the R2.
#' @param data dataframe used for lme4 model fit.
#' @param R2_type "marginal" or "conditional"
#' @param nboot Number of parametric bootstraps for interval estimation
#'        (defaults to NULL). Larger numbers of bootstraps give a better
#'        asymtotic CI, but may be time-consuming. Bootstrapping can be switch on by setting
#'        \code{nboot = 1000}.
#' @param CI Width of the required confidence interval between 0 and 1 (defaults to
#'        0.95).
#' @param parallel If TRUE, computation runs in parallel, leaving one CPU free, except ncpus is specified.
#' @param ncpus number of cpus for parallel computation
#'
#'
#' @return
#' Returns an object of class \code{partR2} that is a a list with the following elements:
#' \item{call}{model call}
#' \item{datatype}{GLMM Family.}
#' \item{R2_type}{Marginal or conditional R2}
#' \item{R2_pe_ci}{R2 and confidence intervals for full model and partitions}
#' \item{R2_boot}{Parametric bootstrap samples for R2 for full model and partitions}
#' \item{partvars}{predictors to partition}
#' \item{CI}{Coverage of the confidence interval as specified by the \code{CI} argument.}
# \item{CC_df}{Commonality coefficients (unique and common R2) and confidence intervals}
# \item{SC_df}{Structure coefficients (correlation between predicted response Yhat and the predictors specified in partvars) and confidence intervals}
# \item{SC_boot}{Parametric bootstrap samples for the SCs}
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
#'
#' # Gaussian data
#' mod <- lmer(BodyL ~ Sex + Treatment + Habitat + (1|Container) + (1|Population),
#'             data = BeetlesBody)
#' (R2 <- partR2(mod,  data = BeetlesBody, R2_type = "marginal", nboot = 5, CI = 0.95))
#' (R2 <- partR2(mod,  partvars = c("Treatment", "Sex", "Habitat"), data = BeetlesBody,
#'                    R2_type = "marginal", nboot = 5, CI = 0.95))
#'
#' # Random slope (fixed effect Treatment can't be removed separately)
#' mod <- lmer(BodyL ~ Treatment + Sex + (1 + Treatment|Population),
#'              data=BeetlesBody)
#' (R2 <- partR2(mod, partvars = c("Sex"), data = BeetlesBody,
#'                    R2_type = "marginal"))
#'
#' # poisson data
#' data(BeetlesFemale)
#' mod <- glmer(Egg ~ Treatment + Habitat + (1|Container) + (1|Population),
#'                   data = BeetlesFemale, family = poisson)
#' (R2 <- partR2(mod, partvars = c("Treatment", "Habitat"), data = BeetlesFemale,
#'               R2_type = "marginal", nboot = 5, CI = 0.95))
#'
#' # binomial data
#'
#' # binary
#' data(BeetlesMale)
#' mod <- glmer(Colour ~ Treatment + (1|Container) + (1|Population), data = BeetlesMale,
#'              family = binomial)
#' (R2 <- partR2(mod, partvars = c("Treatment"), R2_type = "marginal", data = BeetlesMale,
#'              nboot = 5, CI = 0.95))
#'
#' # proportion
#' BeetlesMale$Dark <- BeetlesMale$Colour
#' BeetlesMale$Reddish <- (BeetlesMale$Colour-1)*-1
#' BeetlesColour <- aggregate(cbind(Dark, Reddish) ~ Treatment + Population + Container,
#'      data=BeetlesMale, FUN=sum)
#'
#' mod <- glmer(cbind(Dark, Reddish) ~ Treatment + (1|Container) + (1|Population),
#'              data = BeetlesColour, family = binomial)
#' (R2 <- partR2(mod, partvars = c("Treatment"), R2_type = "marginal",  data = BeetlesColour,
#'              nboot = 5, CI = 0.95))
#'
#' @export


partR2 <- function(mod, partvars = NULL, data = NULL, R2_type = "marginal", nboot = NULL,
                               CI = 0.95, parallel = FALSE, ncpus = NULL){

    # should be possible without providing data, but not sure how at the moment,
    # mainly because of proportion binomial

    if (is.null(data)) stop("please provide the original dataframe used to fit the model")
    if(!inherits(mod, "merMod")) stop("partR2 only supports merMod objects at the moment")
   # if (is.null(partvars)) stop("partvars has to contain the variables for the commonality analysis")
    if (!is.null(nboot)) {
        if (nboot < 2) stop("nboot has to be greater than 1")
    }
    if (isTRUE(parallel)) {
        parallel_option <- "multicore"
        if (is.null(ncpus)) ncpus <- parallel::detectCores()-1
    } else { # for bootMer
        parallel_option <- "no"
    }

    # create list of all unique combinations except for the full model
    if (!is.null(partvars)) {
        if (length(partvars > 1)){
            all_comb <- unlist(lapply(1:(length(partvars)),
                function(x) utils::combn(partvars, x, simplify = FALSE)),
                recursive = FALSE)
        } else {
            all_comb <- as.list(partvars)
        }
    }

    # full model
    mod_original <- mod
    data_original <- data

    # family
    fam <- family(mod)$family

    # add OLRE to model Overdispersion
    Overdispersion <- factor(1:nobs(mod))
    if (fam == "poisson") {
        mod <- model_overdisp(mod, data) # add OLRE and refit
        data <- cbind(data_original, Overdispersion)
    }
    binary_resp <- FALSE
    if (fam == "binomial") {
        # check for binary, if not binary, add OLRE // double check whether this is sensible
        if ((length(unique(stats::na.omit(mod@resp$y))) < 3)) {
            binary_resp <- TRUE
        } else {
            mod <- model_overdisp(mod, data)
            data <- cbind(data_original, Overdispersion)
        }

    }

    # formula
  formula_full <- stats::formula(mod)

    R2_pe <- function(mod) {

        # random slopes
        # This code to calc random effect variances is specific to lme4 at the moment. Maybe change in the future
        var_comps <- lme4::VarCorr(mod)
        # group_vars handles the random slopes
        var_rand <- unlist(lapply(names(var_comps), group_vars, var_comps, mod))
        names(var_rand) <- names(var_comps)
        # residual / distribution specific variance
        var_res <- calc_var_r(mod)
        # add OLRE variance if applicable and seperate overdispersion from other random effects
        if (any(names(var_rand) == "Overdispersion")) {
            var_overdisp <- unname(var_rand["Overdispersion"])
            var_rand <- var_rand[!(names(var_rand) == "Overdispersion")]
            var_res <- var_res + var_overdisp
        }

        # random effect variances and residual variance with broom
        # var_comps <- tidy(mod, effects = "ran_pars", scales = "vcov")

        # Calculation of the variance in predicted values based on fixed effects alone
       # var_fix <- stats::var(broom.mixed::augment(mod)[, ".fixed"])
        var_fix <- stats::var(stats::predict(mod, re.form=NA))
        # denominator variance
        var_d <- sum(var_rand) + var_res + var_fix

        # R2 marginal
        if (R2_type == "marginal") {
            R2 <- var_fix / var_d
        }  else if (R2_type == "conditional") {
            R2 <- (var_fix + sum(var_rand)) / var_d
        }

        R2
    }

    # calc R2 for the full model
   # R2_full <- R2_pe(mod)

    # makes reduced mod R2s
    R2_red_mods <- function(partvar, mod) {

        # which variables to reduce?
        to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
        # reduced formula
        formula_red <- stats::update(formula_full, paste(". ~ . ", to_del, sep=""))
        # reduced model
        mod_red <-  stats::update(mod, formula. = formula_red, data = data) # could be done with model.frame(mod), but hard with proportion
        # reduced model R2
        R2_red <- R2_pe(mod_red)
        # out <- setNames(R2_red, paste(partvar, collapse = "_"))

    }


    if (partition) {
        part_R2s <- function(mod){
            R2_full_boot <- R2_pe(mod)
            R2s_red <- lapply(all_comb, R2_red_mods, mod)
            # give back bootstraps from full model and the unique contribution of each part,
            # which are the unique fixed predictors and their combinations
            ##### this occasionally gives negative R2s but why? --> R2 must decrease when adding predictor
            R2s <- c(R2_full_boot, R2_full_boot - unlist(R2s_red))
        }
    }
    if (!partition) {
        part_R2s <- function(mod){
            R2_full_boot <- R2_pe(mod)
        }
    }





    # parametric bootstrapping // which type of parametric bootstrapping? see help file, two possibilities
    if (!is.null(nboot)) {
        booted_r2s <- bootMer(mod,  part_R2s, nsim = nboot, type = "parametric", .progress = "txt",
            PBargs = list(style=3), parallel = parallel_option, ncpus = ncpus)
        # make dataframe,
        # booted r2 columns get names according to the variance components they represent
        if (partition) {
            booted_r2s_df <- setNames(as.data.frame(booted_r2s$t),
                c("Full", unlist(lapply(all_comb, paste, collapse = "+"))))
        } else {
            booted_r2s_df <- setNames(as.data.frame(booted_r2s$t), "Full")
        }


        full_r2_df <- data.frame(R2 = part_R2s(mod), do.call(rbind, lapply(booted_r2s_df, calc_CI, CI))) %>%
                        tibble::rownames_to_column(var = "model_part")

    } else {
        booted_r2s_df <- NA
        if (partition) {
            full_r2_df <- data.frame(model_part = c("Full", sapply(all_comb, paste, collapse = "+")),
                      R2 = part_R2s(mod), lower_ci = NA, upper_ci = NA)
        } else {
            full_r2_df <- data.frame(model_part = c("Full"),
                      R2 = part_R2s(mod), lower_ci = NA, upper_ci = NA)
        }
    }

    ## Structure coefficients
    # # predicted response
    # Yhat <- stats::predict(mod)
    # # Structure coefficients
    # SC_pe <- function(Yhat) {
    #     mod_mat <- data.frame(stats::model.matrix(mod))
    #     pred_ind <- unlist(sapply(partvars, function(x) grep(x, names(mod_mat))))
    #     # how to calc structure coefficient for categorical variables? Still correlation?
    #     out <- data.frame(stats::cor(Yhat, mod_mat[pred_ind]))
    #     out
    # }
    #
    # SC <- SC_pe(Yhat)
    # # prepare in case of no bootstrapping
    # SC_boot <- NA
    # SC_CI_temp <- matrix(ncol = length(names(SC)), nrow = 2, dimnames = list(c("lower", "upper")))
    #
    # cat("Bootstrapping progress for the structure coefficients: \n")
    # if (!is.null(nboot)){
    #     SC_boot <- do.call(rbind, pbapply::pbapply(Ysim_full, 2, SC_pe))
    #     SC_CI_temp <- data.frame(apply(SC_boot, 2, calc_CI))
    # }
    # SC_df <- data.frame("pred" = names(SC),"SC" = t(SC), t(SC_CI_temp), row.names = NULL)


    res <- list(call = mod@call,
        datatype = "gaussian",
        R2_type = R2_type,
        R2_pe_ci =   full_r2_df,
        R2_boot = booted_r2s_df,
        # CC_df = out,
        partvars = partvars,
        CI = CI)
    class(res) <- "partR2"
    return(res)
}
