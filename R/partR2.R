#' Partition the R2 for Gaussian mixed models
#'
#' R2, commonality coefficients and structure coefficients for gaussian lme4 models.
#' @param mod merMod object fitted with lme4.
#' @param partvars Character vector specifying the predictors for which to partition the R2.
#' @param R2_type "marginal" or "conditional"
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
#' # Only R2 with CI
#' (R2 <- partR2(mod, R2_type = "marginal", nboot = 15, CI = 0.95))
#' # Partitioned R2
#' (R2 <- partR2(mod,  partvars = c("Treatment", "Sex", "Habitat"),
#'               R2_type = "marginal", nboot = 10, CI = 0.95))
#'
#' # Random slopes (fixed effect Treatment can't be removed separately)
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
#' mod <- glmer(cbind(Dark,Reddish) ~ Treatment + (1|Container) + (1|Population),
#'              data = BeetlesColour, family = binomial)
#' (R2 <- partR2(mod, partvars = c("Treatment"), R2_type = "marginal",  data = BeetlesColour,
#'              nboot = 5, CI = 0.95))
#'
#' @import dplyr
#' @export


partR2 <- function(mod, partvars = NULL, R2_type = "marginal", nboot = NULL, #data
                   CI = 0.95, parallel = FALSE, ncores = NULL){

    # initial checks
    if(!inherits(mod, "merMod")) stop("partR2 only supports merMod objects at the moment")
    partition <- ifelse(is.null(partvars), FALSE, TRUE)

    if (!is.null(nboot)) {
        if (nboot < 2) stop("nboot has to be greater than 1 or NULL")
    }

    if (!(R2_type %in% c("marginal", "conditional"))) stop("R2_type has to be marginal or conditional")

    if (isTRUE(parallel)) {
        parallel_option <- "multicore"
        if (is.null(ncores)) ncores <- parallel::detectCores()-1
    } else { # for bootMer
        parallel_option <- "no"
    }

    # checker whether partvars are fixed effects
    fixed_terms <- insight::find_terms(mod)$conditional
    if (!(all(partvars %in% fixed_terms))) stop("partvars have to be fixed effects")

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
    # mod_original <- mod
    data_original <- insight::get_data(mod)
    # terms
    all_terms <- insight::find_terms(mod)

    # check if cbind created a matrix as first data.frame column
    if (any(grepl("cbind", names(data_original)))) {
        data_original <- cbind(as.data.frame(data_original[[1]]),
                               data_original[2:ncol(data_original)])
    }

    # family
    fam <- insight::model_info(mod)$family

    # we probably shouldnt internally add OLRE to model Overdispersion anymore?
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

    # formula
    formula_full <- stats::formula(mod)

    # R2
    if (R2_type == "marginal") {
        R2_pe <- function(mod) {
            out <- as.data.frame(unclass(performance::r2(mod)))["R2_marginal"]
            rownames(out) <- NULL
            out
        }
    } else if (R2_type == "conditional") {
        R2_pe <- function(mod) {
            out <- as.data.frame(unclass(performance::r2(mod)))["R2_conditional"]
            rownames(out) <- NULL
            out
        }
    }


    # makes reduced mod R2s
    R2_red_mods <- function(partvar, mod) {

        # which variables to reduce?
        to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
        # reduced formula
        formula_red <- stats::update(formula_full, paste(". ~ . ", to_del, sep=""))
        # reduced model
        mod_red <-  stats::update(mod, formula. = formula_red, data = data_original) # could be done with model.frame(mod), but hard with proportion
        # reduced model R2
        R2_red <- R2_pe(mod_red)

    }

    # partition R2
    if (partition) {
        # R2 for the full model and each of its partitions
        part_R2s <- function(mod){
            R2_full_boot <- R2_pe(mod)
            R2s_red <- lapply(all_comb, R2_red_mods, mod)
            ##### this occasionally gives negative R2s but why?
            # calculate difference in R2 between full model and each partial model --> partial R2
            R2s <- do.call(rbind, c(list(R2_full_boot),
                                       lapply(R2s_red, function(x) R2_full_boot - x)))
        }
    }
    # or return only the R2 of the full model
    if (!partition) {
        part_R2s <- function(mod){
            R2s <- R2_pe(mod)
        }
    }

    # parametric bootstrapping
    if (!is.null(nboot)) {

        # simulation new responses
        if (nboot > 0)  Ysim <- as.data.frame(stats::simulate(mod, nsim = nboot))

        # main bootstrap function
        bootstr <- function(y, mod) {
            # at the moment lme4 specific, could be extended
            mod_iter <- lme4::refit(mod, newresp = y)
            out <- part_R2s(mod_iter)
            out
        }

        # refit model with new responses
        if (!parallel) {
            boot_r2s <- pbapply::pblapply(Ysim, bootstr, mod)
        }

        if (parallel) {
            cl <- parallel::makeCluster(ncores)
            parallel::clusterExport(cl, c("R2_pe", "bootstr", "part_R2s", "r2",
                                          "R2_red_mods", "all_comb", "formula_full",
                                          "lmer", "data_original"))
            boot_r2s <- parallel::parLapply(cl, Ysim, bootstr, mod)
            stopCluster(cl)
        }
    }
    # if no bootstrap only return one list element
    # the list element contains 1 row with NAs if no partition specified
    # and x + 1 elements if x partitions specified (+1 because full model)
    if (is.null(nboot)) {
        boot_r2s <- list(setNames(data.frame(
                         matrix(ncol = length(names(part_R2s(mod))),
                                nrow = ifelse(partition, length(all_comb) + 1, 1),
                                )), names(part_R2s(mod))))
    }

    # names for parts
    if (partition) {
        part_terms <- c("Full", unlist(lapply(all_comb, paste,   collapse = "+")))
    }
    if (!partition) part_terms <- "Full"

    # add names of parts to R2s
    boot_r2s <- lapply(boot_r2s, function(x) {
        x$parts <- part_terms
        x
    })

    # restructure into one data.frame per part
    boot_r2s_df <- do.call(rbind, boot_r2s)
    boot_r2s_list <- lapply(part_terms, function(x) {
            out <- boot_r2s_df[boot_r2s_df$parts == x, ]
            out$parts <- NULL
            out
        })
    names(boot_r2s_list) <- part_terms

    # calculate CIs
    calc_CIs_per_col <- function(part) {
        #col_names <- names(part)
        cis <- data.frame(as.list(unlist(lapply(part, calc_CI, CI))))
    }

    # put all CIs into a data.frame
    all_cis <- do.call(rbind, lapply(boot_r2s_list, calc_CIs_per_col))
    all_cis <- cbind(data.frame(parts = rownames(all_cis)), all_cis)
    rownames(all_cis) <- NULL

    # df with point estimates and CIs
    full_r2_df <- data.frame(part_R2s(mod), all_cis)
    full_r2_df <- full_r2_df[sort(names(full_r2_df))]

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
                #datatype = "gaussian",
                R2_type = R2_type,
                R2_pe_ci =  full_r2_df,
                R2_boot = boot_r2s_list,
                # CC_df = out,
                partvars = partvars,
                CI = CI)
    class(res) <- "partR2"
    return(res)
}
