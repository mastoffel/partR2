#' Calculate R2
#'
#' @param mod merMod object
#' @param expct Expectation
#' @param overdisp_name Name of overdispersion term
#' @param R2_type "marginal" or "conditional"
#'
#' @keywords internal
#' @return data.frame with one element, R2
#'
R2_pe <- function(mod, expct, overdisp_name, R2_type) {

    # get variance components
    var_comps <- get_var_comps(mod, expct, overdisp_name)

    if (R2_type == "marginal") {
        R2_out <- var_comps %>%
            dplyr::mutate(R2 = .data$var_fix /
                              (.data$var_fix + .data$var_ran + .data$var_res)) %>%
            dplyr::select(.data$R2)
    } else if (R2_type == "conditional") {
        R2_out <- var_comps %>%
            dplyr::mutate(R2 = (.data$var_fix  + .data$var_ran) /
                              (.data$var_fix + .data$var_ran + .data$var_res)) %>%
            dplyr::select(.data$R2)
    }
    dplyr::as_tibble(R2_out)
}


#' Calculate part R2
#'
#' @param mod merMod object
#' @param expct Expectation
#' @param overdisp_name Name of overdispersion term
#' @param R2_type "marginal" or "conditional"
#' @param all_comb variable combinations to remove to calculate R2
#' @param partition check if R2 partitioning is needed
#' @param data_mod data.frame
#' @param allow_neg_r2 TRUE or FALSE
#'
#' @keywords internal
#' @return R2, atm data.frame with one element
#'
# partition R2
part_R2s <- function(mod, expct, overdisp_name, R2_type, all_comb, partition,
                     data_mod, allow_neg_r2) {
    # calculate full model R2
    R2_full <- R2_pe(mod, expct, overdisp_name, R2_type)
    if (!partition) return( tidyr::tibble(term = "Full", estimate = R2_full$R2))

    # calculate fix effects variances of reduced models
    var_fix_red <- purrr::map_df(all_comb, fixvar_of_red_mod, mod = mod,
                                  dat = data_mod, expct = expct,
                                  overdisp_name = overdisp_name, R2_type = R2_type)

    # get variance components of full model
    var_comps_full <- get_var_comps(mod, expct, overdisp_name)

    # combine full model variance components with reduced models
    # fixed effect variances
    var_comps_all <- data.frame(var_comps_full, var_fix_red)

    if (R2_type == "marginal") {
        R2_out <- var_comps_all %>%
            dplyr::transmute(R2 =  (.data$var_fix - .data$var_fix_red) /
                                 (.data$var_fix + .data$var_ran + .data$var_res))
    } else if (R2_type == "conditional") {
        R2_out <- var_comps_all %>%
            dplyr::transmute(R2 = ((.data$var_fix - .data$var_fix_red) + .data$var_ran) /
                                 (.data$var_fix + .data$var_ran + .data$var_res))
    }

    if(!allow_neg_r2) {
        R2s_red <-  R2_out %>%
            # if by chance part R2 drops below 0, set to 0
            dplyr::mutate(R2 = ifelse(.data$R2 < 0, 0, .data$R2)) %>%
            dplyr::bind_rows(R2_full, .)
    } else if (allow_neg_r2) {
        R2s_red <-  R2_out %>%  dplyr::bind_rows(R2_full, .)
    }

    # names for partitions
    if (partition) {
        part_terms <- c("Full", unlist(lapply(all_comb, paste, collapse = "+")))
    } else if (!partition) {
        part_terms <- "Full"
    }

    R2s_red <- tibble::tibble(term = part_terms, estimate = R2s_red$R2)
}

# reduced model R2 (mod without partvar)

#' Calculate fixed effect variance from a reduced model
#'
#' @param partvar One or more fixed effect variables which are taken out
#' of the model.
#' @param mod merMod object.
#' @param dat Data.frame to fit the model
#' @param expct Expectation
#' @param overdisp_name Name of overdispersion term
#' @param R2_type "marginal" or "conditional"
#' @keywords internal
#' @return R2 of reduced model.
#'
fixvar_of_red_mod <- function(partvar, mod, dat, expct, overdisp_name, R2_type) {

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
    # check that random slope models don't reduce fixed effects which
    # are part of the random slope
    get_ran_var(mod_red, overdisp_name) # throws error if action not allowed
    # reduced model fixef variance
    var_fix <- tibble::tibble(var_fix_red = stats::var(stats::predict(mod_red, re.form=NA)))
    #R2_red <- R2_pe(mod_red, expct, overdisp_name, R2_type)
}


#' Extracts random effect variances
#'
#' This function computes the sum of random effect variances where one
#' or more of the random effects are random slopes. It uses Paul Johnson' method
#' to compute the average group variance across the levels of a covariate.
#' This function extracts only grouping factors, no residual or overdispersion.
#'
#'
#' @param mod An lme4 model object.
#' @param overdisp_name name of overdispersion term
#' @keywords internal
#'
get_ran_var <- function(mod, overdisp_name = NULL){

    var_comps <- lme4::VarCorr(mod)

    # gives only grouping factors, no Residual, no overdisp
    if (!is.null(overdisp_name)) {
        grnames <- names(var_comps)[!(names(var_comps) %in% overdisp_name)]
    } else {
        grnames <- names(var_comps)
    }


    var_raneff <- function(grname, var_comps) {
        # check whether component is a matrix (--> random slopes)
        if (sum(dim(var_comps[[grname]])) > 2){
            sigma <- var_comps[[grname]]
            # a problem arises here for random slope models, when in the reduced
            # model the fixed effect isn't there anymore
            if (!all(colnames(sigma) %in% colnames(stats::model.matrix(mod)))){
                stop("partR2 can't calculate part R2
                     for fixed effects involved in random slopes")
            }
            # design matrix subsetted for the elements of sigma
            Z <- stats::model.matrix(mod)[, colnames(sigma)]
            # average variance across covariate
            var_grname <- sum(rowSums((Z %*% sigma) * Z))/stats::nobs(mod)
        } else {
            var_grname <- as.numeric(var_comps[[grname]])
        }
        var_grname
    }

    # random effect variances
    var_raneffs <- data.frame(group = grnames,
                              estimate = purrr::map_dbl(grnames, var_raneff, var_comps))

    var_raneffs

}

#' Extract variance components from merMod.
#'
#' @param mod A merMod object.
#' @param expct expectation.
#' @param overdisp_name name of overdispersion term
#'
#' @keywords internal
#' @return Fixed, random and residual variance
#'
get_var_comps <- function(mod, expct, overdisp_name) {

    fam <- stats::family(mod)[["family"]]
    #if (is.null(expct)) expct <- "meanobs"

    if (!(fam %in% c("gaussian", "poisson", "binomial"))) {
        stop("Currently, only gaussian, poisson and binomial models are supported")
    }

    if (fam == "gaussian") {
        out <- var_comps_gaussian(mod)
    }
    if (fam == "poisson"){
        out <- var_comps_poisson(mod, expct, overdisp_name)
    }
    if (fam == "binomial"){
        # check if binary
        if (length(table(lme4::getME(mod, "y"))) < 3) {
            out <- var_comps_binary(mod, expct)
        }
        # if not binary
        if (length(table(lme4::getME(mod, "y"))) >= 3) {
            out <- var_comps_proportion(mod, expct, overdisp_name)
        }
    }

    out

}


#' Get variance components for gaussian model.
#'
#' @param mod merMod object with gaussian family.
#' @keywords internal
#' @return Fixed, random and residual variance
#'
var_comps_gaussian <- function(mod, ...) {

    # random effect variance
    var_ran <- sum(get_ran_var(mod)$estimate)

    # residual variance
    var_res <- attr(lme4::VarCorr(mod), "sc")^2

    # fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran,
                      var_res = var_res)
}

#' Get variance components for merMod with poisson response.
#'
#' @param mod merMod object with poisson family.
#' @param expct "meanobs" or "latent". "latent" recommended.
#' @param overdisp_name name of overdispersion term
#'
#' @keywords internal
#' @return Fixed, random and residual variance
#'
var_comps_poisson <- function(mod, expct, overdisp_name) {

    # intercept on link scale
    beta0 <- unname(lme4::fixef(mod)[1])

    # random effect variances
    var_ran <- get_ran_var(mod, overdisp_name)

    # fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    # family and link
    mod_fam <- stats::family(mod)

    # overdispersion estimate, if there, else 0
    ran_comps <- lme4::VarCorr(mod)
    var_overdisp <- ifelse(overdisp_name %in% names(ran_comps),
                           as.numeric(ran_comps[[overdisp_name]]), 0)

    # sum up random effects
    # var_ran <- sum(var_ran$estimate)

    # remove overdisp from var_ran and sum up
    # var_ran <- var_ran[!(var_ran$group == "overdisp"), ]
    # var_ran <- sum(var_ran$estimate)

    if (expct == "none") {
        # if none, the distribution specific variance is 0
        var_res <- var_overdisp
    } else if (mod_fam[["link"]] == "sqrt") {
        var_res <- var_overdisp + 0.25
    } else if (mod_fam[["link"]] == "log") {
        if(expct=="meanobs") EY <- mean(mod@resp$y, na.rm=TRUE)
        # no overdisp in var_ran
        if(expct=="latent") EY <- exp(beta0 + (sum(var_ran$estimate) + var_overdisp + var_fix)/2)
        # residual variance
        var_res <- var_overdisp + log(1/EY+1)
    }

    # remove overdisp from var_ran and sum up as overdisp is now part of var_res
    # var_ran <- var_ran[!(var_ran$group == overdisp_name), ]

    # var_ran does not include overdispersion
    var_ran <- sum(var_ran$estimate)

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran,
                      var_res = var_res)
}

#' Get variance components for binomial model with proportion response.
#'
#' @param mod merMod object with binomial family and binary response.
#' @param expct "latent", "meanobs" of "liability"
#' @keywords internal
#' @return Fixed, random and residual variance
#'
var_comps_proportion <- function(mod, expct, overdisp_name) {

    # random effects
    var_ran <- get_ran_var(mod, overdisp_name)

    # overdispersion estimate, if there, else 0
    ran_comps <- lme4::VarCorr(mod)
    var_overdisp <- ifelse(overdisp_name %in% names(ran_comps),
                           as.numeric(ran_comps[[overdisp_name]]), 0)

    # intercept on link scale
    beta0 <- unname(lme4::fixef(mod)[1])

    # Fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    # family and link
    mod_fam <- stats::family(mod)

    if (mod_fam[["link"]] == "logit") {
        # if(expct=="latent") Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(sum(VarComps[,"vcov"])+var_f))^-1)
        if (expct=="latent") {
            # should overdisp be included here? probably yes #### check
            Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(sum(var_ran$estimate) + var_overdisp + var_fix))^-1)
            estdv_link <- 1 / (Ep*(1-Ep))
        }
        if (expct=="meanobs") {
            Ep <- mean(lme4::getME(mod, "y"), na.rm=TRUE)
            estdv_link <- 1 / (Ep*(1-Ep))
        }
        if (expct=="liability") {
            Ep <- exp(beta0) / (1 + exp(beta0))
            estdv_link <- pi^2/3
        }
        if (expct=="none") {
            estdv_link <- 0
        }
        var_res <- var_overdisp + estdv_link
    }

    # Helper function
    # inverf based on posting by sundar on R-help
    # https://stat.ethz.ch/pipermail/r-help/2006-June/108153.html
    inverf <- function(x) stats::qnorm((x + 1)/2)/sqrt(2)

    if (mod_fam[["link"]] == "probit"){
        if (expct == "latent") {
            Ep <- stats::pnorm(beta0*sqrt(1+sum(var_ran$estimate) + var_overdisp +var_fix)^-1)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        }
        if (expct=="meanobs"){
            Ep <- mean(lme4::getME(mod, "y"), na.rm=TRUE)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        }
        if (expct=="liability"){
            estdv_link <- 1
        }
        if (expct=="none") {
            estdv_link <- 0
        }
        var_res <- var_overdisp + estdv_link
    }

    # remove overdisp from var_ran and sum up as overdisp is now part of var_res
    # var_ran <- var_ran[!(var_ran$group == overdisp_name), ]

    # var_ran does not include overdispersion
    var_ran <- sum(var_ran$estimate)

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran,
                      var_res = var_res)

}


#' Get variance components for binomial model with binary response.
#'
#' @param mod merMod object with binomial family and binary response.
#' @param expct "latent", "meanobs" of "liability"
#' @keywords internal
#' @return Fixed, random and residual variance
#'
var_comps_binary <- function(mod, expct) {

    # random effect variance
    var_ran <- sum(get_ran_var(mod)$estimate)

    # intercept on link scale
    beta0 <- unname(lme4::fixef(mod)[1])

    # Fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    # family and link
    mod_fam <- stats::family(mod)

    if (mod_fam[["link"]] == "logit") {
        if (expct=="meanobs") {
            Ep <- mean(lme4::getME(mod, "y"), na.rm=TRUE)
            estdv_link <- 1 / (Ep*(1-Ep))
        } else if (expct=="latent") {
            Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(var_ran + var_fix))^-1)
            estdv_link <- 1 / (Ep*(1-Ep))
        } else if (expct=="liability") {
            Ep <- exp(beta0) / (1 + exp(beta0))
            estdv_link <- pi^2/3
        } else if (expct=="none") {
            estdv_link <- 0
        }
        var_res <-  estdv_link
    }

    # Helper function
    # inverf based on posting by sundar on R-help
    # https://stat.ethz.ch/pipermail/r-help/2006-June/108153.html
    inverf <- function(x) stats::qnorm((x + 1)/2)/sqrt(2)

    if (mod_fam[["link"]] == "probit"){
        if (expct=="meanobs"){
            Ep <- mean(lme4::getME(mod, "y"), na.rm=TRUE)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        } else if (expct == "latent") {
            Ep <- stats::pnorm(beta0*sqrt(1+var_ran+var_fix)^-1)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        } else if (expct=="liability"){
            estdv_link <- 1
        } else if (expct=="none"){
            estdv_link <- 0
        }
        var_res <- estdv_link
    }

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran,
                      var_res = var_res)


}




