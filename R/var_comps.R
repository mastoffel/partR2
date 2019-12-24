#' Extract variance components from merMod.
#'
#' @param mod A merMod object.
#' @param expct expectation.
#'
#' @keywords internal
#' @return Fixed, random and residual variance
#' @export
#'
get_var_comps <- function(mod, expct) {

    fam <- stats::family(mod)[["family"]]
    #if (is.null(expct)) expct <- "meanobs"

    if (fam == "gaussian") {
        out <- var_comps_gaussian(mod)
    }
    if (fam == "poisson"){
        out <- var_comps_poisson(mod, expct)
    }
    if (fam == "binomial"){
        if (length(table(lme4::getME(mod, "y")) < 3)) {
            out <- var_comps_binary(mod, expct)
        }
        if (length(table(lme4::getME(mod, "y")) > 2)) {
            out <- var_comps_proportion(mod, expct)
        }
    }

    out

}

#' Get variance components for binomial model with proportion response.
#'
#' @param mod merMod object with binomial family and binary response.
#' @param expct "latent", "meanobs" of "liability"
#' @keywords internal
#' @return Fixed, random and residual variance
#' @export
#'
var_comps_proportion <- function(mod, expct) {

    # random effect variance
    var_ran <- broom.mixed::tidy(mod, scales = "vcov",  effects = "ran_pars") %>%
        # unclear whether overdisp is in or excluded here!
        dplyr::filter(group != "overdisp") %>%
        dplyr::summarise(sum(estimate)) %>%
        unname() %>%
        unlist()

    var_overdisp <- broom.mixed::tidy(mod, scales = "vcov",  effects = "ran_pars") %>%
        dplyr::filter(group == "overdisp") %>%
        .$estimate

    # intercept on link scale
    beta0 <- unname(lme4::fixef(mod)[1])

    # Fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    # family and link
    mod_fam <- stats::family(mod)

    if (mod_fam[["link"]] == "logit") {
        # if(expct=="latent") Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(sum(VarComps[,"vcov"])+var_f))^-1)
        if (expct=="latent") {
            Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(var_ran + var_fix))^-1)
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
        var_res <- var_overdisp + estdv_link
    }

    if (mod_fam[["link"]] == "probit"){
        if (expct == "latent") {
            Ep <- stats::pnorm(beta0*sqrt(1+var_ran+var_fix)^-1)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        }
        if (expct=="meanobs"){
            Ep <- mean(lme4::getME(mod, "y"), na.rm=TRUE)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        }
        if (expct=="liability"){
            estdv_link <- 1
        }
        var_res <- var_overdisp + estdv_link
    }

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
#' @export
#'
var_comps_binary <- function(mod, expct) {

    # random effect variance
    var_ran <- broom.mixed::tidy(mod, scales = "vcov",  effects = "ran_pars") %>%
        dplyr::summarise(sum(estimate)) %>%
        unname() %>%
        unlist()

    # intercept on link scale
    beta0 <- unname(lme4::fixef(mod)[1])

    # Fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    # family and link
    mod_fam <- stats::family(mod)

    if (mod_fam[["link"]] == "logit") {
        # if(expct=="latent") Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(sum(VarComps[,"vcov"])+var_f))^-1)
        if (expct=="latent") {
            Ep <- stats::plogis(beta0*sqrt(1+((16*sqrt(3))/(15*pi))^2*(var_ran + var_fix))^-1)
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
        var_res <-  estdv_link
    }

    if (mod_fam[["link"]] == "probit"){
        if (expct == "latent") {
            Ep <- stats::pnorm(beta0*sqrt(1+var_ran+var_fix)^-1)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        }
        if (expct=="meanobs"){
            Ep <- mean(lme4::getME(mod, "y"), na.rm=TRUE)
            estdv_link <- 2*pi*Ep*(1-Ep) * (exp(inverf(2*Ep-1)^2))^2
        }
        if (expct=="liability"){
            estdv_link <- 1
        }
        var_res <- estdv_link
    }

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran,
                      var_res = var_res)


}



#' Get variance components for gaussian model.
#'
#' @param mod merMod object with gaussian family.
#' @keywords internal
#' @return Fixed, random and residual variance
#' @export
#'
var_comps_gaussian <- function(mod, ...) {

    # random effect variance
    var_ran <- broom.mixed::tidy(mod, scales = "vcov",  effects = "ran_pars") %>%
        dplyr::filter(group != "Residual") %>%
        dplyr::summarise(sum(estimate)) %>%
        unname() %>%
        unlist()

    # residual variance
    var_res <- broom.mixed::tidy(mod, scales = "vcov",  effects = "ran_pars") %>%
        dplyr::filter(group == "Residual") %>%
        .[["estimate"]]

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
#' @keywords internal
#' @return Fixed, random and residual variance
#' @export
#'
var_comps_poisson <- function(mod, expct) {

    # intercept on link scale
    beta0 <- unname(lme4::fixef(mod)[1])

    # random effects
    var_ran <- broom.mixed::tidy(mod, scales = "vcov", effects = "ran_pars")

    # fixed effect variance
    var_fix <- stats::var(stats::predict(mod, re.form=NA))

    # family and link
    mod_fam <- stats::family(mod)

    # overdispersion estimate
    overdisp_est <- var_ran[var_ran$group == "overdisp", ][["estimate"]]

    if (mod_fam[["link"]] == "sqrt") {
        var_res <- overdisp_est + 0.25
    }
    if (mod_fam[["link"]] == "log") {
        if(expct=="meanobs") EY <- mean(mod@resp$y, na.rm=TRUE)
        # should overdisp be included here? probably yes
        if(expct=="latent") EY <- exp(beta0 + (sum(var_ran$estimate) + var_f)/2)
        # residual variance
        var_res <- overdisp_est + log(1/EY+1)
    }

    # random effect variance without overdispersion
    var_ran_wo_overdisp <- var_ran %>%
        dplyr::filter(group != "overdisp") %>%
        dplyr::summarise(sum(estimate)) %>%
        unname() %>%
        unlist()

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran_wo_overdisp,
                      var_res = var_res)
}
