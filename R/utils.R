#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Get variance components
#'
#' @param mod
#' @param expect "meanobs" or "latent". "latent" recommended.
#'
#' @return Fixed, random and residual variance
#' @export
#'
#' @examples
get_var_comps <- function(mod, expect) {

    if (mod_fam[["family"]] == "poisson"){

        # intercept on link scale
        beta0 <- unname(lme4::fixef(mod)[1])

        # random effects
        var_ran <- broom.mixed::tidy(mod, scales = "vcov", effects = "ran_pars")

        # fixed effect variance
        var_fix <- stats::var(stats::predict(mod, re.form=NA))

        # Distribution-specific and residual variance
        mod_fam <- stats::family(mod)

        # overdispersion estimate
        overdisp_est <- var_ran[var_ran$group == "overdisp", ][["estimate"]]

        if (mod_fam[["link"]] == "sqrt") {
            var_res <- overdisp_est + 0.25
        }
        if (mod_fam[["link"]] == "log") {
            if(expect=="meanobs") EY <- mean(mod@resp$y, na.rm=TRUE)
            # should overdisp be included here? probably yes
            if(expect=="latent") EY <- exp(beta0 + (sum(var_ran$estimate) + var_f)/2)
            # residual variance
            var_res <- overdisp_est + log(1/EY+1)
        }

        # random effect variance without overdispersion
        var_ran_wo_overdisp <- var_ran %>%
                                filter(group != "overdisp") %>%
                                summarise(sum(estimate)) %>%
                                unname() %>%
                                unlist()
    }

    out <- data.frame(var_fix = var_fix,
                      var_ran = var_ran_wo_overdisp,
                      var_res = var_res)
}


# reduced model R2 (mod without partvar)

#' Calculate R2 from a reduced model
#'
#' @param partvar One or more fixed effect variables which are taken out
#' of the model.
#' @param mod An lme4 model.
#'
#' @return R2 of reduced model.
#'
#' @examples
R2_of_red_mod <- function(partvar, mod) {

    # which variables to reduce?
    to_del <- paste(paste("-", partvar, sep= ""), collapse = " ")
    # reduced formula
    formula_red <- stats::update(stats::formula(mod), paste(". ~ . ", to_del, sep=""))
    # fit reduced model
    mod_red <-  stats::update(mod, formula. = formula_red)
    # reduced model R2
    R2_red <- R2_pe(mod_red)

}
