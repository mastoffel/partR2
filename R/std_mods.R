#' Standardise variables
#'
#'
#' @param mod merMod object.
#' @keywords internal
#' @return list with scaling factors to multiply estimates with to get BW
#' @export
#'
#'

get_bw_scaling <- function (mod) {

    mod_mat <- stats::model.matrix(mod)
    #mod_mat <- mod_mat[, !(colnames(mod_mat) == "(Intercept)"), drop = FALSE]
    preds <- strsplit(colnames(mod_mat), split = ":")
    pred_int <- purrr::map_int(preds, length) > 1

    # new mod_mat for scaled interaction
    mod_mat2 <- mod_mat

    for (i in 1:length(pred_int)) {
        if (pred_int[i]) {
            mod_mat2[, i] <-
                purrr::map(preds[[i]], function(pred) scale(mod_mat[, pred], center = FALSE)) %>%
                purrr::reduce(`*`)
        }
    }

    sds <- purrr::map_dbl(as.data.frame(mod_mat), stats::sd, na.rm = TRUE)
    sds2 <- purrr::map_dbl(as.data.frame(mod_mat2), stats::sd, na.rm = TRUE)

    sds_out <- sds
    sds_out[pred_int] <- sds2[pred_int] / sds[pred_int]

    sds_out
}

#' Get beta weights.
#'
#' For Gaussian models, the predictors and the response are standardised.
#' For GLMMs, only predictors are standardised.
#'
#' @param mod merMod object.
#' @param ests tidy merMod output form broom.mixed
#' @keywords internal
#' @return tidy output with bw instead of raw estimates
#' @export
#'
#'


get_bw <- function(mod){

    sds <- get_bw_scaling(mod)
    resp_sd <- stats::sd(lme4::getME(mod, "y"), na.rm = TRUE)

    if (stats::family(mod)$family == "gaussian") {
        sd_scale <- sds / resp_sd
    } else {
        sd_scale <- sds
    }

    ests <- broom.mixed::tidy(mod, effects = "fixed")
    ests_scld <- ests
    ests_scld$estimate <- ests_scld$estimate * sd_scale
    #ests_scld[ests_scld$term != "(Intercept)", "estimate"] <-
    #    ests_scld[ests_scld$term != "(Intercept)", "estimate"] * sd_scale
    ests_scld[ests_scld$term == "(Intercept)", "estimate"] <-  ests[ests$term == "(Intercept)", "estimate"]
    ests_scld
}

