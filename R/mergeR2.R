#' Merge partR2 objects to combine R2s for main effects and interactions
#'
#' The function merges partR2 object based on a full model with interactions
#' with a partR2 object based on a reduced model without interaction. The
#' reduced model is used to infer main effect semi-partial R2s. This function
#' essentially takes over the complete partR2 object for the full model and
#' adds semi-partial R2s which have been calculated based on the reduced
#' model and are not already present in the full model partR2 object (which
#' can be main effects).
#'
#' This function is a bit experimental and should be used with caution.
#' See vignette or paper on how to use it to obtain semi-partial R2s
#' for main effects which are also involved in interactions.
#'
#' @param R2_full partR2 object for the full model, with the interaction
#' (but not the main effects) in partvars.
#' @param ... other partR2 objects, which do not contain the interaction
#' so that the semi-partial R2s for the main effects could be calculated.
#'
#' @return
#' Returns an object of class \code{partR2}, which takes most components from the
#' full model except for semi-partial R2s.
#'
#'
#'
#' @examples
#'
#' data(biomass)
#' library(lme4)
#'
#' # scale data
#' biomass[] <- lapply(biomass, function(x) if (is.double(x)) scale(x) else x)
#'
#' # Full model
#' mod_full <- lmer(Biomass ~  Year + Temperature * Precipitation + SpeciesDiversity + (1|Population),
#'             data = biomass)
#' # Semi-partial R2 for interaction and all other predictors of interest
#' (R2_full <- partR2(mod_full, partvars = c("Temperature:Precipitation", "SpeciesDiversity", "Year"),
#' data = biomass))
#'
#' # model without interaction to get main effect semi-partial R2s
#' mod_noIA <- lmer(Biomass ~  Year + Temperature + Precipitation + SpeciesDiversity + (1|Population),
#'             data = biomass)
#' (R2_noIA <- partR2(mod_noIA, partvars = c("Temperature", "Precipitation"), data = biomass))
#'
#' # combine both
#' (R2_comb <- mergeR2(R2_full, R2_noIA))
#'
#' @export
#'

mergeR2 <- function(R2_full, ...) {
    redmods <- list(...)
    allmods <- c(list(R2_full), redmods)
    R2_full$R2 <- purrr::reduce(purrr::map(allmods, "R2"), rbind) %>%
                  dplyr::filter(!duplicated(.data$parts))
    R2_full
}
