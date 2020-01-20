#' partR2: partitioning R2 in mixed models
#'
#' partR2 provides R2, partitioned R2s, structure coefficients and
#' estimates for for mixed models.
#'
#' It's main goals are:
#'
#' \itemize{
#' \item Estimate marginal and conditional R2 for LMMs and GLMMs.
#' \item Partition the R2 into variance explained uniquely by each predictor
#' and variance explained by a combination of predictors.
#' \item Provide structure coefficients (the correlation between each
#' predictor and the predicted response, independent of the other predictors)
#' \item Report model estimates (based on the broom.mixed package)
#' \item Use parametric bootstrapping to get confidence intervals for
#' all estimates.
#' }
#'
#'
#' @references
#'
#' Nakagawa, S., & Schielzeth, H. (2013). \emph{A general and simple method for obtaining R2 from
#' generalized linear mixed‐effects models}. Methods in Ecology and Evolution, 4(2), 133-142.
#'
#' Newton, R. G., & Spurrell, D. J. (1967).  \emph{A development of multiple regression for the
#' analysis of routine data. Applied Statistics}. 51-64.
#'
#' @importFrom rlang .data
#'
#' @docType package
#'
#' @aliases partR2-package
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
