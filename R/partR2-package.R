#' partR2: Partitioning R2 in generalized linear mixed models
#'
#' The partR2 package provides a simple way to estimate R2 in mixed models fitted with lme4
#' as well as part (semi-partial) R2 for specific predictors and combinations of predictors,
#' among other several other statistics. Here is an overview:
#'
#' \itemize{
#' \item Marginal and conditional R2 for LMMs and GLMMs.
#' \item Part (semi-partial) R2 which estimate the explained variance for
#' specific predictors and combinations of predictors.
#' \item Structure coefficients (SC). SC are the correlation between a
#' predictor and the predicted response (called the linear predictor), independent of the other predictors.
#' \item Inclusive R2 (IR2), which estimate the the total variance explained by a predictor independent of
#' other predictors. IR2 is estimated with SC^2 * R2_full_model.
#' \item Beta weights, which are standardised regression coefficients.
#' If beta is a model estimate for variable x, and y is the response,then the
#' beta weight is beta * (sd(x)/sd(y).
#' \item Confidence intervals for all estimates using parametric bootstrapping.
#' }
#'
#' The package has one main function \code{\link{partR2}} which takes a fitted model
#' from lme4. At the moment, Gaussian, Poisson and binomial models are supported.
#' For Poisson and non-binary binomial models, \code{partR2} adds an
#' observational level random effect to model additive overdispersion (if
#' an olre is not fitted already).
#'
#' The \code{\link{summary.partR2}} function provides an extended summary with R2s, semi-partial
#' R2s, model estimates and structure coefficients. The \code{\link{forestplot}}
#' function provides a means of plotting the results.
#'
#' @references
#'
#' Nakagawa, S., & Schielzeth, H. (2013). \emph{A general and simple method for obtaining R2 from
#' generalized linear mixed-effects models}. Methods in Ecology and Evolution, 4(2), 133-142.
#'
#' Nakagawa, S., Johnson, P. C., & Schielzeth, H. (2017). \emph{The coefficient of
#' determination R2 and intra-class correlation coefficient from generalized
#' linear mixed-effects models revisited and expanded}. Journal of the Royal Society Interface, 14(134), 20170213.
#'
#' @importFrom rlang .data
#'
#' @docType package
#' @name partR2-package
#'
#'
NULL
#> NULL
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
