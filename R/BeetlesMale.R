#' BeetlesMale dataset
#'
#' @details This is an simulated dataset which was used as a toy example for a different purpose
#' (Nakagawa & Schielzeth 2013).
#' It offers a balanced dataset with rather simple structure, sizable effects and decent sample size,
#' just right for demonstrating some features of \code{rptR}.
#' Sufficient sample size is required in particular for the non-Gaussian traits,
#' because those tend to be more computationally demanding and less rich in information per data
#' point than simple Gaussian traits.
#'
#' In brief the imaginary sampling design of the simulated dataset is as follows.
#' Beetle larvae were sampled from 12 populations (`Population`) with samples taken from two
#' discrete microhabitats at each location (`Habitat`). Samples were split in equal proportion
#' and raised in two dietary treatments (`Treatment`). Beetles were sexed at the pupal stage (`Sex`)
#' and pupae were kept in sex-homogeneous containers (`Container`).
#' The phenotype in this dataset is a binary variable containing the two distinct color
#' morphs of males: dark and reddish-brown (`Colour`).
#'
#'
#' @references
#' Nakagawa, S. & Schielzeth, H. (2013) \emph{A general and simple method for obtaining R2
#' from generalized linear mixed-effects models}. Methods in Ecology and Evolution 4: 133-142.
#'
#'
#' @keywords datasets
#' @name BeetlesMale
NULL
