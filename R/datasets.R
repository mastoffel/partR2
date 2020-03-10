#' biomass dataset
#'
#' @details This is an simulated dataset about grassland biodiversity and biomass.
#'
#' In brief the imaginary sampling design of the simulated dataset is as follows.
#' Invertebrates were sampled once every year over 10 successive years (`Year`) from 20
#' different populations (`Population`). For each sample, the temperature (`Temperature`)
#' and (`Precipitation`) were measured and overall species diversity (`SpeciesDiversity`)
#' and biomass were recorded (`Biomass`).
#'
#'
#' @keywords datasets
#' @name biomass
NULL

#' Grasshoppers dataset
#'
#' @details This is a real dataset from grasshoppers.
#'
#' This dataset contains data on spatial variation in color morph ratios in a
#' color-polymorphic species of grasshopper (Dieker et al 2018). Individuals of this species occur
#' either in green or a brown color variant and the dataset contains counts of
#' brown and green individuals (seprarated for females and males) from 42 sites
#' sampled in the field. All 'Bio' variables describe various aspects of ecologically
#' relevant climatic conditions (see Karger et al. 2017).
#'
#' @references
#' Dieker, P., L. Beckmann, J. Teckentrup, and H. Schielzeth (2018)
#' \emph{Spatial analyses of two colour polymorphisms in an alpine grasshopper reveal
#' a role of small-scale heterogeneity}. Ecology and Evolution, 8, 7273-7284.
#'
#' Karger, D. N., O. Conrad, J. Bohner, T. Kawohl, H. Kreft, R. W. Soria-Auza,
#' N. E. Zimmermann, H. P. Linder, and M. Kessler (2017) \emph{Data descriptor: Climatologies
#' at high resolution for the earth's land surface areas}. Scientific Data, 4, 170122.
#'
#' @keywords datasets
#' @name Grasshoppers
NULL

#' GuineaPigs dataset
#'
#' @details This is a real dataset from guinea pigs.
#'
#' The dataset contains testosterone measurements (`Testo`) of 31 male guinea pigs, each measured at 5 time points.
#' (age between 120 and 240 days at 30-day intervals). As covariates the dataset
#' contains the time point of measurement (`Time`) and a rank index derived from behavioral
#' observations (`Rank`) around the time of measurement (see Mutwill et al. in prep. for details).
#'
#'
#' @keywords datasets
#' @name GuineaPigs
NULL

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
#' @keywords internal
#' @name BeetlesMale
NULL


#' BeetlesFemale dataset
#'
#' @details This is an simulated dataset which was used as a toy example for a different purpose
#' (Nakagawa & Schielzeth 2013).
#' It offers a balanced dataset with rather simple structure, sizable effects and decent sample size,
#' just right for demonstrating some features of \code{partR2}.
#' Sufficient sample size is required in particular for the non-Gaussian traits,
#' because those tend to be more computationally demanding and less rich in information per data
#' point than simple Gaussian traits.
#'
#' In brief the imaginary sampling design of the simulated dataset is as follows.
#' Beetle larvae were sampled from 12 populations (`Population`) with samples taken from two
#' discrete microhabitats at each location (`Habitat`). Samples were split in equal proportion
#' and raised in two dietary treatments (`Treatment`). Beetles were sexed at the pupal stage (`Sex`)
#' and pupae were kept in sex-homogeneous containers (`Container`).
#' The phenotype in this dataset is the number of eggs laid by female beetles (`Egg`).
#'
#' @references
#' Nakagawa, S. & Schielzeth, H. (2013) \emph{A general and simple method for obtaining R2
#' from generalized linear mixed-effects models}. Methods in Ecology and Evolution 4: 133-142.
#'
#' @keywords internal
#' @name BeetlesFemale
NULL


#' BeetlesBody dataset
#'
#' @details This is an simulated dataset which was used as a toy example for a different purpose
#' (Nakagawa & Schielzeth 2013).
#' It offers a balanced dataset with rather simple structure, sizable effects and decent sample size,
#' just right for demonstrating some features of \code{partR2}.
#' Sufficient sample size is required in particular for the non-Gaussian traits,
#' because those tend to be more computationally demanding and less rich in information per data
#' point than simple Gaussian traits.
#'
#' In brief the imaginary sampling design of the simulated dataset is as follows.
#' Beetle larvae were sampled from 12 populations (`Population`) with samples taken from two
#' discrete microhabitats at each location (`Habitat`). Samples were split in equal proportion
#' and raised in two dietary treatments (`Treatment`). Beetles were sexed at the pupal stage (`Sex`)
#' and pupae were kept in sex-homogeneous containers (`Container`). The phenotype
#' in this dataset is body length (`BodyL`).
#'
#' @references
#' Nakagawa, S. & Schielzeth, H. (2013) \emph{A general and simple method for obtaining R2
#' from generalized linear mixed-effects models}. Methods in Ecology and Evolution 4: 133-142.
#'
#'
#' @keywords internal
#' @name BeetlesBody
NULL
