% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{make_combs}
\alias{make_combs}
\title{Create list of combination of variables.}
\usage{
make_combs(partvars, partbatch, max_level)
}
\arguments{
\item{partvars}{Character vector specifying the predictors (fixed effects) for which to partition the R2.
Can be main effects like c("Var1", "Var2") and interactions ("Var1:Var2"). Predictors
specified in partvars have to be named precisely like the terms in the formula to
fit the model.}

\item{partbatch}{List of character vectors with predictors that should be fitted and
removed together. For example, partbatch = list(batch1 = c("V1", "V2", "V3"),
batch2 = c("V4", "V5", "V6")) would calculate part R2 only for combinations of
predictors which contain the variables V1, V2, V3 together or/and V4,V5,V6 together.
This is useful when the number of potential subsets gets too large to
be computationally practical, for example when dummy coding is used.
See our vignette for details. This feature is still experimental and
should be used with caution.}

\item{max_level}{Level up to which shared semi-partial R2s are calculated.
The number of sets for which to calculate R2 increases exponentially,
i.e. for 10 variables 2^10 - 1 R2s  can be calculated. If you are
only interested in the unique but not the shared effects, use max_level = 1.
If interested in unique effects and combinations of two terms,
use max_level = 2 etc.}
}
\value{
list with all combinations of predictors specified in partvars/partbatch
}
\description{
Create list of combination of variables.
}
\keyword{internal}
