#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Captures and suppresses (still to find out why) warnings of an expression
#'
#' This function is used within partR2 to capture lme4 model fitting warnings in the
#' bootstrap and permutation procedures.
#'
#' @param expr An expression, such as the sequence of code used by rptR to calculate
#' bootstrap or permutation estimates
#' @keywords internal

with_warnings <- function(expr) {
    myWarnings <- NULL
    myMessages <- NULL
    wHandler <- function(w) {
        myWarnings <<- c(myWarnings, list(w))
        invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(expr, warning = wHandler)
    list(warnings = myWarnings)
}

#' Create list of combination of variables.
#'
#' @inheritParams partR2
#'
#' @return list with all combinations of predictors specified in partvars/partbatch
#'
make_combs <- function(partvars, partbatch, max_level) {
    # create list of all unique combinations except for the full model
    if (!is.null(partvars)) {
        if (length(partvars) > 1){
            all_comb <- unlist(lapply(1:(length(partvars)),
                                      function(x) utils::combn(partvars, x, simplify = FALSE)),
                               recursive = FALSE)
        } else if (length(partvars) == 1) {
            all_comb <- as.list(partvars)
        }
    }  else {
        all_comb <- NA
    }

    # check batches
    if (!is.null(partbatch)) {
        if(!is.list(partbatch)) stop("partbatch must be a list")
        if (is.null(all_comb)) all_comb <-NA
        # first, make combinations of partbatches
        combs_num <- purrr::map(1:length(partbatch),
                                function(m) utils::combn(length(partbatch), m,
                                                         simplify = FALSE)) %>%
            unlist(recursive = FALSE)
        comb_batches <- purrr::map(combs_num, function(x) unlist(partbatch[x]))
        # now add those to partvar combs
        comb_batches2 <- purrr::map(comb_batches, function(x)
            purrr::map(all_comb, function(z) c(z, x))) %>%
            unlist(recursive = FALSE)
        # now add those to all_combs
        all_comb <- c(partbatch, all_comb, comb_batches2)
        # check for duplicates or NA and remove in case
        all_comb <- purrr::map(all_comb, function(x) x[!(duplicated(x) | is.na(x))])
        # last step remove any empty list elements
        all_comb[purrr::map(all_comb, length) == 0] <- NULL
        # remove potential duplicates
        all_comb <- all_comb[!(duplicated(purrr::map(all_comb, function(x) as.character(sort(x)))))]
        # change all formats to unnamed character vecotr
        all_comb <- purrr::map(all_comb, function(x) as.character(unname(x)))
    }

    # commonality coefficients up to max_level (e.g. 3 for
    # the cc of 3 predictors)
    if (!is.null(partbatch) & !is.null(max_level)) {
        stop("Argument max_level does currently not work in combination with argument partbatch,
                please use partvars or leave max_level at NULL")
    }
    if (!is.null(max_level)) {
        remove_combs <- purrr::map_lgl(all_comb, function(x) length(x) > max_level)
        all_comb[remove_combs] <- NULL
    }

    all_comb


}

