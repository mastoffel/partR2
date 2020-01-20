#' Complete summary of a partR2 object
#'
#' Displays the complete results a partR2object (i.e. the result of a partR2 function call)
#' which includes R2, partial R2, model estimates and structure coefficients.
#'
#' @param object An partR2 object returned from one of the partR2 functions
#' @param \dots Additional arguments; none are used in this method.
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
#' @author Martin Stoffel (martin.adam.stoffel@@gmail.com),
#'         Shinichi Nakagawa (s.nakagawa@unsw.edu.au),
#'         Holger Schielzeth  (holger.schielzeth@@uni-jena.de)
#'
#'
#' @keywords models
#'
#' @export
#'
#'
#'
summary.partR2 <- function(object, ...) {

    x <- object
    # prep
    CI_range <- paste0(sub('.*\\.', '', x$CI), "%")
    names(x$R2_pe_ci) <- c("Predictor(s)", "R2", "CI_lower", "CI_upper")
    names(x$SC_pe_ci) <- c("Predictor", "r(Yhat,x)", "CI_lower", "CI_upper")
    # names(x$CC_df) <- c("Predictor(s)", "R2", "CI_lower", "CI_upper")

    cat("\n\n")
    cat(paste0("R2 (", x$R2_type, ") and CI (",CI_range ,") for the full model: \n"))
    print(x$R2_pe_ci[1, 2:4], row.names = FALSE, digits = 3, right = FALSE)
    #cat(paste0("R2 = ", round(x$R2$R2, 3), ", CI = [", round(x$R2$lower, 3), ", ", round(x$R2$upper, 3), "]"))

    cat("\n")
    cat("----------")
    cat("\n\n")

    cat("Model estimates:\n")
    print(x$Ests_pe_ci, row.names = FALSE, digits = 3, right = FALSE)

    cat("\n")
    cat("----------")
    cat("\n\n")

    cat("Partitioned R2s:\n")

    if (nrow(x$R2_pe_ci) == 1) {
        print("No partitions selected.")
    } else {
        print(x$R2_pe_ci[2:nrow(x$R2_pe_ci), ], row.names = FALSE, digits = 2, right = FALSE)
    }

    cat("\n")
    cat("----------")
    cat("\n\n")

    cat("Structure coefficients:\n")
    print(x$SC_pe_ci, row.names = FALSE, digits = 3, right = FALSE)

    cat("\n")
    cat("----------")
    cat("\n\n")

    if (!(is.null(x$boot_warnings) & (is.null(x$boot_messages)))) {
        cat("Parametric bootstrapping resulted in warnings or messages:")
        cat("\n")
        cat("Check out$boot_warnings and out$boot_messages, where out is the partR2 output object.")
        cat("\n\n")
    }

}
