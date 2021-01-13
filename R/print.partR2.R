#' Print a partR2 object
#'
#' Displays the results a partR2object (i.e. the result of a partR2 function call) in a nice form.
#'
#' @param x partR2 object returned from one of the partR2 functions
#' @param round_to defaults to 4 (decimals)
#' @param \dots Additional arguments; none are used in this method.
#'
#'
#' @references
#'
#' Nakagawa, S., & Schielzeth, H. (2013). \emph{A general and simple method for obtaining R2 from
#' generalized linear mixed-effects models}. Methods in Ecology and Evolution, 4(2), 133-142.
#'
#' Newton, R. G., & Spurrell, D. J. (1967).  \emph{A development of multiple regression for the
#' analysis of routine data. Applied Statistics}. 51-64.
#'
#'
#' @keywords models
#'
#' @export
#'
#'
#'
print.partR2 <- function(x, round_to = 4, ...) {

    # prep
    CI_range <- paste0(sub('.*\\.', '', x$CI), "%")
    names(x$R2) <- c("Predictor(s)", "R2", "CI_lower", "CI_upper", "ndf")
    # check how many bootstraps
    num_boot <- ifelse(length(x$boot_warnings) == 0, NA, length(x$boot_warnings))
    cat("\n\n")
    cat(paste0("R2 (", x$R2_type, ") and ", CI_range, " CI for the full model: \n"))
    r2_df <- x$R2 %>%
             dplyr::mutate_if(is.numeric, round, round_to) %>%
             tibble::add_column(nboot = num_boot, .before = 5)
    # rename Full to Model
    r2_df[1, 1] <- "Model"
    print(as.data.frame(r2_df[1, 2:6]), row.names = FALSE, right = FALSE)
    #cat(paste0("R2 = ", round(x$R2$R2, 3), ", CI = [", round(x$R2$lower, 3), ", ", round(x$R2$upper, 3), "]"))
    cat("\n")
    cat("----------")
    cat("\n\n")
    cat("Part (semi-partial) R2:\n")

    if (nrow(x$R2) == 1) {
        print("No partitions selected.")
    } else {
    print(as.data.frame(r2_df), row.names = FALSE,right = FALSE)
    }

#
#     cat("\n")
#     cat("----------")
#     cat("\n\n")

}
