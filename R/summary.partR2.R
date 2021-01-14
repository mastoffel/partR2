#' Complete summary of a partR2 object
#'
#' Displays extended results of partR2, including R2,
#' part (semi-partial) R2, inclusive R2, structure coefficients and beta weights.
#'
#' @param object partR2 object returned from one of the partR2 functions
#' @param round_to Defaults to 4 (decimals)
#' @param ests Defaults to FALSE, if TRUE, also prints raw model estimates.
#' @param \dots Additional arguments; not used at the moment
#'
#' @return No return value, prints extended summary of partR2 calculation.
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
#'
#' @keywords models
#'
#' @export
#'
#'
#'
summary.partR2 <- function(object, round_to = 4, ests = FALSE, ...) {

    x <- object
    # prep
    CI_range <- paste0(sub('.*\\.', '', x$CI), "%")
    names(x$R2) <- c("Predictor(s)", "R2", "CI_lower", "CI_upper", "ndf")
    names(x$SC) <- c("Predictor", "SC", "CI_lower", "CI_upper")
    names(x$IR2) <- c("Predictor", "IR2", "CI_lower", "CI_upper")
    x$BW <- x$BW %>%
        dplyr::select(.data$term, .data$estimate, .data$CI_lower, .data$CI_upper) %>%
        dplyr::rename(Predictor = .data$term, BW = .data$estimate)
    # check how many bootstraps
    num_boot <- ifelse(length(x$boot_warnings) == 0, NA, length(x$boot_warnings))
    cat("\n\n")
    cat(paste0("R2 (", x$R2_type, ") and ", CI_range, " CI for the full model: \n"))
    r2_df <- x$R2 %>%
        dplyr::mutate_if(is.numeric, round, round_to) #%>%
        #tibble::add_column(nboot = num_boot, .before = 5)
    # rename Full to Model
    r2_df[1, 1] <- "Model"
    print(as.data.frame(r2_df[1, 2:5]), row.names = FALSE, right = FALSE)
    #cat(paste0("R2 = ", round(x$R2$R2, 3), ", CI = [", round(x$R2$lower, 3), ", ", round(x$R2$upper, 3), "]"))

    cat("\n")
    cat("----------")
    cat("\n\n")

    cat("Part (semi-partial) R2:\n")

    if (nrow(r2_df) == 1) {
        print("No partitions selected.")
    } else {
        print(as.data.frame(r2_df), row.names = FALSE, right = FALSE)
    }

    cat("\n")
    cat("----------")
    cat("\n\n")

    cat("Inclusive R2 (SC^2 * R2):\n")
    IR2_df <- x$IR2 %>% dplyr::mutate_if(is.numeric, round, round_to)
    print(as.data.frame(IR2_df), row.names = FALSE, right = FALSE)

    cat("\n")
    cat("----------")
    cat("\n\n")

    cat("Structure coefficients r(Yhat,x):\n")
    SC_df <- x$SC %>% dplyr::mutate_if(is.numeric, round, round_to)
    print(as.data.frame(SC_df), row.names = FALSE, right = FALSE)

    cat("\n")
    cat("----------")
    cat("\n\n")

    if (isTRUE(ests)) {
        x$Ests <- x$Ests %>%
            dplyr::select(.data$term, .data$estimate, .data$CI_lower, .data$CI_upper) %>%
            dplyr::rename(Predictor = .data$term, Estimate = .data$estimate) %>%
            dplyr::filter(!(.data$Predictor == "(Intercept)"))

        cat("Model estimates \n")
        ests_df <- x$Ests %>% dplyr::mutate_if(is.numeric, round, round_to)
        print(as.data.frame(ests_df), row.names = FALSE, right = FALSE)

        cat("\n")
        cat("----------")
        cat("\n\n")

    }
    #cat("Model estimates:\n")
    cat("Beta weights (standardised estimates)\n")
    ests_df <- x$BW %>% dplyr::mutate_if(is.numeric, round, round_to)
    print(as.data.frame(ests_df), row.names = FALSE, right = FALSE)

    cat("\n")
    cat("----------")
    cat("\n\n")

    if (!(is.null(unlist(x$boot_warnings)) & (is.null(unlist(x$boot_messages))))) {
        cat("Parametric bootstrapping resulted in warnings or messages:")
        cat("\n")
        cat("Check r2obj$boot_warnings and r2obj$boot_messages.")
        cat("\n\n")
    }

}
