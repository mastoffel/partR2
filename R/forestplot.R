#' Plot a partR2 object
#'
#' Forestplot of the partR2 results
#'
#' @param x A partR2 object.
#' @param type Plot either "R2" or "SC" or "Ests" for R2s, structure coefficients
#' or model estimates.
#'
#'
#' @author Martin Stoffel (martin.adam.stoffel@@gmail.com),
#'         Holger Schielzeth  (holger.schielzeth@@uni-jena.de)
#'         Shinichi Nakagawa (s.nakagawa@unsw.edu.au),
#'
#'
#' @keywords models
#'
#' @import ggplot2 dplyr
#' @export
#'
#'


forestplot <- function(x, type = c("R2", "Ests", "SC")) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (length(type) > 1) type <- type[1]
    to_plot <- paste0(type, "_pe_ci")
    mod_out <- x[[to_plot]]

    # reshape Ests_pe_ci a bit
    if (type == "Ests") {
        mod_out <-
            mod_out %>% dplyr::mutate(group = ifelse(is.na(.data$group), "", .data$group)) %>%
            mutate(combs = paste(.data$term, .data$group, sep = " ")) %>%
            rename(pe = .data$estimate) %>%
            filter(.data$term != "(Intercept)") %>%
            mutate(effect = ifelse(.data$effect == "ran_pars", "random effects", "fixed effects"))
    } else {
        names(mod_out) <- c("combs", "pe", "CI_lower", "CI_upper")
        mod_out$combs <-
            factor(mod_out$combs, levels = rev(mod_out$combs))
    }

    if (type == "R2")
        x_label <- paste0("R2 (", x$R2_type, ") and CI")
    if (type == "SC")
        x_label <- "Structure coefficients and CI"
    if (type == "Ests")
        x_label <- "Model estimates and CI"

    p_out <-
        ggplot(aes_string("pe", "combs", xmax = "CI_upper", xmin = "CI_lower"),
               data = mod_out) +
        geom_vline(xintercept = 0, color = "black", alpha = 0.1) +
        geom_point(size = 3, shape = 21, col = "black", fill = "grey69",
                   alpha = 1) +
        theme_minimal(base_size = 11) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.x = element_line(),
            axis.title.y = element_blank(),
            text = element_text(size = 11)
        ) +
        xlab(x_label)
    # if bootstrap plot errorbars
    if (!is.na(x[[paste0(type, "_pe_ci")]][["CI_lower"]][1])) {
        p_out <- p_out + geom_errorbarh(alpha = 1, color = "black", height = 0)
    }
    # when estimates are plotted, split fixed and random effects into
    # different plots
    if (type == "Ests") {
        p_out <- p_out + facet_wrap(~ effect, scales = "free_y")
    }
    p_out

}
