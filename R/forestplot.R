#' Plot a partR2 object
#'
#' Forestplot of the partR2 results
#'
#' @param x A partR2 object.
#' @param type Plot either "R2" or "SC" or "Ests" for R2s, structure coefficients
#' or model estimates.
#' @param line_size Controls size of all lines in the forestplot. Defaults to 0.5 which usually looks good.
#' @param text_size Base text size, default is 12.
#' @param point_size Point size, default is 3.
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


forestplot <- function(x, type = c("R2", "Ests", "SC", "IR2"), line_size = 0.5, text_size = 12, point_size = 3) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (length(type) > 1) type <- type[1]
    to_plot <- type
    mod_out <- x[[to_plot]]

    # reshape Ests_pe_ci a bit
    if (type == "Ests") {
        mod_out <-
            mod_out %>% dplyr::mutate(group = ifelse(is.na(.data$group), "", .data$group)) %>%
            dplyr::mutate(combs = paste(.data$term, .data$group, sep = " ")) %>%
            dplyr::mutate(combs = gsub("var__", "", .data$combs)) %>%
            dplyr::rename(pe = .data$estimate) %>%
            dplyr::filter(.data$term != "(Intercept)") %>%
            dplyr::mutate(combs = ifelse(.data$group == "Residual", "Residual", .data$combs)) %>%
            dplyr::mutate(effect = ifelse(.data$effect == "ran_pars", "random effects (variance)", "fixed effects"))
    } else {
        names(mod_out) <- c("combs", "pe", "CI_lower", "CI_upper")
        mod_out$combs <-
            factor(mod_out$combs, levels = rev(mod_out$combs))
    }

    if (type == "R2") x_label <-  paste0("R2 (", x$R2_type, ") and CI")
    if (type == "SC") x_label <- "Structure coefficients and CI"
    if (type == "IR2") x_label <-  bquote(Inclusive~R^2~(SC^2%*%R^2~full)~and~CI)
    if (type == "Ests") x_label <- "Model estimates and CI"


    p_out <-
        ggplot(aes_string("pe", "combs", xmax = "CI_upper", xmin = "CI_lower"),
               data = mod_out) +
        geom_vline(xintercept = 0, color = "#1b262c", linetype='dashed', size = line_size) +
        #geom_point(size = point_size, shape = 21, col = "black", fill = "grey69", # "grey69"
        #           alpha = 1, stroke = line_size) +
        geom_point(size = point_size, col = "#1b262c", # "grey69"
                   alpha = 1) +
        theme_classic(base_line_size = line_size, base_size = text_size) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(color = "#1b262c"),
            axis.title.x = element_text(margin=margin(t=8), color = "#1b262c")
        ) +
        xlab(x_label)
    # if bootstrap plot errorbars
    if (!is.na(x[[type]][["CI_lower"]][1])) {
        p_out <- p_out + geom_errorbarh(alpha = 1, color = "#1b262c", height = 0,
                                        size = line_size)
    }
    # when estimates are plotted, split fixed and random effects into
    # different plots
    if (type == "Ests") {
        p_out <- p_out + facet_wrap(~ effect, scales = "free_y")
    }
    p_out

}
