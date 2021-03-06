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
#' @keywords models
#'
#' @export
#'
#'

forestplot <- function(x, type = c("R2", "BW", "SC", "IR2", "Ests"), line_size = 0.5, text_size = 12, point_size = 3) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (length(type) > 1) type <- type[1]
    to_plot <- type
    mod_out <- x[[to_plot]]

    if (type == "R2") {
        mod_out[mod_out$term == "Full", 1] <- "Model"
        names(mod_out) <- c("combs", "pe", "CI_lower", "CI_upper", "ndf")
    } else if (type %in% c("Ests", "BW")) {
        mod_out <- mod_out %>%
            dplyr::select(.data$term, .data$estimate, .data$CI_lower, .data$CI_upper) %>%
            dplyr::rename(Predictor = .data$term, BW = .data$estimate) %>%
            dplyr::filter(!(.data$Predictor == "(Intercept)"))
        names(mod_out) <- c("combs", "pe", "CI_lower", "CI_upper")
    } else {
        names(mod_out) <- c("combs", "pe", "CI_lower", "CI_upper")
    }
    mod_out$combs <- factor(mod_out$combs, levels = rev(mod_out$combs))
    # reshape Ests_pe_ci a bit
    # if (type %in% c("Ests", "BW")) {
    #     mod_out <-
    #         mod_out %>% dplyr::mutate(group = ifelse(is.na(.data$group), "", .data$group)) %>%
    #         dplyr::mutate(combs = paste(.data$term, .data$group, sep = " ")) %>%
    #         dplyr::mutate(combs = gsub("var__", "", .data$combs)) %>%
    #         dplyr::rename(pe = .data$estimate) %>%
    #         dplyr::filter(.data$term != "(Intercept)") %>%
    #         dplyr::mutate(combs = ifelse(.data$group == "Residual", "Residual", .data$combs)) %>%
    #         dplyr::mutate(effect = ifelse(.data$effect == "ran_pars", "random effects (variance)", "fixed effects"))
    # } else {
    #     names(mod_out) <- c("combs", "pe", "CI_lower", "CI_upper")
    #     mod_out$combs <-
    #         factor(mod_out$combs, levels = rev(mod_out$combs))
    # }

   # if (type == "R2") x_label <-  paste0("R2 (", x$R2_type, ") and CI")
    if (type == "R2") x_label <-   bquote(R^2~and~CI)
    if (type == "SC") x_label <- "Structure coefficients and CI"
    if (type == "IR2") x_label <- bquote(Inclusive~R^2~and~CI)  #bquote(Inclusive~R^2~(SC^2%*%R^2~full)~and~CI)
    if (type == "BW") x_label <- "Beta weights and CI"
    if (type == "Ests") x_label <- "Model estimates and CI"

    col_all <- "#2E3440"
    p_out <-
        ggplot2::ggplot(ggplot2::aes_string("pe", "combs", xmax = "CI_upper", xmin = "CI_lower"),
               data = mod_out) +
        ggplot2::geom_vline(xintercept = 0, color = col_all, linetype='dashed', size = line_size) +
        #geom_point(size = point_size, shape = 21, col = "black", fill = "grey69", # "grey69"
        #           alpha = 1, stroke = line_size) +
        ggplot2::theme_classic(base_line_size = line_size, base_size = text_size) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = col_all),
            axis.title.x = ggplot2::element_text(margin=ggplot2::margin(t=8), color = col_all)
        ) +
        ggplot2::xlab(x_label)
    # if bootstrap plot errorbars
    if (!is.na(x[[type]][["CI_lower"]][1])) {
        p_out <- p_out + ggplot2::geom_errorbarh(alpha = 1, color = col_all, height = 0,
                                        size = line_size)
    }
    p_out <- p_out +
        ggplot2::geom_point(size = point_size, shape = 21, fill = "#ECEFF4", col = col_all, # "grey69"
                   alpha = 1, stroke = line_size)

    # if (type %in% c("R2", "IR2")) {
    #     p_out <- p_out +
    #                 scale_x_continuous(limits = c(0,1))
    # }
    # when estimates are plotted, split fixed and random effects into
    # different plots
    # if (type %in% c("BW", "Ests")) {
    #     p_out <- p_out + facet_wrap(~ effect, scales = "free")
    # }
    p_out

}
