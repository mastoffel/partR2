#' Plot a partR2 object
#'
#' Forestplot of the partR2 results
#'
#' @param x A partR2 object.
#' @param type Plot either "R2" or "SC".
#'
#'
#' @author Martin Stoffel (martin.adam.stoffel@@gmail.com),
#'         Holger Schielzeth  (holger.schielzeth@@uni-jena.de)
#'         Shinichi Nakagawa (s.nakagawa@unsw.edu.au),
#'
#'
#' @keywords models
#'
#' @import ggplot2
#' @export
#'
#'


forestplot <- function(x, type = c("R2", "SC")) {

    if (length(type) > 1) type <- type[1]
   #  if (toplot == "CC") mod_out <- x$CC_df
    to_plot <- paste0(type, "_pe_ci")
    mod_out <- x[[to_plot]]
    names(mod_out) <- c("combs", "pe", "cilow", "cihigh")
    mod_out$combs <- factor(mod_out$combs, levels = rev(mod_out$combs))

    if (type == "R2") x_label <- paste0(x$R2_type, "_R2")
    if (type == "SC") x_label <- "Structure coefficient"
    # only check whether there is NA in the first df element, then nboot wasn't specified
    if (!is.na(x[[paste0(type, "_boot")]][1,1])) {
        ggplot(aes_string("pe", "combs", xmax = "cihigh", xmin = "cilow"), data = mod_out) +
            # geom_point(size = 3, color = "grey69") + # abc_out
            geom_errorbarh(alpha=0.4, color="black",height = 0) +
            geom_point(size = 3, shape = 21, col = "black", fill = "grey69") +
            # geom_errorbarh(alpha=0.4, color="black",height = 0) +
            theme_minimal() +
            theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(),
                axis.title.y = element_blank(),
                text = element_text(size=15)) +
            #scale_y_discrete(labels = c("Breeding\nhabitat\nice vs. land", "log(Abundance)",
            #    "Sexual Size\nDimorphism")) +
            xlab(x_label) +
            geom_vline(xintercept = 0, color = "black", alpha = 0.1)
    } else {
        ggplot(aes_string("pe", "combs"), data = mod_out) +
            geom_point(size = 3, shape = 21, col = "black", fill = "grey69") +
            # geom_errorbarh(alpha=0.4, color="black",height = 0) +
            theme_minimal() +
            theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(),
                axis.title.y = element_blank(),
                text = element_text(size=15)) +
            #scale_y_discrete(labels = c("Breeding\nhabitat\nice vs. land", "log(Abundance)",
            #    "Sexual Size\nDimorphism")) +
            xlab(x_label) +
            geom_vline(xintercept = 0, color = "black", alpha = 0.1)
    }


}
