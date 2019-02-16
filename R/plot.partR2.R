#' Plot a partR2 object
#'
#' Forestplot of the partR2 results
#'
#' @param x A partR2 object.
#' @param toplot Commonality coefficients ("CC") or structure coefficients ("SC")
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


plot.partR2 <- function(x, toplot) {

   #  if (toplot == "CC") mod_out <- x$CC_df
    mod_out <- x$R2_pe_ci
    names(mod_out) <- c("combs", "pe", "cilow", "cihigh")
    mod_out$combs <- factor(mod_out$combs, levels = rev(mod_out$combs))

    if (!is.na(x$R2_boot)) {
        ggplot(aes(pe, combs, xmax = cihigh, xmin = cilow), data = mod_out) +
            # geom_point(size = 3, color = "grey69") + # abc_out
            geom_errorbarh(alpha=0.4, color="black",height = 0) +
            geom_point(size = 3, shape = 21, col = "black", fill = "grey69") +
            # geom_errorbarh(alpha=0.4, color="black",height = 0) +
            theme_minimal() +
            theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(color = '#333333'),
                axis.title.y = element_blank()) +
            #scale_y_discrete(labels = c("Breeding\nhabitat\nice vs. land", "log(Abundance)",
            #    "Sexual Size\nDimorphism")) +
            xlab("R2") +
            geom_vline(xintercept = 0, color = "black", alpha = 0.1)
    } else {
        ggplot(aes(pe, combs), data = mod_out) +
            geom_point(size = 3, shape = 21, col = "black", fill = "grey69") +
            # geom_errorbarh(alpha=0.4, color="black",height = 0) +
            theme_minimal() +
            theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(color = '#333333'),
                axis.title.y = element_blank()) +
            #scale_y_discrete(labels = c("Breeding\nhabitat\nice vs. land", "log(Abundance)",
            #    "Sexual Size\nDimorphism")) +
            xlab("R2") +
            geom_vline(xintercept = 0, color = "black", alpha = 0.1)
    }


}
