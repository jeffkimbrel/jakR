#' JAK black ggplot theme
#'
#' Produces a black background and light colors for figures with a dark background.
#' @export

jak_theme_black = function(base_size = 10, base_family = "", keySize = 0.3){

  require("ggplot2")

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      line = element_line(colour = "white", size = 0.5, linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "white", colour = "white", size = 0.5, linetype = 1),
      text = element_text(family = base_family,
                          face = "plain",
                          color = "white",
                          size = base_size,
                          lineheight = 0.9,
                          hjust = 0.5,
                          vjust = 0.5,
                          angle = 0,
                          margin = ggplot2::margin(), debug = FALSE),
      strip.background = element_rect(fill = "transparent", color = "transparent"),
      strip.text = element_text(color = "gray80", size = 10),
      plot.background = element_rect(fill = "black", color = "black"),
      panel.background = element_rect(fill = "transparent", color = "gray80", size = 1),
      panel.grid.major = element_line(colour = "grey30", size = 0.2),
      panel.grid.minor = element_line(colour = "transparent"),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text = element_text(color = "gray80"),
      legend.key = element_rect(colour = 'transparent', fill = 'transparent'),
      strip.text.y = element_text(angle = 0, size = 10, hjust = 0)
    )
}
