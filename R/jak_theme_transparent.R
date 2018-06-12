#' JAK transparent ggplot theme
#'
#' Produces a transparent background and light colors for figures with a dark background.
#' @export

jak_theme_transparent <- function(base_size = 10, base_family = "", keySize = 0.3){

  require("ggplot2")

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      line = element_line(colour = "white", size = 0.5, linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "white", colour = "white", size = 0.5, linetype = 1),
      text = element_text(face = "plain", colour = "white"),
      strip.background = element_rect(fill = "transparent", colour = "transparent"),
      strip.text = element_text(color = "gray80", size = 10),
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = "gray80", size = 1),
      panel.grid.major = element_line(colour = "grey50", size = 0.2),
      panel.grid.minor = element_line(colour = "grey30", size = 0.5),
      axis.text = element_text(color = "gray80")
    )
}
