#' JAK ggplot theme
#'
#' Modify theme of ggplots
#' @export
#' @param base_size Font size for most things
#' @param base_color Color for most text and lines
#' @param grid_color Color for grid lines
#' @param strip_size Font size for facet strip titles
#' @param legend_size Font size for legend
#' @param base_family Font family

jak_theme_black = function(base_size = 10, base_color = "gray80", grid_color = "gray30", strip_size = 10, legend_size = 10, base_family = "") {

  require("ggplot2")

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      legend.key = element_rect(color = 'transparent', fill = 'transparent'),
      legend.text = element_text(size = legend_size),
      legend.title = element_text(size = legend_size),
      line = element_line(color = base_color, size = 0.5, linetype = 1, lineend = "butt"),
      rect = element_rect(fill = 'transparent', color = base_color, size = 0.5, linetype = 1),
      text = element_text(family = base_family,
                          face = "plain",
                          color = base_color,
                          size = base_size,
                          lineheight = 0.9,
                          hjust = 0.5,
                          vjust = 0.5,
                          angle = 0,
                          margin = ggplot2::margin(), debug = FALSE),
      plot.background = element_rect(fill = "black", color = "black"),
      panel.background = element_rect(fill = "transparent", color = base_color, size = 1),
      panel.grid.major = element_line(color = grid_color, size = 0.2),
      panel.grid.minor = element_line(color = "transparent"),
      panel.border = element_rect(fill = "transparent", color = base_color, size = 1),
      axis.text = element_text(color = base_color),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.background = element_rect(fill = "transparent", color = "transparent"),
      strip.text = element_text(color = base_color, size = strip_size, hjust = 0),
      strip.text.x = element_text(margin = ggplot2::margin(1,0,1,0, "mm")),
      strip.text.y = element_text(angle = 0)
    )
}

