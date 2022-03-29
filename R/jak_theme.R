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
#' @param bg_color background color
#' @param t Themes

jak_theme = function(t = "light",
                     base_size = 10,
                     plot_title_size = 12,
                     axis_title_size = 11,
                     base_color = "gray30",
                     plot_title_color = "gray30",
                     axis_title_color = "gray30",
                     bg_color = "transparent",
                     grid_color = "gray80",
                     strip_size = 10,
                     strip_color = "transparent",
                     strip_line = 0,
                     legend_size = 10,
                     legend_position = "bottom",
                     base_family = "")
{

  require("ggplot2")
  require("ggtext")

  if (t == "black") {
    base_color = "gray80"
    plot_title_color = "gray80"
    axis_title_color = "gray80"
    bg_color = "black"
    grid_color = "gray30"
  }

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      line = element_line(color = base_color, size = 0.2, linetype = 1, lineend = "butt"),
      rect = element_rect(fill = 'transparent', color = base_color, size = 0.5, linetype = 1),
      text = element_text(family = base_family,
                          face = "plain",
                          color = base_color,
                          size = base_size,
                          lineheight = 0.9,
                          hjust = 0.5,
                          vjust = 0.5,
                          angle = 0,
                          margin = ggplot2::margin()),

      legend.background = element_blank(),
      legend.box.background = element_rect(fill = "transparent", color = base_color, size = 0.2),
      legend.text = element_text(size = legend_size),
      legend.title = element_text(size = legend_size, face="bold"),
      legend.key = element_rect(color = "transparent", fill = "transparent"),
      legend.position = legend_position,

      plot.background = element_rect(fill = bg_color, color = bg_color),
      plot.title = element_text(size = plot_title_size, face="bold", color = plot_title_color, hjust = 0),

      panel.background = element_rect(fill = "transparent", color = base_color),
      panel.grid.major = element_line(color = grid_color, size = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = "transparent", color = base_color, size = 0.5),

      axis.text = element_text(color = base_color),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),

      axis.title.x = element_text(color=axis_title_color, size=axis_title_size, face="bold"),
      axis.title.y = element_text(color=axis_title_color, size=axis_title_size, face="bold", angle = 90),

      strip.background = element_blank(),
      strip.text.x = element_textbox(
        size = strip_size,
        color = base_color,
        fill = strip_color,
        width = unit(1, "npc"),
        halign = 0.5, linetype = 1, r = unit(3, "pt"), linewidth = strip_line,
        padding = margin(2, 0, 1, 0), margin = margin(2, 2, 2, 2)),
      strip.text.y = element_textbox(
        size = strip_size,
        color = base_color,
        fill = strip_color,
        halign = 0.5, linetype = 1, r = unit(3, "pt"), linewidth = strip_line, hjust = 0,
        padding = margin(4, 0, 1, 0), margin = margin(2, 2, 2, 2))
    )
}



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

jak_theme_black = function(base_size = 10, base_color = "gray80", grid_color = "gray30", strip_size = 10, strip_color = "transparent", legend_size = 10, base_family = "") {

  .Deprecated("jak_theme(t = 'black')")

}


