#' JAK ggplot theme
#'
#' Used for figures with a white background.
#' @export

jak_theme <- function(base_size = 10, base_family = "", keySize = 0.3){

  require("ggplot2")

  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", size = 0),
      panel.background = element_rect(fill = "white", color = "black"),
      legend.key = element_rect(color = NA, fill = NA),
      legend.key.size = unit(keySize, "cm"),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10),
      strip.text.y = element_text(angle = 0, size = 10)
    )
}
