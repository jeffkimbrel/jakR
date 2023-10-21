#' showColorPalette
#'
#' @export
#' @param pal A color palette

show_color_palette = function(pal,
                              labels = FALSE,
                              label_angle = 0) {

  rows = ceiling(sqrt(length(pal)))

  require("tidyverse")
  df = data.frame(pal)
  df$TAXA = as.integer(rownames(df))
  #df$rank <- seq.int(nrow(df))

  colnames(df) = c("COLOR", "TAXA")

  df = df |>
    mutate(y = ntile(n = rows)) |>
    group_by(y) |>
    mutate(x = seq(n()))

  p = df |>
    ggplot(aes(x = x, y = y, fill = COLOR)) +
    theme_void() +
    geom_tile() +
    scale_fill_identity() +
    scale_y_reverse()

  if (isTRUE(labels)) {
    p = p + geom_text(aes(label = COLOR), angle = label_angle)
  }

  p
}

