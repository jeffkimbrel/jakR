#' showColorPalette
#'
#' @export
#' @param pal A color palette
#' @param labels Boolean for whether to show the rgb value as a label
#' @param label_angle The rotational angle for the label

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

#' Show all color palettes in the jak_palettes object
#'
#' @export

show_all_color_palettes = function() {
  input2 <- lapply(jak_palettes, as.data.frame, stringsAsFactors = FALSE)
  df <- bind_rows(input2, .id = "Palette") |>
    dplyr::select(Palette, RGB = `X[[i]]`)

  df = df |>
    group_by(Palette) |>
    mutate(x = seq(n()))

  df |>
    ggplot(aes(x = x, y = Palette, fill = RGB)) +
      theme_minimal() +
      geom_tile() +
      scale_fill_identity() +
      scale_y_discrete(limits = rev)

}




