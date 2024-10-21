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

  df = data.frame(pal)
  df$TAXA = as.integer(rownames(df))
  #df$rank <- seq.int(nrow(df))

  colnames(df) = c("COLOR", "TAXA")

  df = df |>
    dplyr::mutate(y = dplyr::ntile(n = rows)) |>
    dplyr::group_by(y) |>
    dplyr::mutate(x = seq(n()))

  p = df |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y, fill = COLOR)) +
    ggplot2::theme_void() +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_reverse()

  if (isTRUE(labels)) {
    p = p + ggplot2::geom_text(ggplot2::aes(label = COLOR), angle = label_angle)
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




