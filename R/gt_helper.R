#' Web theme for GT
#'
#' @param data gt object
#' @param align column alignment (left, right, center)
#' @param background_color table background
#' @param font_color font color
#' @param column_label_size header font size
#' @param table_font_size table font size
#'
#' @export

gt_theme <- function(data,
                     align = "center",
                     background_color = "#222222",
                     font_color = "#DDDDDD",
                     column_label_size = 18,
                     table_font_size = 16,
                     table_padding = 2,
                     ...) {

  require(gt)

  data %>%
    tab_options(
      column_labels.font.size = column_label_size,
      table.background.color = background_color,
      table.font.size = table_font_size,
      table.font.color = font_color,
      table.font.color.light = font_color,
      data_row.padding = px(table_padding),
      ...
    ) %>%
    cols_align(
      align = align,
      columns = everything()
    )
}
