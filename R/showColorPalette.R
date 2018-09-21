#' showColorPalette
#'
#' @export
#' @param pal A color palette

showColorPalette = function(pal) {

  require("tidyverse")
  df = data.frame(pal)
  df$TAXA = as.factor(rownames(df))
  df$VAL = 1
  colnames(df) = c("COLOR", "TAXA", "VALUE")

  df = df  %>%
    arrange(desc(TAXA))

  p = ggplot(df, aes(x = TAXA, y = VALUE)) +
    jak_theme() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x     = element_blank(),
          axis.text.x      = element_blank(),
          axis.ticks.x     = element_blank(),
          axis.ticks.y     = element_blank(),
          axis.title.y     = element_blank(),
          axis.text.y      = element_blank()) +
    geom_col(aes(fill = COLOR)) +
    scale_fill_identity() +
    facet_wrap( ~ TAXA, scales = "free_x")
  return(p)
}
