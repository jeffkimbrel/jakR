#' A Colorblind-friendly Palette Function
#'
#' This palette can be used all over
#'
#' @param n Number of colors to return
#'
#' @export
#' @examples
#' cbPalette(3)

cbPalette = colorRampPalette(c("#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7"))
