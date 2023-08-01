#' A Color Palette Function
#'
#' Miscellaneous palettes
#'
#' Bay, winter, lake and sailboat from https://github.com/jakelawlor/PNWColors
#'
#' @param n Number of colors to return
#'
#' @export
#' @examples
#' palette_jak$jak$blue1

palette_jak = list()

palette_jak$host     = c("Ns" = "#4DAF4A", "Ms" = "#4DAF4A", "M. salina" = "#4DAF4A", "N. salina" = "#4DAF4A", "Pt" = "#A65628", "P. tricornutum" = "#A65628")

palette_jak$zissou   = colorRampPalette(c("black", "#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))
palette_jak$bay      = colorRampPalette(c('#00496f', '#0f85a0', '#edd746', '#ed8b00', '#dd4124'))
palette_jak$winter   = colorRampPalette(c('#2d2926', '#33454e', '#537380', '#81a9ad', '#ececec'))
palette_jak$lake     = colorRampPalette(c('#362904', '#54450f', '#45681e', '#4a9152', '#64a8a8', '#85b6ce', '#cde5f9', '#eef3ff'))
palette_jak$sailboat = colorRampPalette(c('#6e7cb9', '#7bbcd5', '#d0e2af', '#f5db99', '#e89c81', '#d2848d'))

palette_jak$jak = expand_named_palette(pal = c("blue" = "#037bcf",
                                               "red" = "#A81000",
                                               "yellow" = "#FFC00A",
                                               "green" = "#00A800",
                                               "darkblue" = "#002984",
                                               "purple" = "#4d0051",
                                               "black" = "#222222",
                                               "white" = "#EFEFEF")
                                       )
