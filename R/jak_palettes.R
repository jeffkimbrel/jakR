#' A Colorblind-friendly Palette Function
#'
#' These palettes can be used all over.
#'
#' @param n Number of colors to return
#' @param colors An optional vector of rgb or colors to use. Overwrites `p`
#' @param p A named color palette
#' @param reverse A boolean for whether the palette should be reversed or not
#'
#' @export

palette_jak = function(p = "bay",
                       colors = NULL,
                       n = 2,
                       reverse = FALSE) {

  palettes = list(
    "cb"            = c("#cc79a7","#0072b2","#56b4e9","#009e73","#f0e442","#e69f00","#d55e00"),
    "modernUI"      = c('#09D879','#0069FF','#D6D6D6','#FF4040','#FFDD00'),
    "helpcenter"    = c('#E40035','#6900C5','#2F4AF9','#00BB6A','#8DC000','#FFC400','#FFAA3A','#FF662E','#FB4A4A'),
    "bulma"         = c('#01D1B2','#5178FA','#3273DC','#23D160','#FFDD57','#FF3860'),
    "blackdiamond"  = c('#6C7F50','#98C67C','#34557F','#FF635A','#D98626','#D3BF96','#685A5C','#786592','#78849C',
                        '#DC443A','#ACDEE6','#EFBD47','#B0583A','#E2C2C7','#0088CE','#E9BF9B','#652D86','#512B1B',
                        '#D1D4D3','#FE9666','#7BBBB2'),
    "sachi"         = c('#005eac','#E7004C','#00C36B','#6D6462','#068CD6','#FE1E36','#F57C00','#0097A7','#A35FCA'),
    "inslife"       = c('#80bec8','#6EB5C0','#e8e2e6','#FFCCBB','#83a0cd','#dee6f2','#DEAFB1','#AFDEDC','#DCC48E',
                        '#CEB28D','#56CBF9','#E9D7C3'),
    "pastelrainbow" = c('#E8CACA','#FFE5A8','#F8FF97','#BCFFA4','#A6FFD3','#B9E3FF','#C1CCFF','#E4CFFF','#F1CEF4'),
    "zissou"        = c("#000000","#3B9AB2","#78B7C5","#EBCC2A","#E1AF00","#F21A00"),
    "bay"           = c('#00496f','#0f85a0','#edd746','#ed8b00','#dd4124'),
    "winter"        = c('#2d2926','#33454e','#537380','#81a9ad','#ececec'),
    "lake"          = c('#362904','#54450f','#45681e','#4a9152','#64a8a8','#85b6ce','#cde5f9','#eef3ff'),
    "sailboat"      = c('#6e7cb9','#7bbcd5','#d0e2af','#f5db99','#e89c81','#d2848d')
  )

  if (is.null(colors)) {
    cols = colorRampPalette(unname(unlist(palettes[p])))(n)
  } else {
    if (all_are_valid_colors(colors)) {
      cols = colorRampPalette(colors)(n)
    } else {
      stop("ERROR: one of the colors isn't valid")
    }
  }

  if (isTRUE(reverse)) {
    return(rev(cols))
  } else {
    return(cols)
  }
}




#' Check if a string contains valid color strings
#'
#' @param x A vector of colors in rgb or text string
#'
#' @export

all_are_valid_colors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}



# palette_jak$jak = expand_named_palette(pal = c("blue" = "#037bcf",
#                                                "red" = "#A81000",
#                                                "yellow" = "#FFC00A",
#                                                "green" = "#00A800",
#                                                "darkblue" = "#002984",
#                                                "purple" = "#4d0051",
#                                                "black" = "#222222",
#                                                "white" = "#EFEFEF")
#                                        )
