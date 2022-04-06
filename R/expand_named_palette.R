#' Expanded Color Palette with Names
#'
#' Give a vector or a named vector of rgb hex colors, and this will expand them
#' out adding alpha values, and appending 1, 2, 3, etc, to the names. If there
#' are no names, then they will be assigned names from the alphabet.
#'
#' @param pal A (named) vector with colors
#' @param return_list Whether to return the values as a list (TRUE, default) or a named vector (FALSE).
#'
#' @export

expand_named_palette = function(pal, return_list = TRUE) {

  require(tidyverse)

  # first, fix if the palette doesn't have names
  if (is.null(names(pal))) {
    names(pal) = LETTERS[1:(length(pal))]
  }

  # define expansion
  expansion = tibble(code = c("1", "2", "3", "4", "5", "6", "7", "8"),
                     alpha = c("11", "33", "55", "77", "99", "BB", "DD", "EE"))

  # turn pal into a tibble
  colors = tibble(name = names(pal),
                  color = as.vector(pal))

  # do the expansion
  df = merge(colors, expansion) %>%
    mutate(name = paste(name, code, sep = "")) %>%
    mutate(color = paste(color, alpha, sep = "")) %>%
    select(name, color) %>%
    rbind(colors) %>%
    arrange(name)

  if (return_list == TRUE) {
    return(split(df$color, df$name))
  } else {
    expanded_palette.v <- as.character(df$color)
    names(expanded_palette.v) = as.character(df$name)
    return(expanded_palette.v)
  }
}
