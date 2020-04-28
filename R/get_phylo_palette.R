#' getPhyloPalette
#'
#' @export
#' @param p A Phyloseq object
#' @param n Max number of taxa to be included in the "primary" palette, with the rest in a secondary, less dramatic palette
#' @param random_high If TRUE, randomize the color palette. If FALSE, colors will be arranged from high to low taxa abundance
#' @param random_low If TRUE, randomize the color palette. If FALSE, colors will be arranged from high to low taxa abundance
#' @param palette_high Expanding color palette for the high taxa
#' @param palette_low Expanding color palette for the low taxa
#' @param other_color Color to use for the aggregated Other taxa

get_phylo_palette = function (p, n = 10, palette_high = colorbook$helpcenter, palette_low = colorbook$inslife, random_high = FALSE, random_low = FALSE, other_color = "#555555", OTU = F) {
  require("phyloseq")
  require("tidyverse")
  require("speedyseq")

  pal = list()

  tax_levels = colnames(tax_table(p))

  if (OTU == TRUE) {
    tax_levels = c(tax_levels, "OTU")
  }

  if (random_high == FALSE) {
    top_palette = palette_high(n)
  } else {
    top_palette = sample(palette_high(n))
  }

  df = speedyseq::psmelt(p)

  for (level in tax_levels) {
    print(level)

    df.level = df[, c(level, "Abundance")]
    colnames(df.level) = c("TAXA", "ABUNDANCE")

    sorted_taxa = df.level %>%
      group_by(TAXA) %>%
      summarise(SUM = sum(ABUNDANCE)) %>%
      arrange(desc(SUM))

    top_taxa = sorted_taxa %>%
      top_n(n = n, wt = SUM) %>%
      mutate(TAXA = as.character(TAXA)) %>%
      pull(TAXA)

    bottom_taxa = sorted_taxa %>%
      filter(!TAXA %in% top_taxa) %>%
      mutate(TAXA = as.character(TAXA)) %>%
      pull(TAXA)

    top_palette_round = top_palette
    names(top_palette_round) = top_taxa

    if (random_low == FALSE) {
      bottom_palette = palette_low(length(bottom_taxa))
    } else {
      bottom_palette = sample(palette_low(length(bottom_taxa)))
    }
    names(bottom_palette) = bottom_taxa

    taxa_palette = c(top_palette_round, bottom_palette)
    taxa_palette["Other"] = other_color
    taxa_palette["NA"] = "#000000"

    pal[[level]] = taxa_palette
  }
  return(pal)
}

