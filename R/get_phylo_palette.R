#' getPhyloPalette
#'
#' @export
#' @param p A Phyloseq object
#' @param n Max number of taxa to be included in the "primary" palette, with the rest in a secondary, less dramatic palette.

get_phylo_palette = function (p, n = 10) {
  require("phyloseq")
  require("tidyverse")
  require("speedyseq")

  pal = list()

  tax_levels = colnames(tax_table(p))

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
      top_n(n = 10, wt = SUM) %>%
      mutate(TAXA = as.character(TAXA)) %>%
      pull(TAXA)

    bottom_taxa = sorted_taxa %>%
      filter(!TAXA %in% top_taxa) %>%
      mutate(TAXA = as.character(TAXA)) %>%
      pull(TAXA)

    top_palette = colorbook$helpcenter(length(top_taxa))
    names(top_palette) = top_taxa

    bottom_palette = sample(colorbook$inslife(length(bottom_taxa))) # these are randomly chosen, not in order
    names(bottom_palette) = bottom_taxa

    taxa_palette = c(top_palette, bottom_palette)
    taxa_palette["Other"] = "#555555"
    taxa_palette["NA"] = "#000000"

    pal[[level]] = taxa_palette
  }
  return(pal)
}

