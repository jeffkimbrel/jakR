#' getPhyloPalette
#'
#' @export
#' @param p A Phyloseq object
#' @param level The taxonomic level to make a palette for
#' @param n Max number of taxa to be included in the "primary" palette, with the rest in a secondary, less dramatic palette.

getPhyloPalette = function(p, level = "phylum", n = 10) {

  require("phyloseq")

  df = psmelt(p)
  df = df[,c(level, "Abundance")]
  colnames(df) = c("TAXA", "ABUNDANCE")

  df = df %>%
    group_by(TAXA) %>%
    summarise_all(sum) %>%
    arrange(desc(ABUNDANCE))

  # high
  highTaxaList = sort(df[1:n,]$TAXA) %>% droplevels()
  highPalette = tol$rainbow
  highTaxaPalette = highPalette(length(highTaxaList))
  names(highTaxaPalette) = highTaxaList


  # low
  lowTaxaList = sort(df[n+1:nrow(df),]$TAXA) %>% droplevels()
  lowPalette = tol$pale
  lowTaxaPalette = lowPalette(length(lowTaxaList))
  names(lowTaxaPalette) = lowTaxaList

  taxaPalette = c(highTaxaPalette, lowTaxaPalette)


  taxaPalette["_Other"] = "#333333"
  taxaPalette["NA"] = "#000000"

  return(taxaPalette)
}

