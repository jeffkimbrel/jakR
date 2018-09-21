#' Combine low-abundant taxa into a single category
#'
#' @export
#' @param p A phyloseq object
#' @param level Taxonomic level to use
#' @param min Number of taxa to show, in addition to "_Other"
#' @param xAxis Variable for the x axis

phyloOther = function(p, level = "family", min = 10, xAxis = "SAMPLE") {

  require("tidyverse")

  if (min < 1) {
    min = 1
  }

  p2 = p

  df = cbind(as.data.frame(taxa_sums(p2)), as.data.frame(tax_table(p2)[, level]))
  colnames(df) = c("Abundance", "taxa")

  df2 = aggregate(Abundance ~ taxa, data = df, sum)

  # rank
  order.Abundance <- order(-df2$Abundance)
  df2$rank <- NA
  df2$rank[order.Abundance] <- 1:nrow(df2)
  df2 = df2[order(df2$rank),]
  passTaxa = df2[1:min,]$taxa

  allTaxa = unique(as.data.frame(tax_table(p))[,level])
  diff = setdiff(allTaxa, passTaxa)

  taxTable = tax_table(p)

  taxTable[, level][taxTable[, level] %in% diff] = "_Other"
  tax_table(p) = taxTable[, c("domain", level)]
  phylo = tax_glom(p, level)

  levelUpper = toupper(level)

  plot = plot_bar(phylo, fill = level, x = xAxis)

  l = list("phylo" = phylo, "plot" = plot)
  return(l)
}
