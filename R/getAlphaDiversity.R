#' Alpha-Diversity Measures
#'
#' An extension of the phyloseq `estimate_richness` function to include Shannon's Evenness and Faith's PD (with runPD = TRUE). It also combines the normal estimate_richness output with the sample data dataframe.
#' Running Faith's PD can add a significant amount of time if the tree is large. Faith's PD also requires a rooted tree. If the phyloseq object tree is not rooted, then this function loads the phytools package and does midpoint.root().
#' @export

getAlphaDiversity = function(p) {

  require("phyloseq")

  p = filter_taxa(p, function(x) sum(x >= 1) >= (1), TRUE)

  sampleData = data.frame(sample_data(p))

  rich = estimate_richness(p)
  rich$ShannonEvenness = rich$Shannon / log(rich$Observed)
  row.names(rich) = gsub("\\.", "-", row.names(rich))
  rich$SAMPLE = as.factor(row.names(rich))

  rich = merge(sampleData, rich)
  rownames(rich) = rich$SAMPLE

  depth = as.data.frame(sample_sums(p))
  depth$SAMPLE = rownames(depth)
  colnames(depth) = c("SAMPLE_SUMS", "SAMPLE")

  rich = merge(rich, depth)
  rownames(rich) = rich$SAMPLE

  rich
}
