#' Alpha-Diversity Measures
#'
#' An extension of the phyloseq `estimate_richness` function to include Shannon's Evenness and Faith's PD (with runPD = TRUE). It also combines the normal estimate_richness output with the sample data dataframe.
#' Running Faith's PD can add a significant amount of time if the tree is large. Faith's PD also requires a rooted tree. If the phyloseq object tree is not rooted, then this function loads the phytools package and does midpoint.root().
#' @export

estimate_richness_full = function(phyloObject, runPD = FALSE) {

  require("phyloseq")
  require("dplyr")

  phyloObject = filter_taxa(phyloObject, function(x) sum(x >= 1) >= (1), TRUE)

  sampleData = data.frame(sample_data(phyloObject))

  rich = estimate_richness(phyloObject)
  rich$ShannonEvenness = rich$Shannon / log(rich$Observed)
  row.names(rich) = gsub("\\.", "-", row.names(rich))
  rich$SAMPLE = as.factor(row.names(rich))

  rich = merge(sampleData, rich)
  rownames(rich) = rich$SAMPLE

  depth = as.data.frame(sample_sums(phyloObject))
  depth$SAMPLE = rownames(depth)
  colnames(depth) = c("SAMPLE_SUMS", "SAMPLE")

  rich = merge(rich, depth)
  rownames(rich) = rich$SAMPLE

  # PD
  if (runPD == TRUE) {

    require("picante")

    comm = as.data.frame(t(otu_table(phyloObject)))
    tre = phy_tree(phyloObject)
    if (!is.rooted(tre)) {
      require("phytools")
      tre = midpoint.root(tre)
    }

    pd.results = pd(comm, tre)
    pd.results = select(pd.results, PD)

    pd.results$SAMPLE = as.factor(rownames(pd.results))
    rich = left_join(rich, pd.results, by = "SAMPLE")
  }

  row.names(rich) = rich$SAMPLE
  rich
}
