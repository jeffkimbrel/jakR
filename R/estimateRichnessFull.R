#' Alpha-Diversity Measures
#'
#' An extension of the phyloseq `estimate_richness` function to include Faith's PD and Shannon's Evenness
#' @export

estimate_richness_full = function(phyloObject, runPD = FALSE) {

  sampleData = as.data.frame(sample_data(phyloObject))

  rich = estimate_richness(phyloObject)
  rich$ShannonEvenness = rich$Shannon / log(rich$Observed)
  row.names(rich) = gsub("\\.", "-", row.names(rich))
  rich$SAMPLE = as.factor(row.names(rich))

  rich = left_join(rich, sampleData, by = "SAMPLE")

  # PD
  if (runPD == TRUE) {

    require("picante")

    comm = as.data.frame(t(otu_table(phyloObject)))
    tre = phy_tree(phyloObject)
    pd.results = pd(comm, tre)
    pd.results = select(pd.results, PD)

    pd.results$SAMPLE = as.factor(rownames(pd.results))
    rich = left_join(rich, pd.results, by = "SAMPLE")
  }

  row.names(rich) = rich$SAMPLE
  rich
}
