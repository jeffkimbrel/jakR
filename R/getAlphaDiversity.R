#' Alpha-Diversity Measures
#'
#' An extension of the phyloseq `estimate_richness` function to include Shannon's
#' Evenness and sample depth. It also combines the normal estimate_richness output
#' with the sample data dataframe.
#'
#' @param p A phyloseq object
#'
#' @export

getAlphaDiversity = function(p) {

  require("phyloseq")

  p = filter_taxa(p, function(x) sum(x >= 1) >= (1), TRUE)
  ad = estimate_richness(p) %>%
    rownames_to_column("SAMPLE") %>%
    mutate(ShannonEvenness = Shannon/log(Observed))

  d = as.data.frame(sample_sums(p)) %>%
    rownames_to_column("SAMPLE") %>%
    rename(SAMPLE_SUMS = "sample_sums(p)")

  if ("SAMPLE" %in% colnames(ad)) {
    s = data.frame(sample_data(p))
  } else {
    s = data.frame(sample_data(p)) %>%
      rownames_to_column("SAMPLE")
  }

  final = left_join(ad, s, by = "SAMPLE") %>%
    left_join(d, by = "SAMPLE")

  return(final)
}
