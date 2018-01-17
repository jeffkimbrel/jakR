#' Sets ASV abundance values below x to zero
#'
#'
#'
#'
#' @export

zeroOTU = function(p, min = 5) {
  p = filter_taxa(p, function(x) sum(x >= min) >= (1), TRUE)
  otutab = otu_table(p)
  otutab[otutab < min ] <- 0
  otu_table(p) = otutab
  p
}
