#' Plot Faith's PD
#'
#' This function iteratively removes the least abundant sequence in a phyloseq sample, re-roots the tree, calculates Faith's PD, and plots the results.
#' Returns a list with a dataframe (results$df) and two different plots.
#'
#' results$p1 is from iteratively removing the lowest abundance sequences per sample. This is done on a per sample basis - if
#' a sequence is lowest in sample A but not sample B, it is removed from sample A but retained in sample B until it is lowest there.
#' If there are multiple sequences tied for lowest in a sample, they are all removed at once - hence there can be jumps along the x axis.
#'
#' results$p2 uses the same data and criteria as results$p1, except now the x axis is the sequence count when removed. An X value of 10 for example
#' means that is the PD with all sequences of abundance 10 or less removed.
#'
#' @export

plotPD = function(phyloObject) {

  require("phyloseq")
  require("dplyr")
  require("picante")

  total = 1000
  df = data.frame()

  while (total > 1) {

    phyloObject = filter_taxa(phyloObject, function(x) sum(x >= 1) >= (1), TRUE)

    comm = as.data.frame(otu_table(phyloObject))

    j.df = data.frame()

    for(i in names(comm)) {
      j = min(comm[comm[,i] > 0, i])
      comm[comm[,i] == j, i] = 0

      j.df = rbind(j.df, data.frame("SAMPLE" = i, "j" = j))
    }

    otu_table(phyloObject) = otu_table(comm, taxa_are_rows = T)

    if (ntaxa(phyloObject) > 0) {

      comm = t(comm)

      tre = phy_tree(phyloObject)
      if (!is.rooted(tre)) {
        require("phytools")
        tre = midpoint.root(tre)
      }
      pd.results = pd(comm, tre)
      pd.results$SAMPLE = rownames(pd.results)

      pd.results = merge(pd.results, j.df)

      df = rbind(df, pd.results)

      total = sum(pd.results$SR)
    }

  }

  p1 = ggplot(df, aes(x = SR, y = PD, color = SAMPLE)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    xlab("X most abundant sequences")

  p2 = ggplot(df, aes(x = j, y = PD, color = SAMPLE)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    scale_x_log10() +
    xlab("Minimum sequence abundance")

  list("df" = df, "p1" = p1, "p2" = p2)

}
