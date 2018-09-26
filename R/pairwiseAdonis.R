#' Calculate Total and Pairwise Adonis Values
#'
#' @export
#' @param x A dataframe of values with samples in columns, and observations in rows
#' @param factors A dataframe with samples to be compared as rownames, and the groupings as column 1. It is easy to get this with a "select()" statement.
#' @param sim.method The distance method
#' @param p.adjust.m P-value adjustment method
#' @param perm = Number of permutations


pairwiseAdonis <- function(x, factors, sim.method = 'bray', p.adjust.m = 'BH', perm = 999){

  library("vegan")
  factors = data.frame(factors)
  colnames(factors) = "GROUP"
  factors$SAMPLE = rownames(factors)

  # sort both dataframes so the samples are in the same order
  sampleOrder = rownames(x)
  factors <- factors[ sampleOrder, ]

  combinations = combn(unique(factors$GROUP), 2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  groupSize = c()

  # pairwise
  for(pair in 1:ncol(combinations)) {

    # get comparison group names
    group1 = combinations[1, pair]
    group2 = combinations[2, pair]

    # subset factors and x
    factors.s = filter(factors, GROUP == group1 | GROUP == group2)
    x.s = x[factors.s$SAMPLE,]

    # run Adonis and collect results
    adonis.results = adonis(x.s ~ factors.s$GROUP, method = sim.method, perm = perm)
    pairs = c(pairs, paste0(group1, " vs ", group2))
    groupSize = c(groupSize, paste0(table(factors.s$GROUP)[as.character(group1)], "-", table(factors.s$GROUP)[as.character(group2)]))
    F.Model = c(F.Model, adonis.results$aov.tab[1, 4])
    R2 = c(R2, adonis.results$aov.tab[1,5])
    p.value = c(p.value, adonis.results$aov.tab[1,6])
  }

  # adjust p.val of pairwise only
  p.adjusted = p.adjust(p.value, method = p.adjust.m)

  # run all
  adonis.results = adonis(x ~ factors$GROUP, method = sim.method, perm = perm)
  pairs = c(pairs, "ALL")
  F.Model = c(F.Model, adonis.results$aov.tab[1, 4])
  R2 = c(R2, adonis.results$aov.tab[1,5])
  p.value = c(p.value, adonis.results$aov.tab[1,6])
  p.adjusted = signif(p.adjusted, 3)

  p.adjusted = c(p.adjusted, "-")
  groupSize = c(groupSize, "ALL")
  pairw.res = data.frame(pairs, groupSize, F.Model, R2, p.value, p.adjusted)

  colnames(pairw.res) = c("PAIR", "GROUP_SIZE", "F.Model", "R2", "p-value", p.adjust.m)

  return(pairw.res)
}
