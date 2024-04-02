#' Calculate Total and Pairwise Adonis Values
#'
#' @export
#' @param x A dataframe of values with samples in columns, and observations in rows
#' @param factors A dataframe with samples to be compared as rownames, and the groupings as column 1. It is easy to get this with a "select()" statement.
#' @param sim.method The distance method
#' @param p.adjust.m P-value adjustment method
#' @param perm = Number of permutations


pairwiseAdonis <- function(x, factors, sim.method = 'bray', p.adjust.m = 'BH', perm = 999) {
  factors = data.frame(factors)
  colnames(factors) = "GROUP"
  factors$SAMPLE = rownames(factors)
  sampleOrder = rownames(x)
  factors <- factors[sampleOrder, ]
  combinations = combn(unique(factors$GROUP), 2)
  pairs = c()
  F.Model = c()
  R2 = c()
  p.value = c()
  groupSize = c()
  for (pair in 1:ncol(combinations)) {
    group1 = combinations[1, pair]
    group2 = combinations[2, pair]
    factors.s = dplyr::filter(factors, GROUP == group1 | GROUP ==
                                group2)
    x.s = x[factors.s$SAMPLE, ]
    adonis.results = adonis2(x.s ~ factors.s$GROUP, method = sim.method,
                             perm = perm)
    pairs = c(pairs, paste0(group1, " vs ", group2))
    groupSize = c(groupSize, paste0(table(factors.s$GROUP)[as.character(group1)],
                                    "-", table(factors.s$GROUP)[as.character(group2)]))
    F.Model = c(F.Model, adonis.results$F[1])
    R2 = c(R2, adonis.results$R2[1])
    p.value = c(p.value, adonis.results$`Pr(>F)`[1])
  }
  p.adjusted = p.adjust(p.value, method = p.adjust.m)
  adonis.results = adonis2(x ~ factors$GROUP, method = sim.method,
                           perm = perm)
  pairs = c(pairs, "ALL")
  F.Model = c(F.Model, adonis.results$F[1])
  R2 = c(R2, adonis.results$R2[1])
  p.value = c(p.value, adonis.results$`Pr(>F)`[1])
  p.adjusted = signif(p.adjusted, 3)
  p.adjusted = c(p.adjusted, "-")
  groupSize = c(groupSize, "ALL")

  pairw.res = data.frame(pairs, groupSize, F.Model, R2, p.value,
                         p.adjusted)
  colnames(pairw.res) = c("PAIR", "GROUP_SIZE", "F.Model",
                          "R2", "p-value", p.adjust.m)
  return(pairw.res)
}
