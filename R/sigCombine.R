#' Combine DESeq sigtab contrasts
#'
#' @param group Grouping used in the dds object
#' @param reference The name of the reference group
#' @param comparison The name of the comparison group
#' @param sigtab The sigtab object
#' @param runBH P-value corrections using BH
#'
#' @export

sigCombine = function(group = "group", dds = dds, reference = reference, comparison = comparison, sigtab, runBH = TRUE) {
  df = as.data.frame(results(dds, cooksCutoff = FALSE, contrast = c(group, comparison, reference)))
  df$KO = rownames(df)
  df$REF = reference
  df$COMP = comparison
  df$negLogPval = -log10(df$padj)

  sigtab = rbind(sigtab, df)

  if (runBH == TRUE) {
    sigtab$padj = p.adjust(sigtab$pvalue, method = "BH")
    sigtab$negLogPval = -log10(sigtab$padj)
  }

  sigtab$negLogPval = -log10(sigtab$padj)
  sigtab = sigtab[rev(order(sigtab$padj)),]
  return(sigtab)

}
