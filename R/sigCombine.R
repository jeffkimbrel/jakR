#' Combine DESeq sigtab contrasts
#'
#' This one is a little wonky, because the dds object needs to be already made
#' and living in the environment. Oh, and it has to be named 'dds'.
#'
#' @param group Grouping used in the dds object
#' @param reference The name of the reference group
#' @param comparison The name of the comparison group
#' @param sigtab The sigtab object
#' @param runBH P-value corrections using BH
#'
#' @export

sigCombine = function(group = "group", reference = reference, comparison = comparison, sigtab, runBH = TRUE) {
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
