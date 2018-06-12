#' Combine DESeq sigtab contrasts
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
  return(sigtab)

}
