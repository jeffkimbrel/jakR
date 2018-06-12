#' getSig
#'
#' Produces a df and plot of the significant taxa in a DESeq sigtab. Requires the phyloseq object and contrast field.
#' @export

getSig = function(group = "group", s = sigtab, phylo = phyloTemp) {
  groupSig = s %>%
    select(KO, log2FoldChange, padj, REF, COMP) %>%
    filter(padj <= 0.05)

  ps.melt = psmelt(phylo)
  ps.melt = dplyr::rename(ps.melt, group = "group")

  ps.melt = ps.melt %>%
    filter(OTU %in% groupSig$KO) %>%
    filter(group %in% groupSig$COMP | group %in% groupSig$REF)

  df.SE = summarySE(data = ps.melt, measurevar = "Abundance", groupvar = c("group", "OTU"))
  dodge <- position_dodge(width = 0.8)

  # plot = ggplot(df.SE, aes(x = group, y = mean)) +
  #   jak_theme() +
  #   #geom_col(color = "black", aes(fill = group)) +
  #   geom_point(pch = 21, size = 5, aes(fill = group)) +
  #   geom_errorbar(position = dodge, aes(ymin = mean - sd, ymax = mean + sd)) +
  #   facet_wrap(~OTU) +
  #   scale_fill_manual(values = pal(length(unique(ps.melt$group))))

  plot = ggplot(df.SE, aes(x = OTU, y = mean)) +
    jak_theme() +
    geom_col(color = "black", position = dodge, aes(fill = group)) +
    #geom_point(pch = 21, size = 3, position = dodge, aes(fill = group)) +
    geom_errorbar(position = dodge, aes(color = group, ymin = mean - sd, ymax = mean + sd)) +
    scale_fill_manual(values = jakPalette(length(unique(ps.melt$group)))) +
    scale_color_manual(values = jakPalette(length(unique(ps.melt$group))))

  return(list("plot" = plot, "df" = groupSig))
}
