#' getSig
#'
#' Produces a df and plot of the significant taxa in a DESeq sigtab.
#'
#' @param group A contrast field used to make the dds object
#' @param s A sigtab object
#' @param phylo A phyloseq object
#' @param minplot minimum baseMean to show up in the plot.
#'
#' @export

getSig = function(group = "group", s = sigtab, phylo = phyloTemp, minPlot = 1) {
  groupSig = s %>%
    select(KO, log2FoldChange, padj, REF, COMP, baseMean) %>%
    filter(padj <= 0.05)

  ps.melt = as.tibble(psmelt(phylo))
  ps.melt = dplyr::rename(ps.melt, "group" = group)

  ps.melt = ps.melt %>%
    filter(OTU %in% groupSig$KO) %>%
    filter(group %in% groupSig$COMP | group %in% groupSig$REF)

  df.SE = summarySE(data = ps.melt, measurevar = "Abundance", groupvar = c("group", "OTU"))
  dodge <- position_dodge(width = 0.95)

  # plot = ggplot(df.SE, aes(x = group, y = mean)) +
  #   jak_theme() +
  #   #geom_col(color = "black", aes(fill = group)) +
  #   geom_point(pch = 21, size = 5, aes(fill = group)) +
  #   geom_errorbar(position = dodge, aes(ymin = mean - sd, ymax = mean + sd)) +
  #   facet_wrap(~OTU) +
  #   scale_fill_manual(values = pal(length(unique(ps.melt$group))))

  groupSigBaseMean = groupSig %>%
    filter(baseMean >= minPlot) %>%
    select(KO) %>%
    unique()

  df.SE.plot = df.SE[df.SE$OTU %in% groupSigBaseMean$KO, ]

  plot = ggplot(df.SE.plot, aes(x = OTU, y = mean)) +
    jak_theme() +
    geom_col(color = "black", position = dodge, aes(fill = group)) +
    geom_errorbar(position = dodge, aes(color = group, ymin = mean - sd, ymax = mean + sd)) +
    scale_fill_manual(values = jakPalette(length(unique(ps.melt$group)))) +
    scale_color_manual(values = jakPalette(length(unique(ps.melt$group))))

  return(list("plot" = plot, "df" = groupSig))
}
