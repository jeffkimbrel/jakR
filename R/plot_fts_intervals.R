#' Plot intervals for a run_fts() dataframe
#'
#' @param df A dataframe from run_fts()
#' @param feature A feature (or vector of features) to subset. Overrides the n parameter
#' @param n Subset by the top n features as determined by the order of the features in df
#'
#' @export
#'

plot_fts_intervals = function(df, n = 10, feature = c(), p = colorbook$modernUI(2)) {
  taxa = jakR::get_fts_intervals(df) %>%
    pull(F) %>%
    unique()
  taxa = taxa[1:n]

  if (length(feature) > 0) {
    taxa = feature
  }

  gg = df$i %>%
    filter(F %in% taxa) %>%
    ggplot(aes(x = T, y = fit)) +
    geom_area(aes(group = I, fill = D, y = ifelse(I == 'NONE', 0, fit))) +
    scale_fill_manual(values = p) +
    geom_line(color = "gray50") +
    geom_line(aes(y = fit + se), color = "gray50", linetype = 2) +
    geom_line(aes(y = fit - se), color = "gray50", linetype = 2) +
    geom_point(pch = 21, size = 1, color = "gray50") +
    theme(legend.position = "bottom") +
    labs(x = "Days", y = "FIT") +
    facet_wrap(~F) +
    geom_hline(yintercept = 0, color = "red")

  return(gg)

}
