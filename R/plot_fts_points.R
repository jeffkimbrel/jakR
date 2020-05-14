#' Plot points for a run_fts() dataframe
#'
#' @param df A dataframe from run_fts()
#' @param feature A feature (or vector of features) to subset. Overrides the n parameter
#' @param n Subset by the top n features as determined by the order of the features in df
#'
#' @export

plot_fts_points = function(df, n = 10, feature = c(), p = colorbook$modernUI(2)) {
  taxa = jakR::get_fts_intervals(df) %>%
    pull(FEATURE) %>%
    unique()
  taxa = taxa[1:n]

  if (length(feature) > 0) {
    taxa = feature
  }

  A = df$a %>%
    filter(FEATURE %in% taxa)

  B = df$a_fit %>%
    filter(FEATURE %in% taxa)

  gg = ggplot(A, aes(x = TIME, y = abundance, color = class)) +
    geom_boxplot(fill = NA, aes(group = paste(TIME, class))) +
    geom_line(data = B, size = 1, aes(x = TIME, y = FIT, color = GROUP)) +
    geom_line(data = B, aes(x = TIME, y = FIT + SE, color = GROUP), linetype = 3) +
    geom_line(data = B, aes(x = TIME, y = FIT - SE, color = GROUP), linetype = 3) +
    facet_wrap(~FEATURE, scales = "free_y") +
    scale_fill_manual(values = p) +
    scale_color_manual(values = p) +
    theme(legend.position = "bottom")

  return(gg)
}
