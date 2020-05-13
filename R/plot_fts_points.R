#' Plot points for a run_fts() dataframe
#'
#' @param df A dataframe from run_fts()
#' @param feature A feature (or vector of features) to subset. Overrides the n parameter
#' @param n Subset by the top n features as determined by the order of the features in df
#'
#' @export

plot_fts_points = function(df, n = 10, s = c(), p = colorbook$modernUI(2)) {
  taxa = get_intervals(df) %>%
    pull(F) %>%
    unique()
  taxa = taxa[1:n]

  if (length(s) > 0) {
    taxa = s
  }

  A = df$a %>%
    filter(F %in% taxa)

  B = df$a_fit %>%
    filter(F %in% taxa)

  ggplot(A, aes(x = time, y = abundance, color = class)) +
    jak_theme_black() +
    geom_boxplot(fill = NA, aes(group = paste(time, class))) +
    geom_line(data = B, size = 1, aes(x = T, y = FIT, color = GROUP)) +
    geom_line(data = B, aes(x = T, y = FIT + SE, color = GROUP), linetype = 3) +
    geom_line(data = B, aes(x = T, y = FIT - SE, color = GROUP), linetype = 3) +
    facet_wrap(~F, scales = "free_y") +
    scale_fill_manual(values = p) +
    scale_color_manual(values = p) +
    theme(legend.position = "bottom")
}
