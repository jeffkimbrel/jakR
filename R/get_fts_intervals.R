#' Get intervals for a run_fts() dataframe
#'
#' @param df A dataframe from run_fts()
#'
#' @export

get_fts_intervals = function(df) {
  df$i %>%
    filter(I != "NONE") %>%
    select(F, D, I, Area, p.value) %>%
    unique() %>%
    mutate(FDR = p.adjust(p.value, method = "BH")) %>%
    select(-p.value) %>%
    arrange(FDR, desc(abs(Area)))
}
