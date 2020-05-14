#' Get intervals for a run_fts() dataframe
#'
#' @param df A dataframe from run_fts()
#'
#' @export

get_fts_intervals = function(df) {
  df$i %>%
    filter(INTERVAL != "NONE") %>%
    select(FEATURE, DIRECTION, INTERVAL, Area, p.value, START, END) %>%
    unique() %>%
    mutate(FDR = p.adjust(p.value, method = "BH")) %>%
    select(-p.value) %>%
    arrange(FDR, desc(abs(Area)))
}
