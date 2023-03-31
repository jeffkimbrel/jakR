#' Summarize a fastq_info.py file
#'
#' @export
#' @param file Output from fastq_info.py
#' @param plot Return a ggplot object

fastq_info_summary = function(file) {

  require(tidyverse)
  require(jsonlite)
  require(crayon)
  require(pastecs)

  df = read_delim(file, delim = "\t", comment = "#", show_col_types = FALSE) %>%
    mutate(PAIR = case_when(
      PAIR == "F" ~ "Forward",
      PAIR == "R" ~ "Reverse"
    ))

  sample_count = length(unique(df$SAMPLE))

  # do the F and R read counts match?
  pair_count_match = df %>%
    select(-FILE, -INDEX, -MD5, -RUN_INFO) %>%
    pivot_wider(names_from = PAIR, values_from = TOTAL_READS) %>%
    mutate(MM = case_when(
      Forward == Reverse ~ TRUE,
      TRUE ~ FALSE))

  good_pairs = pair_count_match %>%
    filter(MM == TRUE)

  if (nrow(good_pairs) == sample_count) {
    message(crayon::green("Read counts match between F and R files for all samples"))
  } else {
    bad_pairs = pair_count_match %>%
      filter(MM == "FALSE") %>%
      pull(SAMPLE)

    message(crayon::yellow("Some samples had read counts that do not match between F and R"))

    for (pair in bad_pairs) {
      message(crayon::yellow(pair))
    }

  }

  # is there one and only one RUN id?
  run_id = df %>%
    select(-INDEX, -FILE, -MD5, -TYPE, -PAIR, -TOTAL_READS) %>%
    mutate(RUN_INFO = gsub("\'", "\"", RUN_INFO)) %>%
    mutate(json = map(RUN_INFO, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(json) %>%
    pivot_longer(cols = c(everything(), -SAMPLE, -RUN_INFO), names_to = "RUN_ID", values_to = "READS", values_drop_na = TRUE) %>%
    group_by(RUN_ID) %>%
    summarise(TOTAL_READS = sum(READS))

  if (nrow(run_id) > 1) {
    message(crayon::yellow("These fastq files were run on different Illumina Runs"))
  } else{
    message(crayon::green("All reads appear to be from the same Illumina Run"))
  }

  s = df %>%
    group_by(SAMPLE) %>%
    summarise(TOTAL_READS = sum(TOTAL_READS)) %>%
    pull(TOTAL_READS) %>%
    pastecs::stat.desc(norm = F)

  s_for_return = s %>%
    as.data.frame() %>%
    rownames_to_column("METRIC") %>%
    dplyr::rename("VALUE" = ".")

  p = df %>%
    group_by(SAMPLE) %>%
    summarise(TOTAL_READS = sum(TOTAL_READS)) %>%
    mutate(DIFF_FROM_MEAN = TOTAL_READS - mean(TOTAL_READS)) %>%
    ggplot(aes(x = reorder(SAMPLE, TOTAL_READS), y = TOTAL_READS)) +
      geom_hline(yintercept = s['mean'], color = "gray70", linetype = 3) +
      geom_point(size = 4, pch = 21, aes(fill = abs(DIFF_FROM_MEAN))) +
      scale_fill_fermenter() +
      theme(legend.position = "right") +
      labs(fill = "Difference from Mean", y = "Count of Reads per Sample", x = "Sample Name", title = file) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))


  return(list("run_id" = run_id,
              "stats" = s_for_return,
              "pairs" = pair_count_match,
              "plot" = p)
  )

}

#' Summarize a fastq_filter.py file in amplicon mode
#'
#' @export
#' @param file Output from fastq_filter.py
#' @param text_size Text size for the filtered plot

fastq_filter_summary_amplicon = function(file, text_size = 8) {

  require(tidyverse)

  df = read_delim(file, delim = "\t", comment = '#') %>%
    select(SAMPLE, ORDER_VERIFIED, CF_READS_OUT, CF_READS_REMOVED, CF_BP_OUT, CF_BP_REMOVED) %>%
    pivot_longer(cols = c(CF_READS_OUT, CF_READS_REMOVED, CF_BP_OUT, CF_BP_REMOVED))

  a = df %>%
    separate(name, into = c("FILTER", "TYPE", "STEP")) %>%
    ggplot(aes(x = reorder(SAMPLE, value), y = value, fill = STEP)) +
    jak_theme(t ="black") +
    geom_col() +
    facet_grid(~TYPE, scales = "free_x") +
    coord_flip() +
    scale_fill_manual(values = palette_jak$bay(2)) +
    labs(subtitle = file, title = "Contamination Filter", x = "Sample", y = "Reads") +
    geom_text(aes(label = ifelse(STEP == "OUT", value, NA)), size = 2, color = "gray70", hjust = 0.3)


  b = df %>%
    separate(name, into = c("FILTER", "TYPE", "STEP")) %>%
    select(-FILTER) %>%
    pivot_wider(names_from = STEP, values_from = value) %>%
    mutate(REMOVED = 100 * REMOVED / (OUT + REMOVED))  %>%
    ggplot(aes(x = reorder(SAMPLE, REMOVED), y = REMOVED, fill = REMOVED)) +
    jak_theme(t ="black") +
    geom_col() +
    facet_grid(~TYPE, scales = "free_x") +
    coord_flip() +
    scale_fill_viridis_c() +
    labs(subtitle = file, title = "Reads Removed (% of total)", x = "Sample", y = "Reads Removed") +
    theme(legend.position = "none")

  df_final = df %>%
    pivot_wider(names_from = name, values_from = value)

  l = list("filtered" = a, "reads_removed" = b, "df" = df_final)
}
