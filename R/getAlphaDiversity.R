#' Alpha-Diversity Measures
#'
#' An extension of the phyloseq `estimate_richness` function to include Shannon's
#' Evenness and sample depth. It also combines the normal estimate_richness output
#' with the sample data dataframe.
#'
#' The new version removes much of the phyloseq code, running them instead in Vegan or calculating manually.
#'
#' @param p A phyloseq object
#' @param id_col The name of the column with sample IDs
#'
#' @export

get_alpha_diversity = function (p, id_col = "SAMPLE") {
  require("phyloseq")
  require("vegan")

  p = filter_taxa(p, function(x) sum(x >= 1) >= (1), TRUE)

  SAMPLE_SUMS = as.data.frame(sample_sums(p)) %>%
    rownames_to_column(id_col) %>%
    dplyr::rename(SAMPLE_SUM = "sample_sums(p)")

  S = as.data.frame(otu_table(p)) %>%
    t() %>%
    vegan::estimateR() %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(id_col) %>%
    left_join(SAMPLE_SUMS, by = id_col)

  SIMPSON = as.data.frame(otu_table(p)) %>%
    rownames_to_column("ASV") %>%
    pivot_longer(cols = -ASV, names_to = id_col, values_to = "ABUNDANCE") %>%
    group_by_at(id_col) %>%
    mutate(Pi = ABUNDANCE / sum(ABUNDANCE)) %>%
    left_join(S, by = id_col) %>%
    mutate(Pi2 = Pi ^ 2) %>%
    group_by_at(id_col) %>%
    summarise(SIMPSON_D = sum(Pi2), .groups = 'drop') %>%
    mutate(SIMPSON_EVENNESS = 1 - SIMPSON_D, SIMPSON_INVERSE = 1 / SIMPSON_D) %>%
    left_join(S, by = id_col)


  SHANNON = as.data.frame(otu_table(p)) %>%
    rownames_to_column("ASV") %>%
    pivot_longer(cols = -ASV, names_to = id_col, values_to = "ABUNDANCE") %>%
    group_by_at(id_col) %>%
    mutate(Pi = ABUNDANCE / sum(ABUNDANCE)) %>%
    filter(Pi > 0) %>%
    summarise(SHANNON_H = -sum(Pi*log(Pi)), .groups = 'drop') %>%
    mutate(SHANNON_ENS = exp(SHANNON_H)) %>%
    left_join(SIMPSON, by = id_col) %>%
    mutate(SHANNON_E = SHANNON_H / log(S.obs)) %>%
    select(all_of(id_col), "SAMPLE_SUM", "S.obs", "S.chao1", "se.chao1", "S.ACE", "se.ACE", "SIMPSON_D", "SIMPSON_EVENNESS", "SIMPSON_INVERSE", "SHANNON_H", "SHANNON_ENS", "SHANNON_E")

  data.frame(sample_data(p)) %>%
    dplyr::rename("SAMPLE_orig" = id_col) %>%
    rownames_to_column(id_col) %>%
    tibble()

  s = data.frame(sample_data(p))

  if (id_col %in% colnames(s)) {
    s = data.frame(sample_data(p)) %>%
      dplyr::rename("SAMPLE_orig" = id_col) %>%
      rownames_to_column(id_col) %>%
      tibble()
  } else {
    s = data.frame(sample_data(p)) %>%
      rownames_to_column(id_col) %>%
      tibble()
  }

  final = left_join(s, SHANNON, by = id_col)

  return(final)
}

#' Alpha-Diversity Measures (deprecated)
#'
#' An extension of the phyloseq `estimate_richness` function to include Shannon's
#' Evenness and sample depth. It also combines the normal estimate_richness output
#' with the sample data dataframe.
#'
#' @param p A phyloseq object
#'
#' @export

getAlphaDiversity = function(p) {
  .Deprecated("get_alpha_diversity(p)")
}
