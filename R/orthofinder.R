#' Summarize an orthofinder TSV file
#'
#' @param tsv_path The path to the Orthogroups/Orthogroups.tsv file in the standard orthofinder output
#'
#' @export

orthofinder_summary = function(tsv_path) {
  require(tidyverse)

  orthogroups = read_delim(tsv_path, delim = "\t") %>%
    pivot_longer(cols = -contains('Orthogroup'), names_to = "GENOME", values_to = "GENE") %>%
    drop_na() %>%
    group_by(Orthogroup, GENOME) %>%
    mutate(GENE = length(unlist(strsplit(GENE, ',')))) %>%
    pivot_wider(names_from = GENOME, values_from = GENE)

  return(orthogroups)
}


#' Summarize single copy and orthogroups found in all
#'
#' @param tsv_path The path to the Orthogroups/Orthogroups.tsv file in the standard orthofinder output
#'
#' @export

orthofinder_complete_clusters = function(o) {

  single_copy = o %>%
    pivot_longer(cols = -contains('Orthogroup'), names_to = "GENOME", values_to = "GENE") %>%
    mutate(GENE = replace_na(GENE, 0)) %>%
    group_by(Orthogroup) %>%
    summarize(MAX = max(GENE), MIN = min(GENE), SUM = sum(GENE)) %>%
    filter(MAX == 1, MIN == 1) %>%
    pull(Orthogroup)

  ubiquitous_orthologs = o %>%
    pivot_longer(cols = -contains('Orthogroup'), names_to = "GENOME", values_to = "GENE") %>%
    drop_na() %>%
    group_by(Orthogroup) %>%
    tally() %>%
    filter(n == max(n)) %>%
    pull(Orthogroup)

    return(list("SINGLE" = single_copy, "UBIQUITOUS" = ubiquitous_orthologs))
}
