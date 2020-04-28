#' Combine low-abundant taxa into a single category
#'
#' @export
#' @param p A phyloseq object
#' @param level Taxonomic level to use
#' @param n Number of taxa to show, in addition to "Other"

phylo_other = function (p, level = "phylum", n = 10) {
  require("tidyverse")
  require("speedyseq")

  p_original_taxa_cols = c(colnames(tax_table(p)), "OTU")

  p_melted =
    speedyseq::psmelt(p) %>%
    rename('renamed_taxa' = all_of(level)) %>%
    mutate(renamed_taxa = as.character(renamed_taxa)) %>%
    select(-p_original_taxa_cols[p_original_taxa_cols != level])

  top_n_taxa = p_melted %>%
    group_by(renamed_taxa) %>%
    summarise(taxa_sum = sum(Abundance)) %>%
    arrange(desc(taxa_sum)) %>% # this is really only need so that top_n_taxa doesn't have the top 10 in a random order
    top_n(n, wt = taxa_sum) %>% # grab the top n according to highest sum
    droplevels() %>%
    pull('renamed_taxa') # convert to a vector

  all_taxa = c("Other", top_n_taxa)
  all_taxa = factor(all_taxa, levels = all_taxa)

  p_melted = p_melted %>%
    mutate(renamed_taxa = ifelse(renamed_taxa %in% top_n_taxa, renamed_taxa, "Other")) %>% # actually change the names
    #select(-OTU) %>% # remove OTU column, as we are going to aggregate and don't need it
    group_by_at(vars(-Abundance)) %>% # group by everything except abundance
    summarise(Abundance = sum(Abundance)) %>% # sum abundances from the same sample
    ungroup() %>%
    mutate(renamed_taxa = factor(renamed_taxa, levels = all_taxa)) %>% # reorder so Other is on top
    rename(!!level := 'renamed_taxa') %>% # rename the fake column name back to the original
    ungroup() # a final ungroup, just in case

  return(p_melted)

}
