#' Extract DADA2 seqtab statistics
#'
#' @param seqtab DADA2 seqtab object
#'
#' @export

seqtab_stats = function(seqtab) {

  sizes_freq = as.data.frame(table(nchar(colnames(seqtab)))) %>%
    rename("LENGTH" = "Var1", "FREQ" = "Freq") %>%
    mutate("LENGTH" = as.integer(as.character(LENGTH)))

  sizes_abund = as.data.frame(tapply(colSums(seqtab), nchar(colnames(seqtab)), sum))
  sizes_abund$LENGTH = as.integer(rownames(sizes_abund))
  colnames(sizes_abund) = c("ABUNDANCE", "LENGTH")

  seqtab_stats = left_join(sizes_freq, sizes_abund, by = "LENGTH")

  return(seqtab_stats)
}
