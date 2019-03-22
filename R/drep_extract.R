#' Extract dRep genome results
#'
#' @param path Path to the top level dRep directory
#'
#' @export

drep_extract = function(path) {
  require("tidyverse")

  genomeInfo = read_csv(file = paste0(path,"/data_tables/genomeInformation.csv"))

  Sdb = read_csv(paste0(path,"/data_tables/Sdb.csv"))
  genomeInfo = left_join(genomeInfo, Sdb, by = "genome")

  Cdb = read_csv(paste0(path,"/data_tables/Cdb.csv")) %>%
    select(genome, secondary_cluster)
  genomeInfo = left_join(genomeInfo, Cdb, by = "genome")

  Wdb = read_csv(paste0(path,"/data_tables/Wdb.csv")) %>%
    select(genome, cluster)
  genomeInfo = left_join(genomeInfo, Wdb, by = "genome")

  genomeInfo$genome = gsub(".fa", "", genomeInfo$genome)

  genomeInfo = suppressWarnings(genomeInfo %>%
    separate(cluster, into = c("cluster"), sep = "_") %>%
    separate(secondary_cluster, into = c("secondary_cluster"), sep = "_"))

  return(genomeInfo)
}
