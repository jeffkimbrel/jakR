#' Extract dRep genome results
#'
#' @param path Path to the top level dRep directory
#'
#' @export

drep_extract <- function(path) {

  # genomeInformation.csv
  g_path <- file.path(path, "data_tables/genomeInformation.csv")
  if (file.exists(g_path)) {
    message(glue::glue_col("{green Found {g_path}}"))
    genomeInfo <- readr::read_csv(
      file = g_path,
      show_col_types = F
    )
  } else {
    stop(glue::glue_col("{red genomeInformation.csv not found}"), call. = FALSE)
  }

  # Sdb.csv
  s_path <- file.path(path, "data_tables/Sdb.csv")

  if (file.exists(s_path)) {
    message(glue::glue_col("{green Found {s_path}}"))
    Sdb <- readr::read_csv(
      s_path,
      show_col_types = F
    )
    genomeInfo <- dplyr::left_join(genomeInfo, Sdb, by = "genome")
  } else {
    stop(glue::glue_col("{red Sdb.csv not found}"), call. = FALSE)
  }


  # Cdb.csv
  c_path <- file.path(path, "data_tables/Cdb.csv")

  if (file.exists(c_path)) {
    message(glue::glue_col("{green Found {c_path}}"))

    Cdb <- readr::read_csv(
      c_path,
      show_col_types = F
    ) |>
      dplyr::select(genome, secondary_cluster)

    genomeInfo <- dplyr::left_join(genomeInfo, Cdb, by = "genome")
  } else {
    stop(glue::glue_col("{red Cdb.csv not found}"), call. = FALSE)
  }


  # Wdb.csv
  w_path <- file.path(path, "data_tables/Wdb.csv")

  if (file.exists(w_path)) {
    message(glue::glue_col("{green Found {w_path}}"))

    Wdb <- readr::read_csv(
      w_path,
      show_col_types = F
    ) |>
      dplyr::select(genome, cluster)

    genomeInfo <- dplyr::left_join(genomeInfo, Wdb, by = "genome")
  } else {
    stop(glue::glue_col("{red Wdb.csv not found}"), call. = FALSE)
  }

  genomeInfo$genome <- gsub(".fa", "", genomeInfo$genome)

  # genomeInfo = suppressWarnings(genomeInfo %>%
  #  separate(cluster, into = c("cluster"), sep = "_") %>%
  #  separate(secondary_cluster, into = c("secondary_cluster"), sep = "_"))

  return(genomeInfo)
}
