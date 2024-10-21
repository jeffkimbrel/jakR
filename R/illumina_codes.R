#' Get Illumina machine and flow cell code from run id
#'
#' Machine codes and regular expressions scraped from users at
#' biostars (https://www.biostars.org/p/198143/).
#'
#' @param run_id character string with the run id
#'
#' @export

illumina_codes <- function(run_id) {

  # run_id must be a string
  if (!is.character(run_id)) {
    stop("run_id must be a string", call. = FALSE)
  }



  i <- stringr::str_split(run_id, "\\.")[[1]]

  machine <- i[1] |>
    tibble::enframe(value = "machine") |>
    dplyr::mutate(
      sn_infer = dplyr::case_when(
        stringr::str_detect(machine, "^HWI-M|^M") ~ "MiSeq",
        stringr::str_detect(machine, "^HWUSI") ~ "GAIIx",
        stringr::str_detect(machine, "^HWI-D") ~ "HiSeq 2x00",
        stringr::str_detect(machine, "^K") ~ "HiSeq 3/4000",
        stringr::str_detect(machine, "^N") ~ "NextSeq 5x0",
        stringr::str_detect(machine, "^A|^H") ~ "NovaSeq",
        stringr::str_detect(machine, "^V|^AA") ~ "NextSeq 2000",
        .default = "Unknown Illumina Machine"
      )
    ) |>
    dplyr::pull(sn_infer)


  flowcell <- i[3] |>
    tibble::enframe(value = "flowcell") |>
    dplyr::mutate(fc_infer = dplyr::case_when(
      stringr::str_detect(flowcell, "^(BRB|BP[ACGL]|BNT)") ~ "iSeq_100",
      stringr::str_detect(flowcell, "000H") ~ "MiniSeq",
      stringr::str_detect(flowcell, "^[BCJKDG]") ~ "MiSeq",
      stringr::str_detect(flowcell, "(A[FG]|BG)..$") ~ "NextSeq_500/550 ",
      stringr::str_detect(flowcell, "M5$") ~ "NextSeq_1000/2000",
      stringr::str_detect(flowcell, "HV$") ~ "NextSeq_2000",
      stringr::str_detect(flowcell, "([AB]C|AN)..$") ~ "HiSeq_2500",
      stringr::str_detect(flowcell, "BB..$") ~ "HiSeq_3000/4000",
      stringr::str_detect(flowcell, "(AL|CC)..$") ~ "HiSeq_X",
      stringr::str_detect(flowcell, "D[RS]..$|DM.$") ~ "NovaSeq_6000",
      .default = "Unknown Flowcell Type"
    ))|>
    dplyr::pull(fc_infer)


  return(list(
    "machine_code" = i[1],
    "machine" = machine,
    "flowcell_code" = i[3],
    "flowcell" = flowcell
  ))
}
