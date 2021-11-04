#' Format DT Datatable
#'
#' @param row_count Number of rows in the default view
#' @param rownames Include rownames
#'
#' @export


format_dt <- function(x, row_count = 10, rownames = FALSE) {

  require(DT)

  DT::datatable(x,
                style = "bootstrap4",
                class = "compact",
                filter = "top",
                rownames = rownames,
                extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel'),
                               lengthMenu = list(c(row_count, 50, -1),
                                                 c(row_count, 50, "All")))) %>%
    DT::formatStyle(0,
                    target= 'row',
                    fontSize = '13px')
}
