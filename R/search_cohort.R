#' Search cohort file
#'
#' Quickly search the cohort file and view results. Wrapper for `dplyr::filter`.
#` Invisibly returns the search results so you can attach them for manual tasks.
#'
#' @importFrom dplyr filter
#' @importFrom readr read_csv
#' @export

search_cohort <- function(...){
  # Load cohort
  cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name), show_col_types = F)

  # Filter
  filtered <- dplyr::filter(cohort, ...)

  # View cohort
  View(filtered, "Search")

  # Return
  return(invisible(filtered))
}
