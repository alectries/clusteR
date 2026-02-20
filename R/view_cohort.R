#' View cohort file
#'
#' Quickly opens the cohort file for viewing. Invisibly returns the cohort file
#' so you can attach it for manual tasks.
#'
#' @importFrom readr read_csv
#' @export

view_cohort <- function(){
  # Load cohort
  cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name), show_col_types = F)

  # View cohort
  View(cohort, "Cohort")

  # Return
  return(invisible(cohort))
}
