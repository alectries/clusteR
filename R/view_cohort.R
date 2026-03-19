#' View cohort file
#'
#' Quickly opens the cohort file for viewing. Invisibly returns the cohort file
#' so you can attach it for manual tasks.
#'
#' @param show If FALSE, returns cohort file without opening Viewer. Defaults to TRUE.
#' @importFrom readr read_csv
#' @export

view_cohort <- function(show = TRUE
){
  # Load cohort
  cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name), show_col_types = F)

  # View cohort
  if(show){
    View(cohort, "Cohort")
  }

  # Return
  return(invisible(cohort))
}
