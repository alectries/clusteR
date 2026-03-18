#' Generate mailing list from cohort file
#'
#' Produces a mailing list using the mailing address, city, state, and ZIP code
#' specified in the cohort file. Can output a formatted report as a PDF or a
#' CSV for use outside R.
#'
#' `.status` accepts three options:
#'
#' - `"completed"` includes only residents that have completed the survey
#' - `"pending"` includes only residents that have *not* completed the survey
#' - `"enrolled"` includes residents that have enrolled in the cohort, whether
#' or not they have completed the survey this cycle
#'
#' @param output Output format, either "pdf" or "csv".
#' @param .status If not NA, filters for a status category; see details.
#' @param ... Arguments passed to `dplyr::filter` to limit the cohort file.
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom knitr kable
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom rlang `!!!`
#' @importFrom rlang enquos
#' @importFrom rlang list2
#' @importFrom rmarkdown render
#' @importFrom stringr str_detect
#' @export

make_mailing <- function(output, ..., .status = NA){
  # Capture dots
  `!!!` <- rlang::`!!!`
  dots <- rlang::enquos(...)

  # Read cohort file
  cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name),
                            show_col_types = F)

  # Filter cohort file
  if(length(list2(!!!dots)) > 0){
    cohort <- dplyr::filter(cohort, !!!dots)
  }

  # Filter for status, if given
  if(!is.na(.status)){
    ## Completed
    if(.status == "completed"){
      cohort <- dplyr::filter(cohort, stringr::str_detect(Status, "Completed"))
    }
    ## Pending
    if(.status == "pending"){
      cohort <- dplyr::filter(cohort, Status %in% c("Enrolled", "Re-enroll", "Not enrolled"))
    }
    ## Enrolled
    if(.status == "enrolled"){
      cohort <- dplyr::filter(cohort, Status %in% c("Enrolled", "Completed - enrolled"))
    }
  }

  # Select columns
  out <- dplyr::select(cohort, ID, Name, Mailing, City, State, ZIP)

  # Output CSV
  if(output == "csv"){
    readr::write_csv(out, "Contacts/Mailing.csv")
    rlang::inform(message = c(
      cli::style_bold("Mailing list complete."),
      "v" = paste0("Saved as CSV to ", cli::style_underline("Contacts/Mailing.csv"), ".")
    ))
  }

  # Output PDF
  if(output == "pdf"){
    knit_env <- list(out = out)
    rmarkdown::render(
      input = system.file("auto", "make_table.Rmd", package = "clusteR"),
      output_dir = paste0(getwd(), "/Contacts"),
      output_file = "Mailing.pdf",
      envir = knit_env,
      quiet = T
    )
    rlang::inform(message = c(
      cli::style_bold("Mailing list complete."),
      "v" = paste0("Saved as PDF to ", cli::style_underline("Contacts/Mailing.pdf"), ".")
    ))
  }
}
