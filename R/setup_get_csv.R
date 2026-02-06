#' setup_get_csv: Set up connection to comma-delimited file for survey data
#'
#' Generates the setup_get item in cluster_cfg for a connection to a .csv file.
#'
#' @param file A string representing a permanent file path to your input .csv file.
#' @param ... Arguments passed to readr::read_csv.
#' @importFrom cli style_bold
#' @importFrom readr read_csv
#' @importFrom rlang abort
#' @export

setup_get_csv <- function(file,
                          ...
){
  # Create list
  setup_get <- list(...)

  # Name function
  setup_get$get <- "clusteR::get_csv"

  # Verify path exists and set
  tryCatch(
    {suppressMessages(readr::read_csv(path))},
    error = function(cond){
      rlang::abort(message = c(
        cli::style_bold("Data source import failed."),
        paste0(cond)
      ))
    }
  )

  # Return
  return(setup_get)
}
