#' Set up connection to a comma-delimited survey data file
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
    {suppressMessages(readr::read_csv(file))},
    error = function(cond){
      rlang::abort(message = c(
        cli::style_bold("Data source import failed."),
        paste0(cond)
      ))
    },
    finally = {setup_get$file <- file}
  )

  # Return
  return(setup_get)
}
