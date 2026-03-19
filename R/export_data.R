#' Export survey data to CSV
#'
#' Generates a CSV file from downloaded survey data. By default, uses the most
#' recent survey data.
#'
#' @param ... Arguments passed to `dplyr::select`. If empty, all columns are kept (except any removed by `.deid`).
#' @param .name Name of the exported file. Defaults to `name` from `setup`.
#' @param .dat A string; file path to the data archive to use. Defaults to most recent cleaned data if available, or most recent downloaded data otherwise.
#' @param .deid If TRUE, removes columns in the Cohort file other than ID. Defaults to FALSE.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr select
#' @importFrom magrittr `%>%`
#' @importFrom readr write_csv
#' @importFrom rlang abort
#' @importFrom rlang enquos
#' @importFrom rlang inform
#' @importFrom rlang is_empty
#' @importFrom tidyselect any_of
#' @export

export_data <- function(...,
                        .name = .cluster$cfg$name,
                        .dat = NA,
                        .deid = FALSE
){
  # Definitions
  `%>%` <- magrittr::`%>%`
  dots <- rlang::enquos(...)

  # Get data
  if(is.na(.dat)){
    # Try cleaned data
    if(exists("last_clean", .cluster$cfg)){
      data <- readRDS(.cluster$cfg$last_clean)
    } else {

      # Try raw data
      if(exists("last_data", .cluster$cfg)){
        data <- readRDS(.cluster$cfg$last_data)
      } else {

        # No saved data, error
        rlang::abort(message = c(
          cli::style_bold("No recent data files found!"),
          "x" = "Ensure setup_get is specified and you have run get_data()."
        ))
      }
    }
  } else {
    data <- readRDS(.dat)
  }

  # Deidentify
  if(.deid){
    data <- data %>%
      dplyr::select(-tidyselect::any_of(c(names(dplyr::select(clusteR::view_cohort(F), -ID)), "Address")))
  }

  # Select
  if(rlang::is_empty(dots)){
    data <- data
  } else {
    data <- data %>%
      dplyr::select(...)
  }

  # Write
  readr::write_csv(data, paste0("Survey Data/", .name, ".csv"), na = "")
  rlang::inform(message = c(
    cli::style_bold("Data file exported!"),
    "i" = paste0("See ", cli::style_underline(paste0("Survey Data/", .name, ".csv")))
  ))
}
