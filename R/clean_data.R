#' Clean and save survey data
#'
#' Applies mutates and, if a method is specified, calculates weights for survey
#' data, then saves the data.
#'
#' Your function call will likely be quite long, so you should probably save it
#' in an R script to simplify making minor tweaks. It will quickly become
#' unwieldy in a console.
#'
#' If a path to a data file is not specified, the most recent data file will
#' be used. You can specify another filepath as a string, passed to `.x`.
#'
#' Inside your mutates, you may refer to the dataset as `.`.
#'
#' You can specify a function call, such as `clean_weight`, as `.wt` to weight
#' data. The function you call should return a function with one argument (the
#' dataframe of survey data); the created weighting function must return a
#' dataframe that contains the ID column and any weight columns to add to the
#' cleaned data.
#'
#' Other arguments (`...`) are passed to `dplyr::mutate` for cleaning tasks.
#'
#' @param ... Mutates to clean data.
#' @param .x A string; the file path to the data to be used. Defaults to most recent data file.
#' @param .wt A function call, such as `clean_weight`, to weight data if desired.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr left_join
#' @importFrom magrittr `%>%`
#' @importFrom rlang inform
#' @importFrom stringr str_remove
#' @export

clean_data <- function(...,
                       .x = .cluster$cfg$last_data,
                       .wt = NULL
){
  # Definitions
  `%>%` <- magrittr::`%>%`
  # Get data
  ..x <- readRDS(.x)

  # Weighting
  if(!is.null(.wt)){
    ..wt <- .wt(..x)
    ..x <- dplyr::left_join(..x, ..wt, by = "ID")
  }

  # Mutates
  ..out <- ..x %>%
    dplyr::mutate(...)

  # Save and return
  path <- paste0(stringr::str_remove(.x, ".rds"), "_clean.rds")
  saveRDS(..out, path)
  .cluster$cfg$last_clean <<- path
  saveRDS(.cluster$cfg, "Scripts/config.rds")
  rlang::inform(message = c(
    cli::style_bold("Data cleaned."),
    "i" = paste0("Saved to ", cli::style_underline(path), ".")
  ))
  return(invisible(..out))
}
