#' Update cohort file
#'
#' Gathers data from manual and automatic sources, generates a report with
#' data conflicts between the current cohort file and the updated data, and
#' updates the cohort file.
#'
#' `update` is the first of two functions that must be run in series; after
#' running and verifying the outputs of `update`, run `update_confirm`.
#' `update` does not actually *perform* the cohort file update, but instead
#' prepares the data to be updated later while providing a preview of the
#' changes for your review. `update` will set three global values: `df_cohort`,
#' `df_manual`, and `df_source`. These will then be accessed by `update_confirm`.
#'
#' Importantly, if the `update` report alerts you to issues in your data
#' that you subsequently fix, you must run `update` again before running
#' `update_confirm` for your changes to take effect.
#'
#' `update` runs `get_data` under the hood, meaning that messages will be
#' displayed in the console as you run it.
#'
#' You should not need to provide any arguments to `update`. However, if your
#' get function requires arguments, provide them to `update` as a named list, as
#' you would to `get_data`.
#'
#' @param args If arguments are needed for your `get` function, they can be passed as a list.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr coalesce
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom knitr kable
#' @importFrom lubridate now
#' @importFrom magrittr `%>%`
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom rlang `:=`
#' @importFrom rlang inform
#' @importFrom rstudioapi isAvailable
#' @importFrom rstudioapi viewer
#' @importFrom tidyselect all_of
#' @importFrom tidyselect any_of
#' @export

update_confirm <- function(){
  # Define
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  # Initial values
  out <- df_cohort

  # Merge in source data
  if(!is.null(df_source) && nrow(df_source) > 0){
    out <- out %>%
      left_join(
        df_source,
        by = "ID",
        suffix = c("", ".src")
      )
    src_cols <- grep("\\.src$", names(out), value = T)
    for(col in src_cols){
      orig <- sub("\\.src$", "", col)
      out <- out %>%
        dplyr::mutate(!!orig := dplyr::coalesce(!!dplyr::sym(col), !!dplyr::sym(orig)))
    }
    out <- out %>%
      dplyr::select(-tidyselect::all_of(src_cols))
  }

  # Merge in manual data
  if(!is.null(df_manual) && nrow(df_manual) > 0){
    out <- out %>%
      dplyr::left_join(df_manual, by = "ID", suffix = c("", ".man"))
    man_cols <- grep("\\.man$", names(out), value = T)
    for(col in man_cols){
      orig <- sub("\\.man$", "", col)
      out <- out %>%
        dplyr::mutate(!!orig := dplyr::coalesce(!!dplyr::sym(col), !!dplyr::sym(orig)))
    }
    out <- out %>%
      dplyr::select(-tidyselect::all_of(man_cols))
  }

  # Sort
  out <- dplyr::arrange(out, ID)

  # Save
  readr::write_csv(out, paste0("Cohort/", cluster_cfg$short_name))
  rlang::inform(message = c(
    cli::style_bold("Cohort file updated.")
  ))
}
