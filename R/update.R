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

update <- function(args = list()){
  # Define
  `%>%` <- magrittr::`%>%`

  # Get fresh data
  path <- clusteR::get_data(args)
  data <- readRDS(path)

  # Get current cohort file
  df_cohort <<- readr::read_csv(
    paste0("Cohort/", cluster_cfg$short_name), show_col_types = F
  )

  # Archive current cohort file
  time <- gsub('[:. ]', '-', lubridate::now())
  saveRDS(df_cohort, paste0("Cohort/Archive/", cluster_cfg$short_name, time, ".rds"))
  inform(message = c(
    paste0(
      cli::style_bold("Archive saved to: "),
      cli::style_underline(paste0("Cohort/Archive/",
                                  cluster_cfg$short_name, time, ".rds"))
    ),
    "i" = "Restore using restore()."
  ))

  # Pull new data
  ## From editor
  df_manual <<- readr::read_csv("Cohort/Editor.csv", show_col_types = F) %>%
    dplyr::mutate(
      "Mailing" = toupper(Mailing),
      "City" = toupper(City),
      "ZIP" = as.character(ZIP),
      "Status" = dplyr::case_when(
        Consent == "Do not contact" ~ "DO NOT CONTACT",
        Consent == "Yes" ~ "Completed - enrolled",
        Consent == "No" ~ "Completed - unenroll",
        Consent == "Unknown" ~ "Completed - re-enroll",
        .default = NA
      ),
      "Source" = "MANUAL"
    )

  ## From source
  df_source <<- data %>%
    dplyr::select(tidyselect::any_of(names(df_cohort))) %>%
    dplyr::mutate(
      "Status" = dplyr::case_when(
        Consent == "Do not contact" ~ "DO NOT CONTACT",
        Consent == "Yes" ~ "Completed - enrolled",
        Consent == "No" ~ "Completed - unenroll",
        Consent == "Unknown" ~ "Completed - re-enroll",
        .default = NA
      ),
      "Source" = cluster_cfg$setup_get$get
    )

  # Join and verify manual data
  coh_man_errs <- list()
  i <- 0
  for(col in setdiff(names(df_manual), c("ID", "Source"))){
    i <- i + 1
    coh_man_errs[[i]] <- dplyr::full_join(
      df_cohort,
      df_manual,
      by = "ID",
      suffix = c(".coh", ".man")
    ) %>%
      dplyr::filter(.data[[paste0(col, ".coh")]] != .data[[paste0(col, ".man")]]) %>%
      dplyr::select("ID", any_of(c(
        paste0(col, ".coh"),
        paste0(col, ".man")
      )))
  }
  names(coh_man_errs) <- setdiff(names(df_manual), c("ID", "Source"))

  # Join and verify source data
  coh_src_errs <- list()
  i <- 0
  for(col in setdiff(names(df_source), c("ID", "Source"))){
    i <- i + 1
    coh_src_errs[[i]] <- dplyr::full_join(
      df_cohort,
      df_source,
      by = "ID",
      suffix = c(".coh", ".src")
    ) %>%
      dplyr::filter(.data[[paste0(col, ".coh")]] != .data[[paste0(col, ".src")]]) %>%
      dplyr::select("ID", any_of(c(
        paste0(col, ".coh"),
        paste0(col, ".src")
      )))
  }
  names(coh_src_errs) <- setdiff(names(df_source), c("ID", "Source"))

  # Knit
  knit_env <- list(
    cluster_cfg = cluster_cfg,
    coh_man_errs = coh_man_errs,
    coh_src_errs = coh_src_errs
  )
  rmarkdown::render(
    input = system.file("auto", "update.Rmd", package = "clusteR"),
    output_dir = getwd(),
    output_file = "Update Errors.html",
    envir = knit_env
  )
  if(rstudioapi::isAvailable()){
    rstudioapi::viewer("Update Errors.html")
  } else {
    rlang::inform(
      message = c(
        cli::style_bold("Update Errors report generated."),
        "i" = paste0("See ", cli::style_underline("Update Errors.html"), ".")
      )
    )
  }

  # Messages
  rlang::inform(message = c(
    cli::style_bold("Checks complete. Next:"),
    "*" = paste0("Make any necessary changes in ", cli::style_underline("Cohort/Editor.csv"), "."),
    "*" = paste0("If changes are made, re-run ", cli::style_underline("update()"), "."),
    "*" = paste0("Run ", cli::style_underline("update_confirm()"), ".")
  ))
}
