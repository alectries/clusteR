#' Export the cohort file to CSV
#'
#' Generates a CSV file with specified modifications based on the current
#' cohort file.
#'
#' `export_cohort` does not modify the cohort file itself, only the exported
#' output. To remove a column from the export using `dplyr::mutate` syntax,
#' set the column to NULL (e.g. `export_cohort(Name = NULL)`).
#'
#' Enabling `.status` will prepare the cohort export for a new clusteR
#' configuration. Previously enrolled participants will remain enrolled,
#' participants with Unenroll status will be removed, and all others will be
#' based on their consent status. Participants with `NA` consent status are
#' considered not enrolled and will be included in the export. If you need to
#' exclude participants from the export that are included by default behavior,
#' set their status to Unenroll using appropriate mutates.
#'
#' Enabling `.removed` will create a separate file (`Cohort/removed.csv`) with
#' cohort members that are filtered out by `.status` when enabled. This is
#' primarily useful to avoid re-sampling previously sampled households.
#'
#' @param ... Arguments passed to `dplyr::mutate`.
#' @param .name Name of the exported file. Defaults to `name` from `setup`.
#' @param .status If TRUE, update status to Enrolled, Re-enroll, or Not enrolled for a new cycle and delete unenrolled/do not contact participants. If FALSE (default), keep current status.
#' @param .removed If TRUE, write removed participants to `removed.csv`. Defaults to FALSE.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr `%>%`
#' @importFrom readr write_csv
#' @importFrom rlang inform
#' @export

export_cohort <- function(...,
                          .name = .cluster$cfg$name,
                          .status = FALSE,
                          .removed = FALSE
){
  # Definitions
  `%>%` <- magrittr::`%>%`

  # Get cohort file
  coh <- clusteR::view_cohort(F)

  # Perform mutates
  out <- coh %>%
    dplyr::mutate(...)

  # Update status
  if(.status){
    out <- out %>%
      dplyr::mutate(Status = dplyr::case_when(
        Status == "Unenroll" ~ "Unenroll",
        Status == "Enrolled" ~ "Enrolled",
        Consent == "Yes" ~ "Enrolled",
        Consent == "No" ~ "Unenroll",
        Consent == "Unknown" ~ "Re-enroll",
        Consent == "Do not contact" ~ "Unenroll",
        is.na(Consent) ~ "Not enrolled",
        .default = NA
      )) %>%
      dplyr::filter(Status %in% c("Enrolled", "Re-enroll", "Not enrolled"))
  }

  # Write file
  readr::write_csv(out, paste0("Cohort/", .name, ".csv"))
  rlang::inform(message = c(
    cli::style_bold("Cohort file exported!"),
    "i" = paste0("See ", cli::style_underline(paste0("Cohort/", .name, ".csv")))
  ))

  # Write removed
  if(.removed){
    rmv <- coh %>%
      dplyr::mutate(...) %>%
      dplyr::mutate(Status = dplyr::case_when(
        Status == "Enrolled" ~ "Enrolled",
        Consent == "Yes" ~ "Enrolled",
        Consent == "No" ~ "Unenroll",
        Consent == "Unknown" ~ "Re-enroll",
        Consent == "Do not contact" ~ "Unenroll",
        is.na(Consent) ~ "Not enrolled",
        .default = NA
      )) %>%
      dplyr::filter(!(Status %in% c("Enrolled", "Re-enroll", "Not enrolled")))
    readr::write_csv(rmv, "Cohort/removed.csv", na = "")
    rlang::inform(message = c(
      "i" = paste0("Removed IDs written to ", cli::style_underline("Cohort/removed.csv"), ".")
    ))
  }
}
