#' Restore cohort file
#'
#' Loads an archived cohort file and, if confirmed, replaces the current file.
#'
#' `restore_cohort` will load the most recent archive by default, which is the
#' last file (alphabetically) in Cohort/Archive. You can supply the filename
#' to `file` if you want to restore an older version.
#'
#' @param file The name of a specific archive if different than the most recent file.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom readr write_csv
#' @importFrom rlang inform
#' @export

restore_cohort <- function(file = NA){
  # If unspecified
  if(is.na(file)){
    # Get name
    file <- sort(dir(path = "Cohort/Archive"), decreasing = T)[1]
  }

  # If specified or once determined
  if(grepl(".rds", file, ignore.case = TRUE)){
    out <- readRDS(paste0("Cohort/Archive/", file))
  } else {
    out <- readRDS(paste0("Cohort/Archive/", file, ".rds"))
  }

  # View and pause
  View(out, "Archive")
  save <- readline("Enter 'save' to restore this archive. Otherwise, press enter. ")

  # Save
  if(save == "save" | save == "'save'"){
    readr::write_csv(out, paste0("Cohort/", cluster_cfg$short_name))
    rlang::inform(message = c(
      cli::style_bold("Cohort file restored."),
      "v" = paste0("Overwritten with ", cli::style_underline(file), ".")
    ))
  } else {
    rlang::inform(message = c(
      "Restore cancelled."
    ))
  }
}
