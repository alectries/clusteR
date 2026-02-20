#' @importFrom rlang warn
#' @importFrom rlang inform
#' @importFrom cli style_bold
#' @importFrom cli style_underline

.onLoad <- function(libname, pkgname){
  # Check for settings file
  tryCatch(
    {
      .cluster <<- new.env(parent = emptyenv())
      .cluster$cfg <<- suppressWarnings(readRDS("Scripts/config.rds"))
      rlang::inform(message = c(
        cli::style_bold("clusteR started successfully!"),
        "v" = paste0(cli::style_underline(.cluster$cfg$name), " config loaded.")
      ))
    },
    error = function(cond){
      rlang::warn(message = c(
        cli::style_bold("clusteR environment not configured!"),
        "!" = paste0("Please run ", cli::style_underline("clusteR::setup()"),
                     " to set up your survey environment.")
      ))
    }
  )
}
