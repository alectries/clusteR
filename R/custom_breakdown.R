#' Create custom template for breakdown reports
#'
#' Creates an R markdown template in the Scripts folder for you to customize
#' and use with `view_breakdown` rather than its default, simple template.
#'
#' @importFrom cli style_bold
#' @importFrom rlang inform
#' @export

custom_topline <- function(){
  # Copy file
  file.copy(
    from = system.file("custom", "breakdown.Rmd", package = "clusteR"),
    to = "Scripts/breakdown.Rmd",
    overwrite = F
  )

  # Message to view file
  rlang::inform(message = c(
    cli::style_bold("Template created. Next steps:"),
    "*" = paste0("Open ", cli::style_underline("Scripts/breakdown.Rmd"), "."),
    "*" = "Modify as needed.",
    "*" = "Run view_breakdown(template = 'Scripts/breakdown.Rmd', ...)"
  ))
}
