#' Create custom template for topline reports
#'
#' Creates an R markdown template in the Scripts folder for you to customize
#' and use with `view_topline` rather than its default, simple template.
#'
#' @importFrom cli style_bold
#' @importFrom rlang inform
#' @export

custom_topline <- function(){
  # Copy file
  file.copy(
    from = system.file("custom", "topline.Rmd", package = "clusteR"),
    to = "Scripts/topline.Rmd",
    overwrite = F
  )

  # Message to view file
  rlang::inform(message = c(
    cli::style_bold("Template created. Next steps:"),
    "*" = paste0("Open ", cli::style_underline("Scripts/topline.Rmd"), "."),
    "*" = "Modify as needed.",
    "*" = "Run view_topline(template = 'Scripts/topline.Rmd', ...)"
  ))
}
