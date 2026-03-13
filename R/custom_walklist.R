#' Create custom template for door-to-door walk lists
#'
#' Creates an R markdown template in the Scripts folder for you to customize
#' and use with `make_walklist` rather than its default, simple template.
#'
#' Your custom template can use additional objects if you pass them as named
#' arguments to `make_walklist`. For example, if you would like to reference
#' a customizable string representing a time in your template, you can pass it
#' like `make_walklist(template = "Scripts/walklist.Rmd", time = "12:00 AM")`
#' and refer to it in your template as `time`.
#'
#' If you would like to include additional columns in the walklist tables, you
#' will need to specify them in `make_walklist`, not in a custom template.
#'
#' @importFrom cli style_bold
#' @importFrom rlang inform
#' @export

custom_walklist <- function(){
  # Copy file
  file.copy(
    from = system.file("custom", "walklist.Rmd", package = "clusteR"),
    to = "Scripts/walklist.Rmd",
    overwrite = F
  )

  # Message to view file
  rlang::inform(message = c(
    cli::style_bold("Template created. Next steps:"),
    "*" = paste0("Open ", cli::style_underline("Scripts/walklist.Rmd"), "."),
    "*" = "Modify as needed.",
    "*" = "Run make_walklist(template = 'Scripts/walklist.Rmd', ...)"
  ))
}
