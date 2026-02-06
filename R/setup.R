#' setup: Create folder structure and generate config file
#'
#' Generates a folder structure and config.rds, which holds persistent
#' information about the survey environment, in the current working directory.
#'
#' Setup creates the necessary elements of the survey environment (literally,
#' the structure and contents of the working directory) for a clusteR managed
#' survey. You may wish to write your setup function call in an R script and
#' save it in a subfolder called Scripts (which will be created if you don't
#' create it yourself) so you can document it and modify it later if needed.
#'
#' While only name and short_name are required for setup to run successfully,
#' many of the other parameters are required in practice. You will receive
#' warnings for parameters you did not specify that are soft-required.
#'
#' `setup_get`: setup should see a list that contains information necessary
#' for a corresponding `get` function to retrieve data from a connection.
#' As a user, you should call a `setup_get` function (like [`setup_get_csv`]),
#' which will create this list and pass it to setup for you. For a highly
#' customized setup, you may wish to `source` a script to generate this list
#' instead.
#'
#' `setup_cohort`: setup should see a string denoting a path where a
#' properly-formatted cohort input file exists. See `vignette('setup_cohort')`
#' for details, but in short, this cohort input file should be tidy and have the
#' fields ID, Cluster, Name, Mailing, Physical, City, State, ZIP, Phone, Email,
#' Consent, and Status, as well as any other fields you would like to persist in
#' your cohort database. You can call a function here to generate the input file
#' and return the path as a string. The input file must be a .csv, .xls, .xlsx,
#' or .rds file with the proper file extension.
#'
#' `state`, `county`: This package is designed for a local epidemiologist to
#' perform a cluster sampling survey; therefore, the state and county (or
#' county-equivalent) should be specified. setup will obtain the 2020 TIGER/Line
#' shapefiles for your county for you, but you must specify the
#' [state](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code)
#' and [county](https://en.wikipedia.org/wiki/List_of_United_States_INCITS_codes_by_county)
#' FIPS/INCITS codes for your area. These are technically not required if you
#' choose to avoid all mapping functions, but that may cause problems.
#'
#' @param name A string, the full name of your project (used for formatted outputs)
#' @param short_name A string, the short name of your project (used for file names and other short outputs)
#' @param setup_get The output of a function, such as setup_get_csv or setup_get_alc, to set up your data source connection (see `vignette('setup_get')`)
#' @param setup_cohort The name of a properly-formatted cohort input file or a function to generate that file (see `vignette('setup_cohort')`)
#' @param state The FIPS code for the state of interest
#' @param county The FIPS code for the county of interest
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom readxl read_excel
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom stringr str_detect
#' @export

setup <- function(name,
                  short_name,
                  ...
){
  # Print working directory
  rlang::inform(message = c(
    cli::style_bold("clusteR setup:"),
    "v" = paste0("Creating project in ", cli::style_underline(getwd()))
  ))

  # Generate folder structure
  dir.create("Cohort")
  dir.create("Cohort/Archive", recursive = T)
  dir.create("Cohort/Shapefiles", recursive = T)
  dir.create("Contacts")
  dir.create("Scripts")
  dir.create("Survey Data")

  # Check for .Rproj file
  if(length(list.files(pattern = "\\.Rproj$")) == 0){
    rlang::warn(message = c(
      cli::style_bold("No .Rproj file found!"),
      "!" = "If you are using RStudio, you should create an R project in this directory."
    ))
  }

  # Accept arguments as list
  input <- list(name = name, short_name = short_name, ...)

  # Validation
  if(!is.character(input$name) | length(input$name) != 1){
    rlang::abort(message = c(
      cli::style_bold("Error in name"),
      "x" = paste0(name, " is not a string.")
    ))
  }
  if(!is.character(input$short_name) | length(input$short_name) != 1 |
     stringr::str_detect(input$short_name, "\\s")){
    rlang::abort(message = c(
      cli::style_bold("Error in short_name"),
      "x" = paste0(short_name, " is not a string without spaces.")
    ))
  }
  if(!exists("input$setup_get")){
    rlang::warn(message = c(
      cli::style_bold("No data connection specified!"),
      "!" = "Data cannot be collected without a data connection.",
      "*" = paste0("Try ", cli::style_underline("setup_get_csv()"), "."),
      "*" = paste0("You can also use a custom function from a script or another package."),
      "i" = paste0("See ", cli::style_underline("vignette('setup_get')"), " for more information.")
    ))
  }
  if(!exists("input$state") | !exists("input$county")){
    rlang::warn(message = c(
      cli::style_bold("No state or county specified!"),
      "!" = "Mapping functions will fail without shapefiles.",
      "i" = "To solve this issue, run setup with state and county specified."
    ))
  }

  # Get shapefiles
  if(exists("input$state") & exists("input$county")){
    ## Get, unzip, and save name of county shapefile
    download.file(
      url = paste0("https://www2.census.gov/geo/tiger/TIGER2020/County/tl_2020_us_county.zip"),
      destfile = "Cohort/Shapefiles/tl_2020_us_county.zip"
    )
    unzip("Cohort/Shapefiles/tl_2020_us_county.zip", exdir = "Cohort/Shapefiles/")
    input$shape_county <- "Cohort/Shapefiles/tl_2020_us_county.zip"

    ## Get, unzip, and save name of block shapefile
    download.file(
      url = paste0("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_",
                   input$state,
                   "_tabblock20.zip"),
      destfile = paste0("Cohort/Shapefiles/tl_2020_", input$state, "_tabblock20.zip")
    )
    unzip(
      paste0("Cohort/Shapefiles/tl_2020_", input$state, "_tabblock20.zip"),
      exdir = "Cohort/Shapefiles/"
    )
    input$shape_block <- paste0("Cohort/Shapefiles/tl_2020_", input$state, "_tabblock20.zip")
  }

  # Set up cohort file
  if(exists("input$setup_cohort")){
    if(is.character(input$setup_cohort) && length(input$setup_cohort) == 1){
      ## Read file
      if(stringr::str_detect(input$setup_cohort, regex("\\.csv", ignore_case = T))){cohort <- readr::read_csv(input$setup_cohort, show_col_types = F)}
      if(stringr::str_detect(input$setup_cohort, regex("\\.xls|\\.xlsx", ignore_case = T))){cohort <- readxl::read_excel(input$setup_cohort)}
      if(stringr::str_detect(input$setup_cohort, regex("\\.rds", ignore_case = T))){cohort <- readRDS(input$setup_cohort)}

      ## Fail if format not supported
      if(stringr::str_detect(input$setup_cohort, regex("\\.csv|\\.xls|\\.xlsx|\\.rds", ignore_case = T), negate = T)){
        rlang::abort(message = c(
          cli::style_bold("Cohort file import failed."),
          "x" = "File type not supported.",
          "i" = "Ensure your cohort file is a .csv, .xls, .xlsx, or .rds file."
        ))
      }

      ## Fail if necessary columns are not in cohort file
      if(FALSE %in% (
        c("ID", "Name", "Mailing", "Physical", "City", "State", "ZIP", "Phone",
          "Email", "Consent", "Status") %in%
        names(cohort)
      )){
        rlang::abort(message = c(
          cli::style_bold("Cohort file import failed."),
          "x" = "Required columns not present in cohort file.",
          "i" = paste0("See ", cli::style_underline("vignette('setup_cohort')"), " for more information.")
        ))
      }

      ## Export cohort file and set path
      input$cohort <- paste0("Cohort/", input$short_name)
      readr::write_csv(cohort, input$cohort)

    } else {
      rlang::abort(message = c(
        cli::style_bold("Cohort file import failed."),
        "x" = "Given input is not a file path.",
        "i" = "If you provided a function to setup_cohort, ensure the function returns the file path as a string."
      ))
    }
  } else {
    rlang::warn(message = c(
      cli::style_bold("No cohort file or function specified!"),
      "!" = "A cohort file is required to use clusteR.",
      "i" = paste0("See ", cli::style_underline("vignette('setup_cohort')"), " for more information.")
    ))
  }

  # Output config
  saveRDS(input, file = "Scripts/config.rds")
  rlang::inform(message = c(
    cli::style_bold("clusteR: Configuration saved.")
  ))
}
