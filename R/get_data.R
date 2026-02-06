#' get_data: Standardize and save survey data
#'
#' Using the function specified during [`setup`], pull and process the latest
#' survey data, then archive it. Invisibly returns the path to the saved data.
#'
#' `get_data` is the function you should run when you need to download and save
#' data from your source. It requires proper configuration via `setup`, and
#' obtains data based on parameters set using a `setup_get` function.
#'
#' If you are using a function from a different package to download data for
#' clusteR, you may wish to attach it using `library` before running this
#' function. If you are using a custom function from an R script to download
#' data for clusteR, ensure you have set it up properly by including the
#' script's file path in the clusteR environment as
#' `cluster_cfg$setup_get$run_before`. This should be handled by your custom
#' `setup_get_x` function as described in `vignette('setup_get')`.
#'
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom rlang inform
#' @export

get_data <- function(){
  # Execute run_before script if it exists
  if(exists("cluster_cfg$setup_get$run_before")){
    source(cluster_cfg$setup_get$run_before)
  }

  # Run get function
  data <- do.call(
    what = cluster_cfg$setup_get$get
  )

  # Save data and return path invisibly
  time <- gsub('[:. ]', '-', lubridate::now())
  path <- paste0("Survey Data/", cluster_cfg$short_name, "-", time, ".rds")
  saveRDS(data, path)
  rlang::inform(
    message = c(
      cli::style_bold("Data archived:"),
      "i" = cli::style_underline(path)
    )
  )
  return(invisible(path))
}
