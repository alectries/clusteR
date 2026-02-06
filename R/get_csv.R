#' get_csv: Get data from a comma-delimited survey data file
#'
#' Returns a dataframe of survey data from a comma-delimited file specified in
#' setup.
#'
#' `get_csv` is the simplest data-gathering function. It pulls data from a
#' .csv input file that you specify during setup. The .csv can generally be
#' whatever you'd like it to be, but it must contain a few essential fields:
#'
#' - ID: The unique ID of the survey participant.
#' - Name: The name of the survey participant.
#' - Phone: The survey participant's phone number.
#' - Email: The survey participant's email address.
#' - Consent: The survey participant's consent status.
#'
#' You can format these columns largely however you'd like, and you can even
#' leave them blank if they are not of use to you, but the columns must exist
#' and be named correctly.
#'
#' @importFrom readr read_csv
#' @export

get_csv <- function(){
  do.call(
    what = "readr::read_csv",
    args = cluster_cfg$setup_get[!names(cluster_cfg$setup_get) %in% c("run_before", "get")]
  )
}
