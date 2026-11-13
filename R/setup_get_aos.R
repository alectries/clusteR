#' Set up a connection to an AOS survey
#'
#' Generates the setup_get item in .cluster$cfg for a connection to an AOS
#' survey.
#'
#' For [`get_aos`] to properly process survey data, certain information is
#' required:
#'
#' `survey_id`: The numeric survey identifier, found after /id/ in the URL of
#' the survey builder page.
#'
#' `api_key`: Your API Key created under Data Connections in Manage Team on the
#' AOS platform.
#'
#' `codebook`: The file path to a *tab*-delimited text file (not
#' comma-delimited) with two columns: QN and AOS. The QN column should be a
#' question number or other standard column identifier (such as ID, Name, etc.),
#' while the AOS column should be the exact text of the question in AOS.
#'
#' Some additional rules apply to `codebook`. For multiple-select questions,
#' you will need to include in your codebook a row for each answer. Each line
#' should be formatted as follows:
#'
#' - The question number (Q##) should be consistent for all responses to a question.
#' - The question number should be followed by an underscore and a unique answer number for each response (_##).
#' - The question text (AOS column) should be the exact text of the question, followed immediately by a period, then immediately by the exact text of the response.
#'
#' For example:
#'
#' | QN     | AOS                                                                   |
#' |:-------| :-------------------------------------------------------------------- |
#' | Q01_01 | 1. What is your race? (Select all that apply.).Black                  |
#' | Q01_02 | 1. What is your race? (Select all that apply.).Hispanic or Latino     |
#' | Q01_03 | 1. What is your race? (Select all that apply.).White                  |
#'
#' It is extremely important that all question and answer text appears exactly
#' as it appears in AOS, in full. Shortening the text in AOS will cause
#' `get_aos` to produce nonsensical results or fail.
#'
#' `consent_opts`: A named vector to convert your AOS survey's consent
#' question responses to clusteR's Consent variable. This should be in the
#' format `c("Consent" = "Survey response")`, e.g.
#' `c("Yes" = "I agree", "No" = "I don't agree")`. Valid `Consent` values are
#' shown in `vignette("setup_cohort")`.
#'
#' @param survey_id The Survey ID for your AOS survey, found beside the survey title on the Build page.
#' @param api_key Your API key, found in Manage Team > Data Connections.
#' @param codebook The file path, as a string, to a tab-delimited text file to key your survey data to standard column names.
#' @param consent_opts A named vector, where names are clusteR Consent options and values are survey responses.
#' @importFrom cli style_bold
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_tsv
#' @importFrom tibble enframe
#' @export

setup_get_aos <- function(survey_id,
                          api_key,
                          codebook,
                          consent_opts
){
  # Make URL
  url <- paste0(
    "https://www.lhdacademyofscience.org/api/data/surveys/",
    survey_id,
    "/responses.json?status=complete,partial&api_key=",
    api_key
  )

  # Test URL
  tryCatch(
    {aos <- jsonlite::fromJSON(url)},
    error = function(e){
      rlang::abort(message = c(
        cli::style_bold("Data import from AOS failed."),
        "x" = e
      ))
    }
  )

  # Test codebook for compliance
  codebook_file <- read_tsv(codebook, show_col_types = F)
  if(FALSE %in% (names(codebook_file) == c("QN", "AOS"))){
    rlang::abort(message = c(
      cli::style_bold("Data import from AOS failed."),
      "i" = "Codebook names do not match requirements.",
      "x" = paste0(
        names(codebook_file)[!names(codebook_file) %in% c("QN", "AOS")],
        " is not QN or AOS."
      )
    ))
  }

  # Build consent_opts and test for compliance
  consent <- tibble::enframe(
    consent_opts,
    name = "Consent",
    value = "Consent_old"
  )

  # Write setup_get
  setup_get <- list(
    get = "clusteR::get_aos",
    url = url,
    codebook = codebook,
    consent = consent
  )
  return(setup_get)
}
