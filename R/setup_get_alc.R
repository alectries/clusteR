#' Set up a connection to an Alchemer survey
#'
#' Generates the setup_get item in cluster_cfg for a connection to an Alchemer
#' survey.
#'
#' For [`get_alc`] to properly process survey data, certain information is
#' required:
#'
#' `survey_id`: The numeric survey identifier, found after /id/ in the URL of
#' the survey builder page.
#'
#' `api_token`: Your API Key found by clicking your profile icon at the top
#' right of your Alchemer interface and selecting "Your API Key".
#'
#' `api_token_secret`: Your API Secret Key found by clicking your profile icon
#' at the top right of your Alchemer interface and selecting "Your API Key".
#'
#' `codebook`: The file path to a *tab*-delimited text file (not
#' comma-delimited) with two columns: QN and ALC. The QN column should be a
#' question number or other standard column identifier (such as ID, Name, etc.),
#' while the ALC column should be the exact text of the question in Alchemer.
#'
#' Some additional rules apply to `codebook`. For multiple-select questions,
#' you will need to include in your codebook a row for each answer. Each line
#' should be formatted as follows:
#'
#' - The question number (Q##) should be consistent for all responses to a question.
#' - The question number should be followed by an underscore and a unique answer number for each response (_##).
#' - The question text (ALC column) should be the exact text of the question, followed immediately by a period, then immediately by the exact text of the response.
#'
#' For example:
#'
#' | QN     | ALC                                                                   |
#' |:-------| :-------------------------------------------------------------------- |
#' | Q01_01 | 1. What is your race? (Select all that apply.).Black                  |
#' | Q01_02 | 1. What is your race? (Select all that apply.).Hispanic or Latino     |
#' | Q01_03 | 1. What is your race? (Select all that apply.).White                  |
#'
#' It is extremely important that all question and answer text appears exactly
#' as it appears in Alchemer, in full. Shortening the text in ALC will cause
#' `get_alc` to produce nonsensical results or fail.
#'
#' `consent_opts`: A named vector to convert your Alchemer survey's consent
#' question responses to clusteR's Consent variable. This should be in the
#' format `c(Consent = "Survey response")`, e.g.
#' `c(Yes = "I agree", No = "I don't agree")`. Valid `Consent` values are
#' shown in `vignette("setup_cohort")`. For valid `Consent` values with spaces,
#' enclose the name in backticks (`).
#'
#' Due to a restriction in Alchemer's API, the maximum number of observations
#' pulled by `get_alc` is 9,999. You will need to write a custom function if
#' this is not adequate for your survey.
#'
#' @param survey_id The Survey ID for your Alchemer survey, found after /id/ in the builder URL.
#' @param api_token Your API token.
#' @param api_token_secret Your API token secret (password).
#' @param codebook The file path, as a string, to a tab-delimited text file to key your survey data to standard column names.
#' @param consent_opts A named vector, where names are clusteR Consent options and values are survey responses.
#' @importFrom cli style_bold
#' @importFrom jsonlite fromJSON
#' @importFrom readr read_tsv
#' @importFrom tibble enframe
#' @export

setup_get_alc <- function(survey_id,
                          api_token,
                          api_token_secret,
                          codebook,
                          consent_opts
){
  # Make URL
  url <- paste0(
    "https://api.alchemer.com/v5/survey/",
    survey_id,
    "/surveyresponse?api_token=",
    api_token,
    "&api_token_secret=",
    api_token_secret,
    "&resultsperpage=9999"
  )

  # Test URL
  alc <- jsonlite::fromJSON(url)
  if(!alc$result_ok){
    rlang::abort(message = c(
      cli::style_bold("Data import from Alchemer failed."),
      "i" = paste0("HTTP error: ", alc$code),
      "x" = alc$message
    ))
  }

  # Test codebook for compliance
  codebook_file <- read_delim(codebook, delim = "  ", show_col_types = F)
  if(FALSE %in% (names(codebook_file) == c("QN", "ALC"))){
    rlang::abort(message = c(
      cli::style_bold("Data import from Alchemer failed."),
      "i" = "Codebook names do not match requirements.",
      "x" = paste0(
        names(codebook_file)[!names(codebook_file) %in% c("QN", "ALC")],
        " is not QN or ALC."
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
    get = "clusteR::get_alc",
    url = url,
    codebook = codebook,
    consent = consent
  )
  return(setup_get)
}
