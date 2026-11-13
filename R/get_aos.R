#' Get data from an AOS survey
#'
#' Returns a dataframe of survey data from an AOS survey specified in
#' setup.
#'
#' `get_aos` pulls data from [AOS](https://www.lhdacademyofscience.org/), a
#' collaborative survey platform for local public health agencies. It is quite
#' flexible, but some key fields must exist after conversion using the codebook
#' specified in [`setup_get_aos`]:
#'
#' - ID: The unique ID of the survey participant.
#' - Name: The name of the survey participant.
#' - Phone: The survey participant's phone number.
#' - Email: The survey participant's email address.
#' - Consent: The survey participant's consent status.
#'
#' You can format these fields largely however you'd like, and you can even
#' leave them blank if they are not of use to you, but the fields must exist
#' and be named correctly.
#'
#' Currently, `get_aos` can only handle four AOS output types: textbox,
#' radio, essay, and parent. Most question types yield one of these field types,
#' but you should verify before setup.
#'
#' @importFrom cli style_bold
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr `%>%`
#' @importFrom purrr imap
#' @importFrom purrr list_c
#' @importFrom purrr map_dfc
#' @importFrom purrr map_vec
#' @importFrom purrr pluck
#' @importFrom purrr pmap
#' @importFrom readr read_tsv
#' @importFrom rlang set_names
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @importFrom tibble deframe
#' @importFrom tibble tibble
#' @importFrom tibble tibble_row
#' @importFrom tidyselect where
#' @export

get_aos <- function(){
  # Define magrittr pipe
  `%>%` <- magrittr::`%>%`

  # Connect to API
  tryCatch(
    {aos <- jsonlite::fromJSON(.cluster$cfg$setup_get$url)},
    error = function(e){
      rlang::abort(message = c(
        cli::style_bold("Data import from AOS failed."),
        "x" = e
      ))
    }
  )
  if(aos$count < 1){
    rlang::abort(message = c(
      cli::style_bold("No responses yet! Data processing stopped.")
    ))
  } else {
    rlang::inform(message = c(
      cli::style_bold("Data download from AOS successful."),
      "v" = paste0("Responses: ", aos$count)
    ))
  }

  # Get responses
  res.list <- purrr::pmap(
    aos$questions,
    \(id, title, type){
      ans <- purrr::pluck(aos, "responses", "answers", id)
      if(type == "checkbox"){
        val <- sort(unique(as.character(purrr::list_c(ans))))
        res <- purrr::map_dfc(rlang::set_names(val), \(v){
          purrr::map_vec(ans, \(r){if(v %in% r){v} else {NA}})
        })
      } else {
        res <- ans
      }
      return(res)
    }
  )
  names(res.list) <- aos$questions$title
  res.list <- purrr::imap(
    res.list,
    \(q, n){
      if(is.data.frame(q)){
        dplyr::rename_with(q, ~paste(n, .x, sep = "."))
      } else {
        q
      }
    }
  )

  # Convert to frame, rename, and modify consent column
  res.bind <- dplyr::bind_cols(res.list)
  names(res.bind) <- gsub("\u00A0", " ", names(res.bind))
  res <- dplyr::rename(
    res.bind,
    !!!readr::read_tsv(.cluster$cfg$setup_get$codebook,
                       show_col_types = F) %>%
      dplyr::filter(AOS %in% names(res.bind)) %>%
      tibble::deframe()
  ) %>%
    dplyr::select(., ID, Name, Email, Phone, Consent, sort(names(.))) %>%
    dplyr::select(tidyselect::where(~!all(is.na(.)))) %>%
    dplyr::rename("Consent_old" = Consent) %>%
    dplyr::left_join(
      .cluster$cfg$setup_get$consent,
      by = "Consent_old"
    ) %>%
    dplyr::select(-Consent_old)


  # Return data
  return(res)
}
