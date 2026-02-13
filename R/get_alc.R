#' Get data from an Alchemer survey
#'
#' Returns a dataframe of survey data from an Alchemer survey specified in
#' setup.
#'
#' `get_alc` pulls data from [Alchemer](https://www.alchemer.com/), a survey
#' platform. It is quite flexible, but some key fields must exist after
#' conversion using the codebook specified in [`setup_get_alc`]:
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
#' Currently, `get_alc` can only handle four Alchemer output types: textbox,
#' radio, essay, and parent. Most question types yield one of these field types,
#' but you should verify before setup.
#'
#' @importFrom cli style_bold
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr `%>%`
#' @importFrom readr read_delim
#' @importFrom tibble deframe
#' @importFrom tibble tibble
#' @importFrom tibble tibble_row
#' @importFrom tidyselect where
#' @export

get_alc <- function(){
  # Define magrittr pipe
  `%>%` <- magrittr::`%>%`

  # Connect to API
  alc <- jsonlite::fromJSON(cluster_cfg$setup_get$url)
  if(alc$result_ok){
    rlang::inform(message = c(
      cli::style_bold("Data download from Alchemer successful."),
      "v" = paste0("Responses: ", alc$total_count)
    ))
  } else {
    rlang::abort(message = c(
      cli::style_bold("Data import from Alchemer failed."),
      "i" = paste0("HTTP error: ", alc$code),
      "!" = alc$message
    ))
  }

  # Prep table of question types
  types <- tibble::tibble(
    .rows = 0,
    "id" = c(),
    "question" = c(),
    "type" = c()
  )

  # Build types table
  for(i in 1:length(alc$data$survey_data)){

    # Get question
    q <- alc$data$survey_data[[i]]

    # Log information
    types <- dplyr::bind_rows(
      types,
      tibble::tibble_row(
        id = unique(q$id)[!is.na(unique(q$id))],
        question = unique(q$question)[!is.na(unique(q$question))],
        type = unique(q$type)[!is.na(unique(q$type))]
      )
    )
  }
  types <- filter(types, !is.na(question))

  # Make results list
  res.list <- list()

  # Question type functions
  q.textbox <- function(q, i){ # For small free-text questions
    # Add data to list
    return(q$answer)
  }
  q.radio <- function(q, i){ # For multiple-choice questions
    # Add data to list
    return(q$answer)
  }
  q.essay <- function(q, i){ # For long free-text questions
    # Add data to list
    return(q$answer)
  }
  q.parent <- function(q, j){ # For multiple-select (select all that apply) questions
    choice <- q$options[[j]]$option[!is.na(unique(q$options[[j]]$option))]
    return(q$options[[j]]$answer)
  }

  # Pull data
  for(i in 1:nrow(types)){

    # Get question
    q <- alc$data$survey_data[[i]]

    # Determine question type
    type <- types$type[[i]]

    # Transform data
    if(type == "TEXTBOX"){res.list[[types$question[[i]]]] <- q.textbox(q, i)}
    if(type == "RADIO"){res.list[[types$question[[i]]]] <- q.radio(q, i)}
    if(type == "ESSAY"){res.list[[types$question[[i]]]] <- q.essay(q, i)}
    if(type == "parent"){

      # Loop through all options for multiple select
      for(j in 1:length(q$options)){

        # Get answer object for option j
        ans <- q.parent(q, j)

        # Get the text of the choice (replace with other if needed)
        choice <- unique(ans)[!is.na(unique(ans))]
        if(length(choice) > 1){choice <- "other"}

        # Create answer vector, named with question and choice
        res.list[[paste0(types$question[[i]], ".", choice)]] <- ans
      }
    }
  }

  # Convert to frame, rename, and modify consent column
  res.bind <- dplyr::bind_cols(res.list)
  names(res.bind) <- gsub("\u00A0", " ", names(res.bind))
  res <- dplyr::rename(
    res.bind,
    !!!readr::read_delim(cluster_cfg$setup_get$codebook, delim = "  ",
                  show_col_types = F) %>%
      dplyr::filter(ALC %in% names(res.bind)) %>%
      tibble::deframe()
  ) %>%
    dplyr::select(., ID, Name, Email, Phone, Consent, sort(names(.))) %>%
    dplyr::select(tidyselect::where(~!all(is.na(.)))) %>%
    dplyr::rename("Consent_old" = Consent) %>%
    dplyr::left_join(
      cluster_cfg$setup_get$consent,
      by = "Consent_old"
    ) %>%
    dplyr::select(-Consent_old)


  # Return data
  return(res)
}
