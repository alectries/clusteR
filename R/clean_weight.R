#' Calculate weights from logistic regression and append to data
#'
#' This function uses logistic regression to weight survey data for analysis.
#' It should be used as the `wt` argument in `clean_data`. Returns a dataframe
#' with inverse probability weights that is appended to your data by
#' `clean_data`. Specifically, it will create a three-column dataframe
#' with participant IDs, the probability calculated by regression (`prob`), and
#' the inverse probability weight (`ipw`).
#'
#' Technically, `clean_weight` returns a function, which is then called by
#' `clean_data` with the survey data as an argument.
#'
#' You can import data that applies to all clusters by specifying a file as
#' .import. This file must include a column, `Cluster`, with cluster
#' identifiers consistent with the cohort file. Additional data for cohort
#' members should be stored in the cohort file, which will be imported and
#' joined before mutates are applied.
#'
#' By default, every entry in the cohort file is included for weighting. You
#' can filter those included for weighting using .filt: for example, you can
#' use `.filt = stringr::str_detect(Status, "Completed")` to only apply
#' weighting to participants who have completed the survey.
#'
#' @param formula The regression formula to pass to `glm`.
#' @param ... Mutates to perform on data before regression; passed to `dplyr::mutate`.
#' @param .import A string; the file path to a delimited file with cluster-level data to join to the survey data. The file must contain a column titled `Cluster` with the cluster identifiers.
#' @param .filt Filter data before weighting; defaults to include everyone in the cohort file.
#' @importFrom dplyr c_across
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom magrittr `%>%`
#' @importFrom readr read_csv
#' @importFrom rlang `!!`
#' @importFrom rlang expr
#' @importFrom tidyselect all_of
#' @export

clean_weight <- function(formula,
                         ...,
                         .import = NA,
                         .filt = T
){
  function(x){
    # Definitions
    `%>%` <- magrittr::`%>%`
    `!!` <- rlang::`!!`

    # Load cohort file and join data
    cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name),
                              show_col_types = F)
    data <- dplyr::left_join(
      cohort,
      dplyr::select(x, -any_of(names(cohort)[names(cohort) != "ID"])),
      by = "ID"
    )

    # If specified, import and join
    if(!is.na(.import)){
      data <- dplyr::left_join(
        data,
        readr::read_delim(.import, show_col_types = F),
        by = "Cluster"
      )
    }

    # Filter before weighting
    data <- dplyr::filter(data, !!rlang::expr(.filt))

    # Perform mutates
    data <- dplyr::mutate(data, ...)

    # Regression
    model <- glm(
      formula = formula,
      family = binomial(link = "logit"),
      data = data
    )

    # Calculate IPWs
    vars <- c("ones", vars)
    data_wt <- data %>%
      dplyr::mutate(ones = 1) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        prob = exp(sum(
          dplyr::c_across(
            tidyselect::all_of(c("ones", names(model$coefficients)[-1]))
          ) * model$coefficients,
          na.rm = TRUE
        )),
        ipw = 1 / prob
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID, prob, ipw)

    # Return weights
    return(data_wt)
  }
}
