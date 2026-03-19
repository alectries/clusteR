#' Calculate results broken down by a variable and generate report
#'
#' Using archived (by default, cleaned) survey data, automatically combines
#' columns, calculates frequencies, and applies weights to survey responses
#' along a specified variable, then outputs a report of data.
#'
#' Most often, `view_breakdown` will be used to calculate survey responses
#' along a demographic variable. This variable will need to be created during
#' data cleaning, not within `view_breakdown`, and should be named as a string
#' for `strata`.
#'
#' Selecting PDF generates a clean report with one table per question, one
#' group of rows per stratum, and one row in each group per answer choice. If
#' a .Rmd file is specified as `template`, a custom template is used to render
#' the report. The custom template can retrieve data from `tables`, which is a
#' list of tibbles available in the rendering environment. If found in
#' `codebook`, titles and captions are added to the default report and can be
#' accessed by a custom template as the tibble `codebook`; see below for
#' details.
#'
#' Selecting CSV generates a single, clean comma-delimited file with one group
#' of rows per stratum and one row in each group per answer choice. Specifying
#' `template` has no effect other than to produce a warning. Columns found in
#' `codebook` will not be joined to the output by question number.
#'
#' The file specified as `codebook` must be a **tab-delimited** file and include
#' a `QN` column containing the question numbers corresponding to columns in the
#' cleaned data and a `type` column specifying the question type (`mc` for
#' multiple-choice, `ms` for multiple-select, `no` for numeric, or `ft` for
#' free-text). If you have multiple-select questions on your survey (select all
#' that apply), your data likely has multiple columns for these questions;
#' `codebook` should only have one entry for the full question. For example,
#' while your data may have columns `Q01_1`, `Q01_2`, and `Q01_3` for three
#' answer choices, your `codebook` file should only have a `Q01` row. For this
#' reason, `codebook` here is different from in `setup` and probably can't be
#' the same file. For PDF output using the default template, you may also
#' include a `title` column, which will be used as the subheading for each
#' question, and/or a `caption` column, which will be added as paragraph text
#' between the title and the table.  You may include other columns for a custom
#' template, but they will have no effect if using the default template.
#'
#' @param codebook A string; the file path to a tab-delimited file with question names. See details.
#' @param strata A string; the name of the variable by which to break down results.
#' @param data The (cleaned) survey data to use. Defaults to the last `clean_data` output.
#' @param make "pdf" or "csv", the type of output to produce. See details.
#' @param template If producing a PDF report from a custom template, a string; the file path to a .Rmd file.
#' @param wt The column in `data` to use as survey weights. Defaults to equal weight for each response.
#' @param exclude Columns (names, as strings) in `data` to exclude from weighting. Generally, these are your demographic variables.
#' @importFrom cli cli_progress_bar
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr if_all
#' @importFrom dplyr if_else
#' @importFrom dplyr lead
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom kableExtra collapse_rows
#' @importFrom kableExtra column_spec
#' @importFrom knitr asis_output
#' @importFrom knitr kable
#' @importFrom magrittr `%>%`
#' @importFrom purrr map_int
#' @importFrom readr read_tsv
#' @importFrom readr write_csv
#' @importFrom rlang `!!`
#' @importFrom rlang `:=`
#' @importFrom rlang as_string
#' @importFrom rlang enquo
#' @importFrom rlang get_expr
#' @importFrom rlang inform
#' @importFrom rlang is_null
#' @importFrom rlang warn
#' @importFrom rmarkdown render
#' @importFrom tibble add_column
#' @importFrom tibble add_row
#' @importFrom tibble enframe
#' @importFrom tidyselect all_of
#' @importFrom tidyselect any_of
#' @importFrom tidyselect starts_with
#' @export

view_breakdown <- function(codebook,
                           strata,
                           data = .cluster$cfg$last_clean,
                           make = "pdf",
                           template = NA,
                           wt = NULL,
                           exclude = NA
){
  # Definitions
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`
  `:=` <- rlang::`:=`

  # Load data
  rlang::inform(message = c(
    "i" = paste0("Loading responses from ", data, ".")
  ))
  res <- readRDS(data)

  # Codebook
  rlang::inform(message = c(
    "i" = paste0("Loading codebook from ", codebook, ".")
  ))
  cb <- readr::read_tsv(codebook, show_col_types = F)

  # Weights
  wt_quo <- rlang::enquo(wt)
  if(
    !identical(rlang::get_expr(wt_quo), NULL) &&
    rlang::as_string(rlang::get_expr(wt_quo)) %in% names(res)
  ){
    wt <- rlang::as_string(rlang::get_expr(wt_quo))
    rlang::inform(message = c(
      "i" = paste0("Using ", wt, " as weights.")
    ))
  } else {
    rlang::inform(message = c(
      "i" = "Not using weights."
    ))
    wt <- NULL
  }

  # Generate tables
  tables <- list()
  progress <- cli::cli_progress_bar(
    name = "Generating tables...",
    total = nrow(cb)
  )
  for(i in 1:nrow(cb)){

    ## Question info
    q <- cb$QN[i]
    type <- cb$type[i]

    ## For multiple-choice questions
    if(type == "mc"){

      ### Weight denominator
      n_tot <- res %>%
        dplyr::filter(!is.na(!!as.symbol(q))) %>%
        nrow()
      if(!rlang::is_null(wt)){
        wt_tot <- res %>%
          dplyr::filter(!is.na(!!as.symbol(q))) %>%
          dplyr::pull(as.symbol(wt)) %>%
          sum(na.rm = T)
      }

      ### Summarize
      tosum <- dplyr::select(
        res, Q = tidyselect::all_of(q),
        wt = tidyselect::any_of(wt),
        strat = tidyselect::all_of(strata)
      )
      if("wt" %in% names(tosum) && !(q %in% exclude)){

        #### Weighted
        tables[[i]] <- tosum %>%
          dplyr::group_by(strat) %>%
          dplyr::mutate(n_grp = dplyr::n(), wt_grp = sum(wt)) %>%
          dplyr::group_by(strat, Q) %>%
          dplyr::summarize(
            count = dplyr::n(),
            pct = sum(wt) / mean(wt_grp),
            lci = pct - 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
            uci = pct + 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            dplyr::across(pct:uci, ~ifelse(.x < 0, 0, .x)),
            dplyr::across(pct:uci, ~ifelse(.x > 1, 1, .x))
          ) %>%
          dplyr::arrange(strat, dplyr::desc(pct)) %>%
          tibble::add_row(
            strat = NA,
            Q = "Missing",
            count = sum(is.na(dplyr::pull(res, tidyselect::all_of(q))), na.rm = T)
          ) %>%
          dplyr::filter(!is.na(Q)) %>%
          dplyr::rename(!!strata := strat, !!q := Q)
      } else {

        #### Unweighted
        tables[[i]] <- tosum %>%
          dplyr::group_by(strat) %>%
          dplyr::mutate(n_grp = dplyr::n()) %>%
          dplyr::group_by(strat, Q) %>%
          dplyr::summarize(
            count = dplyr::n(),
            pct = count / mean(n_grp),
            lci = pct - 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
            uci = pct + 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            dplyr::across(pct:uci, ~ifelse(.x < 0, 0, .x)),
            dplyr::across(pct:uci, ~ifelse(.x > 1, 1, .x))
          ) %>%
          dplyr::arrange(strat, dplyr::desc(pct)) %>%
          tibble::add_row(
            strat = NA,
            Q = "Missing",
            count = sum(is.na(dplyr::pull(res, tidyselect::all_of(q))), na.rm = T)
          ) %>%
          dplyr::filter(!is.na(Q)) %>%
          dplyr::rename(!!strata := strat, !!q := Q)
      }
    }

    ## For multiple-select questions
    if(type == "ms"){

      ### Weight denominator
      ans <- res %>%
        dplyr::mutate(Q_ans = dplyr::case_when(
          dplyr::if_all(tidyselect::starts_with(q), is.na) ~ F,
          .default = T
        ))
      n_tot <- ans %>%
        dplyr::filter(Q_ans) %>%
        nrow()
      if(!is.null(wt)){
        wt_tot <- ans %>%
          dplyr::filter(Q_ans) %>%
          dplyr::pull(as.symbol(wt)) %>%
          sum()
      }

      ### Summarize
      tables[[i]] <- list()
      for(h in 1:ncol(dplyr::select(res, tidyselect::starts_with(q)))){
        tosum <- ans %>%
          dplyr::filter(Q_ans) %>%
          dplyr::select(
            Q = tidyselect::starts_with(q)[h],
            wt = tidyselect::any_of(wt),
            strat = tidyselect::all_of(strata)
          )
        if("wt" %in% names(tosum) && !(q %in% exclude)){

          #### Weighted
          tables[[i]][[h]] <- tosum %>%
            dplyr::group_by(strat) %>%
            dplyr::mutate(n_grp = dplyr::n(), wt_grp = sum(wt)) %>%
            dplyr::group_by(strat, Q) %>%
            dplyr::summarize(
              count = dplyr::n(),
              pct = sum(wt) / mean(wt_grp),
              lci = pct - 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              uci = pct + 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              dplyr::across(pct:uci, ~ifelse(.x < 0, 0, .x)),
              dplyr::across(pct:uci, ~ifelse(.x > 1, 1, .x))
            ) %>%
            dplyr::filter(!is.na(Q))
        } else {

          #### Unweighted
          tables[[i]][[h]] <- tosum %>%
            dplyr::group_by(strat) %>%
            dplyr::mutate(n_grp = dplyr::n()) %>%
            dplyr::group_by(strat, Q) %>%
            dplyr::summarize(
              count = dplyr::n(),
              pct = count / mean(n_grp),
              lci = pct - 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              uci = pct + 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              dplyr::across(pct:uci, ~ifelse(.x < 0, 0, .x)),
              dplyr::across(pct:uci, ~ifelse(.x > 1, 1, .x))
            ) %>%
            dplyr::filter(!is.na(Q))
        }
      }
      tables[[i]] <- dplyr::bind_rows(tables[[i]]) %>%
        dplyr::arrange(strat, dplyr::desc(pct)) %>%
        tibble::add_row(
          strat = NA,
          Q = "Missing",
          count = sum(!ans$Q_ans, na.rm = T)
        ) %>%
        dplyr::rename(!!strata := strat, !!q := Q)
    }

    ## For numeric questions
    if(type == "no"){

      ### Set to list
      tables[[i]] <- list()

      ### Loop through strata
      for(h in 1:length(unique(res[[strata]]))){

        #### Pull variable
        var <- res %>%
          dplyr::filter(!!rlang::sym(strata) == unique(res[[strata]])[i]) %>%
          dplyr::pull(q) %>%
          as.numeric()

        #### Summarize
        tables[[i]][[h]] <- var %>%
          quantile(c(0, 0.25, 0.333, 0.5, 0.667, 0.75, 1), na.rm = T) %>%
          tibble::enframe(q, "value") %>%
          mutate(
            "count" = purrr::map_int(
              dplyr::row_number(),
              ~sum(var >= value[.x] & var < dplyr::lead(value)[.x], na.rm = T)
            )
          ) %>%
          dplyr::filter(value != max(var, na.rm = T)) %>%
          tibble::add_row(
            !!q := "100%",
            value = max(var, na.rm = T),
            count = sum(var == max(var, na.rm = T), na.rm = T)
          ) %>%
          tibble::add_column(
            !!strata := unique(res[[strata]])[i],
            .before = 1
          )
      }

      ### Merge and output
      tables[[i]] <- dplyr::bind_rows(tables[[i]]) %>%
        dplyr::arrange(!!rlang::sym(strata), value) %>%
        tibble::add_row(
          !!strata := NA,
          !!q := "Missing",
          value = NA,
          count = sum(is.na(res[[q]]), na.rm = T)
        )
    }

    ## For free-text questions
    if(type == "ft"){

      ### Weight denominator
      ans <- res %>%
        dplyr::mutate(Q_ans = dplyr::case_when(
          dplyr::if_all(tidyselect::starts_with(q), is.na) ~ F,
          .default = T
        ))
      n_tot <- ans %>%
        dplyr::filter(Q_ans) %>%
        nrow()
      if(!is.null(wt)){
        wt_tot <- ans %>%
          dplyr::filter(Q_ans) %>%
          dplyr::pull(as.symbol(wt)) %>%
          sum()
      }

      ### Summarize
      tables[[i]] <- list()
      for(h in 1:ncol(dplyr::select(res, tidyselect::starts_with(q)))){
        tosum <- ans %>%
          dplyr::filter(Q_ans) %>%
          dplyr::select(
            Q = tidyselect::starts_with(q)[h],
            wt = tidyselect::any_of(wt),
            strat = tidyselect::all_of(strata)
          )
        if("wt" %in% names(tosum) && !(q %in% exclude)){

          #### Weighted
          tables[[i]][[h]] <- tosum %>%
            dplyr::group_by(strat) %>%
            dplyr::mutate(n_grp = dplyr::n(), wt_grp = sum(wt)) %>%
            dplyr::group_by(strat, Q) %>%
            dplyr::summarize(
              count = dplyr::n(),
              pct = sum(wt) / mean(wt_grp),
              lci = pct - 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              uci = pct + 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              dplyr::across(pct:uci, ~ifelse(.x < 0, 0, .x)),
              dplyr::across(pct:uci, ~ifelse(.x > 1, 1, .x))
            ) %>%
            dplyr::filter(!is.na(Q))
        } else {

          #### Unweighted
          tables[[i]][[h]] <- tosum %>%
            dplyr::group_by(strat) %>%
            dplyr::mutate(n_grp = dplyr::n()) %>%
            dplyr::group_by(strat, Q) %>%
            dplyr::summarize(
              count = dplyr::n(),
              pct = count / mean(n_grp),
              lci = pct - 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              uci = pct + 1.96 * sqrt(pct * (1 - pct) / mean(n_grp)),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              dplyr::across(pct:uci, ~ifelse(.x < 0, 0, .x)),
              dplyr::across(pct:uci, ~ifelse(.x > 1, 1, .x))
            ) %>%
            dplyr::filter(!is.na(Q))
        }
      }
      tables[[i]] <- dplyr::bind_rows(tables[[i]]) %>%
        dplyr::arrange(strat, dplyr::desc(pct)) %>%
        add_row(
          strat = NA,
          Q = "Missing",
          count = sum(!ans$Q_ans, na.rm = T)
        ) %>%
        dplyr::rename(!!strata := strat, !!q := Q)
    }

    ## Progress bar
    cli::cli_progress_update()
  }

  # PDF
  if(make == "pdf"){
    ## Knit
    knit_env <- list(
      "cluster_cfg" = .cluster$cfg,
      "tables" = tables,
      "codebook" = cb,
      "strata" = strata
    )
    if(is.na(template)){
      rmarkdown::render(
        input = system.file("auto", "view_breakdown.Rmd", package = "clusteR"),
        output_dir = paste0(getwd(), "/Survey Data"),
        output_file = paste0("Breakdown Report by ", strata, ".pdf"),
        envir = knit_env,
        quiet = T
      )
    } else {
      rmarkdown::render(
        input = template,
        output_dir = paste0(getwd(), "/Survey Data"),
        output_file = paste0("Breakdown Report by ", strata, ".pdf"),
        envir = knit_env,
        quiet = T
      )
    }
    if(Sys.getenv("RSTUDIO") == "1"){
      rstudioapi::viewer(paste0("Survey Data/Breakdown Report by ", strata, ".pdf"))
    } else {
      rlang::inform(
        message = c(
          cli::style_bold("Report generated."),
          "i" = paste0("See ", cli::style_underline(paste0("Survey Data/Breakdown Report by ", strata, ".pdf")), ".")
        )
      )
    }
    return()
  }

  # CSV
  if(make == "csv"){
    ## Warn if template is present
    if(!is.na(template)){
      rlang::warn(message = c(
        "!" = "Template detected but ignored for CSV output."
      ))
    }

    ## Loop to get names and format
    for(i in 1:length(tables)){
      q <- names(tables[[i]])[2]
      names(tables[[i]])[2] <- "answer"
      tables[[i]] <- tibble::add_column(
        tables[[i]],
        "QN" = q,
        .before = 1
      )
    }

    ## Bind and join
    out <- dplyr::left_join(
      cb,
      dplyr::bind_rows(tables),
      by = "QN"
    )

    ## Output
    readr::write_csv(out, "Survey Data/Topline.csv")
    rlang::inform(
      message = c(
        cli::style_bold("Report generated."),
        "i" = paste0("See ", cli::style_underline("Survey Data/Topline.csv"), ".")
      ))
    return()
  }
}
