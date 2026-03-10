#' Generate a report on survey completion and enrollment by cluster
#'
#' Generates and opens a brief HTML report. The HTML report is standalone and
#' can be shared as a file.
#'
#' Included on the report is a map, which color-codes clusters by their
#' survey completion percentages. The default color-coding scheme is based on
#' CDC CASPER, which requires 80% completion per cluster. You can manually
#' adjust this scheme by adjusting the vectors of breaks and colors. There
#' should be one more color specified than the number of breaks.
#'
#' @param breaks A vector of numbers as percentages out of 100.
#' @param colors A vector of strings representing colors.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr count
#' @importFrom dplyr join_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr summarize
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_fill_gradient2
#' @importFrom ggplot2 scale_fill_stepsn
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme_void
#' @importFrom kableExtra kable_material
#' @importFrom kableExtra row_spec
#' @importFrom knitr kable
#' @importFrom lubridate now
#' @importFrom magrittr `%>%`
#' @importFrom readr read_csv
#' @importFrom rlang inform
#' @importFrom rmarkdown render
#' @importFrom rstudioapi viewer
#' @importFrom sf read_sf
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
#' @importFrom tidyr spread
#' @importFrom tidyselect everything
#' @export

view_progress <- function(breaks = c(1, 30, 50, 80, 90),
                          colors = c("white", "tomato3", "tomato2", "tomato",
                                     "lightblue", "lightblue4")
){
  # Definitions
  `%>%` <- magrittr::`%>%`

  # Get cohort file
  cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name),
                            show_col_types = F)

  # Time and message
  time <- lubridate::now()
  rlang::inform(message = c(
    cli::style_bold("Have you updated the cohort data file?"),
    "!" = paste0("First, run ", cli::style_underline("update_cohort"), "."),
    "i" = paste0("This report is being generated at ", time)
  ))

  # Topline stats
  completion <- dplyr::filter(
    cohort,
    stringr::str_detect(Status, "Completed")
  ) %>%
    nrow()
  total <- nrow(cohort)
  n_clusters <- length(unique(cohort$Cluster))

  # Overall cohort status
  stat_overall <- cohort %>%
    dplyr::count(Status) %>%
    dplyr::mutate(
      Status = factor(
        x = Status,
        levels = c("Completed - enrolled", "Completed - re-enroll",
                   "Completed - unenroll", "Enrolled", "Re-enroll",
                   "Not enrolled", "Unenroll", "DO NOT CONTACT")
      ),
      Status_simp = dplyr::case_when(
        Status %in% c("Completed - enrolled", "Completed - re-enroll",
                      "Completed - unenroll") ~ 1,
        Status %in% c("Enrolled", "Re-enroll", "Not enrolled") ~ 2,
        Status %in% c("Unenroll", "DO NOT CONTACT") ~ 3
      )
    )
  plot_stat_overall <- stat_overall %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(
      ggplot2::aes(x = Status, y = n, fill = Status_simp),
      color = "black",
      show.legend = F
    ) +
    ggplot2::scale_fill_gradient2(
      midpoint = 2,
      low = "lightblue",
      mid = "gray",
      high = "tomato"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = Status, y = n, label = n),
      vjust = -0.5
    ) +
    ggplot2::labs(
      x = "Cohort status",
      y = "Current count",
      fill = "Survey status"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1)
    )

  # Cohort status table by cluster
  total <- nrow(cohort)
  stat_cluster <- cohort %>%
    dplyr::mutate(
      Completion = dplyr::case_when(
        Status %in% c("Completed - enrolled", "Completed - re-enroll",
                      "Completed - unenroll") ~ "Completed",
        Status %in% c("Enrolled", "Re-enroll",
                      "Not enrolled") ~ "Not yet completed",
        Status %in% c("Unenroll", "DO NOT CONTACT") ~ "Refused"
      )
    ) %>%
    dplyr::count(Cluster, Completion) %>%
    tidyr::spread(Completion, n) %>%
    dplyr::mutate(
      "Completed" = ifelse("Completed" %in% names(.) && !is.na(Completed), Completed, 0),
      Completion = paste0(round(Completed / (Completed + `Not yet completed`) * 100, 1), "%")
    ) %>%
    dplyr::arrange(Cluster) %>%
    dplyr::bind_rows(dplyr::summarize(
      .,
      Cluster = NA,
      Completed = sum(Completed, na.rm = T),
      `Not yet completed` = sum(`Not yet completed`, na.rm = T),
      Completion = paste0(round(Completed / (Completed = `Not yet completed`) * 100, 1), "%")
    ))

  # Cohort status map by cluster
  blocks <- dplyr::left_join(
    readr::read_csv(.cluster$cfg$geoids, show_col_types = F) %>%
      dplyr::mutate(geoid = as.character(geoid)),
    sf::read_sf(.cluster$cfg$shape_block),
    by = dplyr::join_by(geoid == GEOID20)
  ) %>%
    dplyr::rename("Cluster" = cluster)
  county <- sf::read_sf(.cluster$cfg$shape_county) %>%
    dplyr::filter(
      STATEFP == .cluster$cfg$state,
      COUNTYFP %in% substr(.cluster$cfg$county, 3, 5)
    )
  map_stat_cluster <- dplyr::left_join(
    blocks,
    stat_cluster,
    by = "Cluster"
  ) %>%
    dplyr::mutate(
      Completion = as.numeric(gsub("%", "", Completion))
    )
  plot_stat_cluster <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geometry),
      data = county,
      color = "lightblue4",
      linewidth = 1,
      fill = NA
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = Completion, geometry = geometry),
      data = map_stat_cluster,
      color = "lightgray",
      linewidth = 1.05,
      inherit.aes = F
    ) +
    ggplot2::geom_sf_text(
      aes(label = Cluster, geometry = geometry),
      data = map_stat_cluster,
      size = 2.5
    ) +
    ggplot2::scale_fill_stepsn(
      breaks = breaks,
      limits = c(0, 100),
      colors = colors
    ) +
    ggplot2::theme_void()

  # Knit
  knit_env <- list(
    time = time,
    completion = completion,
    total = total,
    n_clusters = n_clusters,
    plot_stat_overall = plot_stat_overall,
    stat_cluster = stat_cluster,
    plot_stat_cluster = plot_stat_cluster
  )
  rmarkdown::render(
    input = system.file("auto", "view_progress.Rmd", package = "clusteR"),
    output_dir = paste0(getwd(), "/Cohort"),
    output_file = "Progress Report.html",
    envir = knit_env,
    quiet = T
  )
  if(Sys.getenv("RSTUDIO") == "1"){
    rstudioapi::viewer("Cohort/Progress Report.html")
  } else {
    rlang::inform(message = c(
      cli::style_bold("Progress report generated."),
      "i" = paste0("Open ", cli::style_underline("Cohort/Progress Report.html"), ".")
    ))
  }
}
