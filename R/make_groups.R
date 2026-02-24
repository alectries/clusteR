#' Generate groups of clusters by proximity for door-to-door surveying
#'
#' Using a kmeans clustering algorithm and Census block latitude and
#' longitude, calculates nearby clusters and attempts to produce approximately
#' even groups. Generates `Contacts/Assignments.csv`, where the user can
#' make adjustments before running `make_walklist` or `make_walkmap` to export
#' simple door-to-door materials.
#'
#' You can filter the clusters by the number of completed, pending, and enrolled
#' participants in each cluster in the `filt` argument. The filter defaults to
#` include all clusters that have at least one resident. You can filter based on
#' the numbers in the following columns:
#'
#' - *n*: The total number of cohort members in the cluster.
#' - *completed*, *completed_pct*: The number or percent (out of 100) of
#' participants with any Completed status.
#' - *pending*, *pending_pct*: The number or percent (out of 100) of
#' participants with an Enrolled, Re-enroll, or Not enrolled status.
#' - *enrolled*, *enrolled_pct*: The number or percent (out of 100) of
#' participants with a Completed - enrolled or Enrolled status.
#'
#' `make_groups` uses the internal function `mult_kmeans` by default. You can
#' write a custom function instead and pass it as an argument to produce data
#' if needed. `make_groups` requires that function output a dataframe with the
#' following columns:
#'
#' - **cluster**, matching the cohort file cluster identifier
#' - **geoid**, matching the Census block GEOID
#' - **lat** and **long**, the latitude and longtitude of a point within the cluster
#' - **ur**, showing whether the block is urban or rural
#' - **geometry**, an *sf* geometry field to draw the block shape
#'
#' Alternatively, you can modify the operation of `mult_kmeans` by passing
#' arguments through `make_groups`:
#'
#' - **k**, required for `make_groups` and defines the number of centers (groups)
#' to generate
#' - **runs**, the number of kmeans runs to perform
#' - **iter.max**, the maximum number of iterations each kmeans run will perform
#'
#' `make_groups` invisibly returns the same table written to
#' `Contacts/Assignments.csv` so you can manipulate it in R if you choose.
#'
#' @param k The number of groups to create.
#' @param filt How to filter the summary for making the groups; see details.
#' @param fn The function to group clusters, by default `mult_kmeans`.
#' @param ... Arguments to be passed to fn.
#' @importFrom cli style_bold
#' @importFrom cli style_underline
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_brewer
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme_void
#' @importFrom magrittr `%>%`
#' @importFrom Polychrome createPalette
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom rlang `!!`
#' @importFrom rlang enquo
#' @importFrom sf read_sf
#' @importFrom stringr str_detect
#' @export

make_groups <- function(k,
                        filt = n > 0,
                        fn = "clusteR:::mult_kmeans",
                        geoids = .cluster$cfg$geoids,
                        shape_county = .cluster$cfg$shape_county,
                        shape_block = .cluster$cfg$shape_block,
                        runs = 300,
                        iter.max = 50,
                        ...
){
  # Definitions
  `%>%` <- magrittr::`%>%`
  `!!` <- rlang::`!!`

  # Get unenrolled clusters
  cohort <- readr::read_csv(paste0("Cohort/", .cluster$cfg$short_name), show_col_types = F)
  summary <- dplyr::summarize(
    cohort,
    .by = Cluster,
    n = n(),
    completed = sum(stringr::str_detect(Status, "Completed"), na.rm = T),
    completed_pct = completed / n * 100,
    pending = sum(Status %in% c("Enrolled", "Re-enroll", "Not enrolled"), na.rm = T),
    pending_pct = pending / n * 100,
    enrolled = sum(Status %in% c("Enrolled", "Completed - enrolled"), na.rm = T),
    enrolled_pct = enrolled / n * 100
  )
  filt <- rlang::enquo(filt)
  include <- sort(unique(dplyr::filter(summary, !!filt)$Cluster))

  # Run grouping function
  assign <- do.call(
    what = eval(parse(text = fn)),
    args = list(
      k = k,
      include = include,
      geoids = geoids,
      shape_county = shape_county,
      shape_block = shape_block,
      runs = runs,
      iter.max = iter.max,
      ...
    )
  )

  # Load county sf
  county <- sf::read_sf(shape_county) %>%
    dplyr::filter(GEOID == as.character(.cluster$cfg$county))

  # Map
  plot_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(geometry = geometry),
      data = county,
      color = "lightblue4",
      linewidth = 1,
      fill = NA
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = group, geometry = geometry, color = ur),
      data = assign,
      linewidth = 1.05,
      inherit.aes = F
    ) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = cluster, geometry = geometry),
      data = assign,
      size = 2.5
    ) +
    ggplot2::scale_fill_manual(values = unname(Polychrome::createPalette(
      N = as.numeric(k),
      seedcolors = c("#F58638", "#008746", "#0098DA"),
      range = c(50, 90),
      target = "normal",
      M = 1000
    ))) +
    ggplot2::labs(
      fill = "Group (auto)",
      color = "Rural/Urban"
    ) +
    ggplot2::scale_color_brewer(palette = "Accent") +
    ggplot2::theme_void()

  # Add manual assignment column, save, and return
  out <- dplyr::arrange(
    dplyr::mutate(
      dplyr::select(assign, geoid, cluster, ur, auto = group),
      manual = ""
    ),
    auto, cluster
  )
  readr::write_csv(out, "Contacts/Assignments.csv")
  print(plot_map)
  rlang::inform(message = c(
    cli::style_bold("Group assignments generated. Next steps:"),
    "*" = paste0("Review ", cli::style_underline("Contacts/Assignments.csv"), "."),
    "*" = "Assign groups by filling in the `manual` column with a group identifier.",
    "*" = "Leave groups as assigned by leaving the `manual` column blank.",
    "*" = paste0("Run ", cli::style_underline("make_walklist"), " or ",
                 cli::style_underline("make_walkmap"), ".")
  ))
  return(invisible(out))
}
