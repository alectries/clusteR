#' Generate a map of clusters
#'
#' Produces a simple, labeled map of your clusters. Simpler than `make_groups`,
#' which is for cluster grouping.
#'
#' @param title The title of the resulting map, if desired.
#' @param subtitle The subtitle of the resulting map, if desired.
#' @param fill The fill color for the clusters. Defaults to light gray.
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_brewer
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom magrittr `%>%`
#' @importFrom sf read_sf
#' @export

view_map <- function(title = NULL,
                     subtitle = NULL,
                     fill = "lightgray"
){
  # Definitions
  `%>%` <- magrittr::`%>%`

  # Get shapefiles
  county <- sf::read_sf(.cluster$cfg$shape_county) %>%
    dplyr::filter(GEOID %in% as.character(.cluster$cfg$county))
  blocks <- sf::read_sf(.cluster$cfg$shape_block) %>%
    dplyr::filter(
      STATEFP20 == as.character(.cluster$cfg$state) &
        COUNTYFP20 %in% substr(as.character(.cluster$cfg$county), 3, 5)
    )

  # Get geoids and merge
  geoids <- readr::read_delim(.cluster$cfg$geoids, show_col_types = F)
  clusters <- geoids %>%
    dplyr::mutate(geoid = as.character(geoid)) %>%
    dplyr::left_join(
      dplyr::select(blocks, geoid = GEOID20, lat = INTPTLAT20, long = INTPTLON20,
             ur = UR20, geometry),
      by = "geoid"
    )

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
      ggplot2::aes(geometry = geometry, color = ur),
      data = clusters,
      fill = fill,
      linewidth = 1.05,
      inherit.aes = F
    ) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = cluster, geometry = geometry),
      data = clusters,
      size = 2.5
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      color = "Rural/Urban"
    ) +
    ggplot2::scale_color_brewer(palette = "Accent") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  # Return
  return(plot_map)
}
