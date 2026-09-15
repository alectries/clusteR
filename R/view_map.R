#' Generate a map of clusters
#'
#' Produces a simple, labeled map of your clusters. For a more complex map, use
#' `make_map`.
#'
#' @param title The title of the resulting map, if desired.
#' @param subtitle The subtitle of the resulting map, if desired.
#' @param fill The fill color for the clusters. Defaults to light gray.
#' @param background The background color for the map. Defaults to white.
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
#' @importFrom tidyselect everything
#' @importFrom tidyselect matches
#' @importFrom tidyselect starts_with
#' @importFrom tigris blocks
#' @importFrom tigris counties
#' @export

view_map <- function(title = NULL,
                     subtitle = NULL,
                     background = "white"
){
  # Definitions
  `%>%` <- magrittr::`%>%`

  # Get shapefiles
  county <- tigris::counties(
    state = .cluster$cfg$state,
    year = .cluster$cfg$year
  ) %>%
    dplyr::filter(GEOID %in% paste0(.cluster$cfg$state, .cluster$cfg$county))
  blocks <- tigris::blocks(
    state = .cluster$cfg$state,
    county = .cluster$cfg$county,
    year = .cluster$cfg$year
  ) %>%
    dplyr::select(tidyselect::everything(),
                  STATEFP = tidyselect::starts_with("STATEFP"),
                  COUNTYFP = tidyselect::starts_with("COUNTYFP")) %>%
    dplyr::filter(
      STATEFP == .cluster$cfg$state &
        COUNTYFP %in% .cluster$cfg$county
    )

  # Get geoids and merge
  geoids <- readr::read_delim(.cluster$cfg$geoids, show_col_types = F)
  clusters <- geoids %>%
    dplyr::mutate(geoid = as.character(geoid)) %>%
    dplyr::left_join(
      dplyr::select(blocks, geoid = tidyselect::matches("^GEOID[0-9]+$"),
                    lat = tidyselect::starts_with("INTPTLAT"),
                    long = tidyselect::starts_with("INTPTLON"),
                    ur = tidyselect::starts_with("UR"), geometry),
      by = "geoid"
    )

  # Map
  plot_map <- clusteR::make_map(
    NA,
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ),
    ggplot2::scale_color_brewer(palette = "Accent"),
    ggplot2::theme_void(paper = background)
  )

  # Return
  return(plot_map)
}
