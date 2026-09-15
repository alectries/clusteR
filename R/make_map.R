#' Generate a customizable map of clusters
#'
#' Produces a map of your clusters with layers as requested. For a simpler
#' function, use `view_map`.
#'
#' # Layers
#'
#' Additional layers, such as county subdivisions, block groups, and roads, can
#' be added to maps using their [`tigris`][tigris::tigris] functions. Layers
#' are added using a named vector, like `c(function_name = "color")`. Do not use
#' parentheses with the function name. Examples of layers include:
#'
#' - [tigris::area_water]
#' - [tigris::block_groups]
#' - [tigris::coastline]
#' - [tigris::combined_statistical_areas]
#' - [tigris::congressional_districts]
#' - [tigris::core_based_statistical_areas]
#' - [tigris::county_subdivisions]
#' - [tigris::divisions]
#' - [tigris::landmarks]
#' - [tigris::linear_water]
#' - [tigris::metro_divisions]
#' - [tigris::military]
#' - [tigris::native_areas]
#' - [tigris::new_england]
#' - [tigris::places]
#' - [tigris::primary_roads]
#' - [tigris::primary_secondary_roads]
#' - [tigris::pumas]
#' - [tigris::rails]
#' - [tigris::regions]
#' - [tigris::roads]
#' - [tigris::school_districts]
#' - [tigris::state_legislative_districts]
#' - [tigris::tracts]
#' - [tigris::tribal_block_groups]
#' - [tigris::tribal_census_tracts]
#' - [tigris::tribal_subdivisions_national]
#' - [tigris::urban_areas]
#' - [tigris::voting_districts]
#' - [tigris::zctas]
#'
#' These layers will be automatically trimmed to fit within the boundaries of
#' your configured county/ies.
#'
#' @param x Data to dictate cluster fill or border color. Must be a dataframe with column `cluster` matching cluster identifiers. Defaults to NA, which will leave clusters empty.
#' @param ... `ggplot2` layers to add to the map. For example, use `scale_fill_brewer` to set cluster fill color.
#' @param layers A named vector of tigris layers and border colors; see Layers.
#' @param fill The column of x (or the Census block shapefile) to fill by, passed to [ggplot2::aes]. *Not* a fill name or scheme function.
#' @param color The column of x (or the Census block shapefile) to color borders by, passed to [ggplot2::aes]. *Not* a color name or scheme function.
#' @param title The title of your map, if desired.
#' @param subtitle The subtitle of your map, if desired.
#' @param background The background color for your map. Defaults to white.
#' @importFrom cli style_bold
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr join_by
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 geom_sf_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom magrittr `%>%`
#' @importFrom purrr imap
#' @importFrom purrr reduce
#' @importFrom readr read_csv
#' @importFrom rlang abort
#' @importFrom rlang list2
#' @importFrom sf st_intersection
#' @importFrom sf st_intersects
#' @importFrom sf st_filter
#' @importFrom sf st_union
#' @importFrom tidyselect everything
#' @importFrom tidyselect starts_with
#' @importFrom tigris blocks
#' @importFrom tigris counties
#' @export

make_map <- function(x = NA,
                     ...,
                     layers = c(),
                     fill = NULL,
                     color = NULL,
                     title = NULL,
                     subtitle = NULL,
                     background = "white"
){
  # Definitions
  `%>%` <- magrittr::`%>%`

  # Load shapefiles
  geoids <- readr::read_csv(.cluster$cfg$geoids, col_types = "cc", show_col_types = F)
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
                  COUNTYFP = tidyselect::starts_with("COUNTYFP"),
                  GEOID = tidyselect::starts_with("GEOID") &
                    !tidyselect::starts_with("GEOIDFQ")) %>%
    dplyr::filter(
      STATEFP == .cluster$cfg$state & COUNTYFP %in% .cluster$cfg$county
    ) %>%
    dplyr::right_join(
      geoids,
      by = dplyr::join_by(GEOID == geoid)
    )

  # Check and merge data
  if("data.frame" %in% class(x)){
    if(!("cluster" %in% names(x))){
      rlang::abort(message = c(
        cli::style_bold("No clusters in x!"),
        "!" = "Your dataset to map must include a cluster column."
      ))
    } else {
      blocks <- dplyr::inner_join(
        blocks,
        x,
        by = "cluster"
      )
    }
  } else {
    if(F %in% (class(x) == "logical")){
      rlang::abort(message = c(
        cli::style_bold("x is not a dataframe!"),
        "!" = paste0("x is a ", class(x), ", not a dataframe."),
        "i" = "If x was intended as a ... argument, add x = NA to your call."
      ))
    }
  }

  # Create template map
  plot_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = county,
      color = "lightblue4",
      linewidth = 1,
      fill = NA
    ) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = {{fill}}, color = {{color}}),
      data = blocks,
      linewidth = 1.05,
      inherit.aes = F
    ) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = cluster),
      data = blocks,
      size = 2.5,
      inherit.aes = F
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_void(paper = background) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  # Build tigris layers
  targs <- list(
    year = .cluster$cfg$year,
    state = .cluster$cfg$state,
    county = .cluster$cfg$county
  )
  tigris <- purrr::imap(
    layers,
    \(color, f){
      ggplot2::geom_sf(
        data = sf::st_intersection(
          sf::st_filter(
            do.call(
              getExportedValue("tigris", f),
              args = targs[
                names(targs) %in% formalArgs(getExportedValue("tigris", f))
              ]
            ),
            county,
            .predicate = sf::st_intersects
          ),
          sf::st_union(county)
        ),
        color = color,
        fill = NA,
        alpha = 0.25,
        linewidth = 1,
        inherit.aes = F
      )
    }
  )

  # Build manual layers
  manual <- list2(...)

  # Add layers
  plot_map <- purrr::reduce(
    c(tigris, manual),
    `+`,
    .init = plot_map
  )

  # Return
  return(plot_map)
}
