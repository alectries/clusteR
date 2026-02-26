#' Run multiple iterations of kmeans on cluster geography
#'
#' Not exported.
#'
#' @param include A vector of cluster identifiers to include in the map.
#' @param geoids A table matching cluster identifiers to Census GEOIDs.
#' @param shape_county The path to the county shapefile.
#' @param shape_block The path to the Census block shapefile.
#' @param k The number of groups to create.
#' @param runs The number of kmeans runs to complete.
#' @param iter.max The number of loops each kmeans run should complete.
#' @importFrom cli style_bold
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr `%>%`
#' @importFrom readr read_delim
#' @importFrom rlang inform
#' @importFrom sf read_sf
#' @importFrom tibble tibble_row
#' @keywords internal

mult_kmeans <- function(include, geoids, shape_county, shape_block, k, runs, iter.max){
  # Definitions
  `%>%` <- magrittr::`%>%`

  # Get clusters and geographies
  geoids <- readr::read_delim(geoids, show_col_types = F)
  blocks <- sf::read_sf(shape_block) %>%
    dplyr::filter(
      STATEFP20 == as.character(.cluster$cfg$state) &
        COUNTYFP20 %in% substr(as.character(.cluster$cfg$county), 3, 5)
    )

  # Merge clusters with geographies
  clusters <- geoids %>%
    dplyr::mutate(geoid = as.character(geoid)) %>%
    dplyr::left_join(
      dplyr::select(blocks, geoid = GEOID20, lat = INTPTLAT20, long = INTPTLON20,
             ur = UR20, geometry),
      by = "geoid"
    ) %>%
    dplyr::filter(cluster %in% include)

  # Run kmeans
  kruns <- list()
  kstats <- list()
  for(i in 1:runs){
    ## Try kmeans
    tryCatch(
      {
        kruns[[i]] <- kmeans(
          select(clusters, lat, long),
          centers = k,
          iter.max = iter.max,
          algorithm = "MacQueen"
        )
      },
      error = function(e){
        rlang::inform(message = c(
          cli::style_bold("Kmeans algorithm failed."),
          "*" = paste0("Run ", i, " of ", runs, " failed."),
          "x" = paste0(e),
          "i" = "mult_kmeans still running..."
        ))
        kruns[[i]] <- list(cluster = NA)
      }
    )

    ## Log stats
    kstats[[i]] <- tibble::tibble_row(
      id = i,
      min = min(table(kruns[[i]]$cluster), na.rm = T),
      max = max(table(kruns[[i]]$cluster), na.rm = T),
      range = max - min,
      sd = sd(table(kruns[[i]]$cluster), na.rm = T)
    )
  }
  kstats <- dplyr::bind_rows(kstats)

  # Select kmeans iteration with most even groups and assign
  best <- dplyr::filter(kstats, sd == min(sd, na.rm = T))$id[[1]]
  assign <- tibble::tibble(
    clusters,
    group = factor(
      x = kruns[[best]]$cluster,
      levels = sort(unique(kruns[[best]]$cluster))
    )
  )

  # Return assignments
  return(assign)
}
