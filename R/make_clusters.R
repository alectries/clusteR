#' Randomly select census blocks and assign cluster identifiers
#'
#' Using U.S. Census data, randomly selects Census blocks, weighted by
#' population, and assign numeric cluster identifiers to them.
#'
#' Unlike most clusteR functions, `make_clusters` can be run before `setup`
#' because you need to identify clusters before you can identify households
#' to survey.
#'
#' @param n The number of clusters to select.
#' @param state The state where clusters will be selected. Defaults to the state in the config file; must be specified if running before `setup`.
#' @param county The county where clusters will be selected. Defaults to the county in the config file; must be specified if running before `setup`.
#' @importFrom cli style_bold
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @importFrom tidycensus get_decennial
#' @export

make_clusters <- function(n,
                          state = .cluster$cfg$state,
                          county = .cluster$cfg$county
){
  # Check if state and county exist
  if(is.null(state) && is.null(county)){
    rlang::abort(message = c(
      cli::style_bold("Geography not specified!"),
      "i" = "Specify state and county."
    ))
  }

  # If county is from config, modify it
  if(exists(".cluster") && exists("cfg", where = .cluster) &&
     exists("county", where = .cluster$cfg) &&
     !(F %in% (.cluster$cfg$county == county))){
    county <- substr(county, 3, 5)
  }

  # Get population data
  dec <- tidycensus::get_decennial(
    "block",
    "P1_001N",
    year = 2020,
    state = state,
    county = county
  )

  # Sample
  geoid <- sample(dec$GEOID, size = n, prob = dec$value)

  # Create geoids file
  out <- tibble::tibble(
    geoid = geoid,
    cluster = 1:n
  )
  dir.create("Cohort", showWarnings = F)
  readr::write_csv(out, "Cohort/geoids.csv")

  # Save filepath to environment if it exists
  if(exists(".cluster") && exists("cfg", where = .cluster)){
    .cluster$cfg$geoids <- "Cohort/geoids.csv"
    saveRDS(.cluster$cfg, "Scripts/config.rds")
    rlang::inform(message = c(
      cli::style_bold("Clusters saved."),
      "v" = paste0("Saved to ", cli::style_underline("Cohort/geoids.csv"), "."),
      "i" = "The geoids file has been saved to your clusteR configuration."
    ))
  } else {
    rlang::inform(message = c(
      cli::style_bold("Clusters saved."),
      "v" = paste0("Saved to ", cli::style_underline("Cohort/geoids.csv"), "."),
      "!" = "This clusteR environment is not configured. Manually add geoids in setup()."
    ))
  }

  # Return file path
  return(invisible("Cohort/geoids.csv"))
}
