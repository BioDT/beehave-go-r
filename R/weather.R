#' Add collecting days Beehave experiment settings
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param weather_vector collection hours defined as vector
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_weather_vector <- function(
    experiment,
    weather_vector = NULL) {
  # Add checks

  weather_input <- list(Years = weather_vector)

  experiment[["ForagingPeriod"]] <- weather_input

  return(experiment)
}

#' Add collecting days Beehave experiment settings
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param weather_file filepath to the collection hours
#' @param weather_file_type what is the filetype of the collection hours file, default beehave_legacy, which means files used in original Netlogo version of Beehave
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_weather_file <- function(
    experiment,
    weather_file,
    weather_file_type = "beehave_legacy") {
  if (weather_file_type == "beehave_legacy") {
    # Load weather data ----
    weather_vector <- readr::read_file(weather_file) |>
      stringr::str_split(" ",
        simplify = TRUE
      ) |>
      as.integer() |>
      na.omit()
  }

  weather_input <- list(Years = weather_vector)

  experiment[["ForagingPeriod"]] <- weather_input

  return(experiment)
}



#' Add collecting days Beehave experiment settings
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param start_date a start day of simulation
#' @param days how many will simulation take
#' @param location lat/lon location of the simulation
#' @param source what should be the data source, default is rdwd
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_weather_location <- function(
    experiment,
    start_date = "2016-01-01",
    days = 365,
    location,
    source = "rdwd") {
  if (
    !is.null(start_date) &&
      is.numeric(days) &&
      !is.null(location) &&
      is.character(source)
  ) {
    # Add result to experiment
    weather_vector <- weather_rdwd(
      location,
      from_date = start_date,
      to_date = as.Date(start_date) + days
    )
  }

  weather_input <- list(Years = weather_vector)

  experiment[["ForagingPeriod"]] <- weather_input
}
