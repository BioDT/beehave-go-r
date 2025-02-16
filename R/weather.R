#' Add weather data to Beehave experiment
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param weather_vector A numeric vector of length 365 containing daily sunshine duration values in hours.
#'        Values should typically range from 0 (no sunshine) to 24 (maximum sunshine).
#'        This vector represents the daily foraging time available to bees.
#'
#' @return Returns the modified Beehave experiment list with added weather data
#' @export
#'
#' @details
#' The weather vector represents daily sunshine duration or potential foraging time for bees.
#' Each value in the vector corresponds to one day of the year (365 days total).
#' Values should be in hours and typically range from 0 to 24.
#' Zero indicates no foraging possible that day (e.g., rain or cold weather),
#' while higher values indicate more time available for foraging.
#'
#' @examples
#' # Create an empty experiment
#' experiment <- beehave_init()
#'
#' # Create a weather vector with mostly no foraging (0 hours)
#' # but 10 random days with 20 hours of foraging time
#' weather_vector <- rep(0, 365)
#' weather_vector[sample(1:365, 10)] <- 20
#'
#' # Add weather data to the experiment
#' experiment <- add_weather_vector(experiment, weather_vector)
#'
#' # Create a more realistic weather pattern with seasonal variation
#' days <- 1:365
#' weather_vector <- 8 + 6 * sin((days - 172) * 2 * pi / 365)  # Peak in summer
#' experiment <- add_weather_vector(experiment, weather_vector)
#'
add_weather_vector <- function(
    experiment,
    weather_vector = NULL) {

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
#' @importFrom readr read_file
#' @importFrom stringr str_split
#'
#' @examples
#' # Create an empty experiment
#' /dontrun{
#' experiment <- beehave_init()
#'
#' # Add weather to the experiment
#' experiment <- add_weather_file(experiment, "data/weather.csv")
#' print(experiment)
#' }
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
      as.numeric() |>
      # as.integer() |>
      na.omit()
  }

  weather_length <- length(weather_vector)
  weather_ind <- 1:(floor(weather_length / 365) * 365)
  weather_input <- list(Years = matrix(weather_vector[weather_ind], nrow = 1))

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
#' \dontrun{
#' experiment <- beehave_init()
#' experiment <- add_weather_location(
#'   experiment,
#'   start_date = "2016-01-01",
#'   days = 365,
#'   location = c("52.5200,13.4050"))
#' print(experiment)
#' }
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
