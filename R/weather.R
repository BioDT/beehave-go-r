#' Print Beehave experiment settings
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param start_date a start day of simulation
#' @param days how many will simulation take
#' @param location lat/lon location of the simulation
#' @param source what should be the data source, default is rdwd
#' @param weather_vector collection hours defined as vector
#' @param weather_file filepath to the collection hours
#' @param weather_file_type what is the filetype of the collection hours file, default beehave_legacy, which means files used in original Netlogo version of Beehave
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_weather <- function(experiment,
                        start_date = NULL,
                        days = NULL,
                        location = NULL,
                        source = c("rdwd"),
                        weather_vector = NULL,
                        weather_file = NULL,
                        weather_file_type = "beehave_legacy") {
  if (!is.null(start_date) &
      is.numeric(days) &
      !is.null(location) &
      is.character(source)
     ) {

    # Add checks

    # Add result to experiment
    weather_input <- weather_rdwd(location,
                        from_date = start_date,
                        to_date = start_date + days)
  }

  if (!is.null(weather_vector)) {
    # Add checks

    weather_input <- list(Years = weather_vector)
  }

  if (!is.null(weather_file)) {
    if (weather_file_type == "beehave_legacy") {
      # Load weather data ----
      weather_input <- readr::read_file(weather_file) |>
        stringr::str_split(" ",
                  simplify = TRUE
        ) |>
        as.integer() |>
        na.omit()
    }
  }

  experiment[["ForagingPeriod"]] <- weather_input

  return(experiment)
}
