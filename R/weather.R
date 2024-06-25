add_weather <- function(experiment,
                        start_date = NULL,
                        days = NULL,
                        location = NULL,
                        source = c("rsds"),
                        weather_vector = NULL,
                        weather_file = NULL,
                        weather_file_type = "beehave_legacy") {
  if (!is.null(start_date) |
      is.numeric(days) |
      !is.null(location) |
      is.string(source)) {

    # Add checks

    # Add result to experiment
    weather_vector <- weather_rdwd(location,
                        from_date = start_date,
                        to_date = start_date + days)
  }

  if (!is.null(weather_vector)) {
    # Add checks
  }

  if (!is.null(weather_file)) {
    if (weather_file_type == "beehave_legacy") {
      # Load weather data ----
      weather_vector <- read_file(weather_file) |>
        str_split(" ",
                  simplify = TRUE
        ) |>
        as.integer() |>
        na.omit()
    }
  }

  experiment["ForagingPeriod"] <- weather_vector

  return(experiment)
}
