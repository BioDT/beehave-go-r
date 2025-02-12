###### R - script to create input (resources and weather) files to run the BEEHAVE model
# main contributor Anna Wendt, University of Freiburg
# contributor of an earlier version of the WeatherDataInput() function Okan Özsoy
# modifications have been done by Jürgen Groeneveld, Tomas Martinovic, Tuomas Rossi
# the honeybee pDT has benefited from the work of the honeybee pDT team

# Function to create a weatherinput file for the beehave model
#' @export
weather_rdwd <- function(
    bee_location,
    from_date = "2016-01-01",
    to_date = "2016-12-31") {
  # transform input coordinates to degrees
  TrachtnetConv <- terra::project(bee_location, "epsg:4326")
  Coordinates <- as.data.frame(terra::crds(TrachtnetConv))

  # Read the station data
  WeatherStations <- rdwd::nearbyStations(
    Coordinates$y,
    Coordinates$x,
    radius = 50,
    res = "daily",
    var = "kl",
    per = "historical",
    mindate = to_date
  ) |>
    # dplyr::select only stations that started measuring before 2016
    dplyr::filter(von_datum < from_date)

  # check through the stations for NA values in data
  for (i in seq_along(WeatherStations)) {
    weather_data <-
      rdwd::dataDWD(WeatherStations$url[i],
        varnames = TRUE,
        quiet = TRUE
      ) |>
      dplyr::select(
        MESS_DATUM,
        SDK.Sonnenscheindauer,
        TXK.Lufttemperatur_Max
      ) |>
      # dplyr::mutate(MESS_DATUM = rdwd::as_date(MESS_DATUM)) |>
      dplyr::filter(
        MESS_DATUM >= as.POSIXct(from_date, tz = "GMT"),
        MESS_DATUM <= as.POSIXct(to_date, tz = "GMT")
      )

    # breaks when file with no NAs in SDK found
    if (anyNA(weather_data$SDK.Sonnenscheindauer) == FALSE &&
      length(weather_data$SDK.Sonnenscheindauer) > 0) {
      break
    }

    # if all stations contain NA values give warning
    if (i == length(WeatherStations$Stations_id)) {
      warning(
        paste(
          "Final dplyr::selected weather station includes NA values. No stations found without any NA within 50km distance. Station ID:",
          WeatherStations$Stations_id[i]
        )
      )
    }
  }

  # Add station id and day number
  weather_data <- weather_data |>
    dplyr::rename(
      Date = MESS_DATUM,
      T_max = TXK.Lufttemperatur_Max,
      Sun_hours = SDK.Sonnenscheindauer
    ) |>
    dplyr::mutate(
      Station_id = WeatherStations$Stations_id[i],
      Day = seq_along(dplyr::n()),
      .before = Date
    ) |>
    # Use only sun hours where max temperature is above 15 degrees celsium
    dplyr::mutate(Sun_hours = ifelse(T_max < 15, 0, Sun_hours))

  return(weather_data$Sun_hours)
}
