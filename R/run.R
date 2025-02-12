#' Run simulation
#'
#' @useDynLib beehave.go.r run_beecs
#' @param experiment a beehave experiment created by beehave_init()
#'
#' @return A list containing worker cohorts data for each timestep
#' @export
#'
#' @importFrom jsonlite toJSON fromJSON
#'
#' @examples
#' \dontrun{
#' # Create experiment with specified parameters
#' experiment <- beehave_init(params = list(InitialPopulation = list(Count = 50000)))
#' results <- run_simulation(experiment)
#' }
run_simulation <- function(experiment) {
  # Convert parameters to JSON
  tryCatch({
    params_json <- jsonlite::toJSON(
      experiment,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"  # Ensure proper handling of NULL values
    )
  }, error = function(e) {
    stop("Failed to convert parameters to JSON: ", e$message)
  })

  # Run simulation and get JSON data
  json_data <- .Call(
    "run_beecs",
    params_json,
    PACKAGE = "beehave.go.r"
  )

  if (is.null(json_data)) {
    stop("Simulation failed")
  }

  # Parse JSON into R list
  tryCatch({
    jsonlite::fromJSON(
      json_data,
      simplifyVector = TRUE,  # Convert arrays to matrices where possible
      simplifyDataFrame = FALSE  # Don't convert to data frames
    )
  }, error = function(e) {
    stop("Failed to parse simulation results: ", e$message)
  })
}
