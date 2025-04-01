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
  # Clean patch data by removing extra descriptors not used by Go code
  if (
    !is.null(experiment$InitialPatches) &&
      !is.null(experiment$InitialPatches$Patches)
  ) {
    # Process each patch to remove extra descriptors
    for (i in seq_along(experiment$InitialPatches$Patches)) {
      # Remove PatchType field (used only for visualization)
      experiment$InitialPatches$Patches[[i]]$PatchType <- NULL

      # Remove Coords field but store it for later reference if needed
      if (!is.null(experiment$InitialPatches$Patches[[i]]$Coords)) {
        if (is.null(experiment$Visualization)) {
          experiment$Visualization <- list()
        }
        if (is.null(experiment$Visualization$PatchCoords)) {
          experiment$Visualization$PatchCoords <- list()
        }

        # Store coords in a visualization section for later use
        experiment$Visualization$PatchCoords[[
          i
        ]] <- experiment$InitialPatches$Patches[[i]]$Coords

        # Remove from the main patch data
        experiment$InitialPatches$Patches[[i]]$Coords <- NULL
      }
    }
  }

  # Convert parameters to JSON
  tryCatch(
    {
      params_json <- jsonlite::toJSON(
        experiment,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null" # Ensure proper handling of NULL values
      )
    },
    error = function(e) {
      stop("Failed to convert parameters to JSON: ", e$message)
    }
  )

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
  tryCatch(
    {
      jsonlite::fromJSON(
        json_data,
        simplifyVector = TRUE, # Convert arrays to matrices where possible
        simplifyDataFrame = FALSE # Don't convert to data frames
      )
    },
    error = function(e) {
      stop("Failed to parse simulation results: ", e$message)
    }
  )
}
