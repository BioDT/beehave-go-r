#' Run simulation
#'
#' @useDynLib beehave.go.r gobeecs
#' @param experiment a beehave experiment created by beehave_init()
#'
#' @return Return 0
#' @export
#'
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#' # Create experiment with specified parameters
#' experiment <- beehave_init(params = list(InitialPopulation = list(Count = 50000)))
#' run_simulation(experiment)
#' }
run_simulation <- function(
    experiment) {
  experiment <- experiment |>
    jsonlite::toJSON(
      auto_unbox = TRUE,
      pretty = TRUE
    ) #|> as.character()

  res <- .Call(
    "gobeecs",
    experiment,
    PACKAGE = "beehave.go.r"
  )

  return(0)
}
