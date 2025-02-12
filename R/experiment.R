#' Initialize Beehave experiment
#'
#' @param params list of initial parameters
#' @param add_default logical, if TRUE, add default parameters
#'
#' @return a list of parameters for Beehave experiment
#' @export
#'
#' @examples
#' # Create an empty experiment
#' experiment <- beehave_init()
#'
#' # Create experiment with default parameters
#' experiment <- beehave_init(add_default = TRUE)
#' print(experiment)
#'
#' # Create experiment with specified parameters
#' experiment <- beehave_init(params = list(InitialPopulation = list(Count = 50000)))
#' print(experiment)
#'
#' # Create experiment with default parameters and specified parameters
#' experiment <- beehave_init(params = list(InitialPopulation = list(Count = 50000)), add_default = TRUE)
#' print(experiment)
#'
beehave_init <- function(
    params = NULL,
    add_default = FALSE) {
  out <- list()

  if (add_default) {
    out <- add_parameter(out, get_default_params())
  }

  if (!is.null(params)) {
    stopifnot(
      is.list(params)
    )
    out <- params
  }

  class(out) <- c("beehave.experiment", "list")
  return(out)
}
