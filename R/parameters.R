#' Add parameters to the Beehave experiment
#'
#' @param experiment
#' @param params
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_parameter <- function(experiment, params){
  stopifnot(
    is.list(params),
    class(experiment) == "beehave.experiment"
  )

  experiment[names(params)] <- params
  return(experiment)
}
