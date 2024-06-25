#' Initialize Beehave experiment
#'
#' @param params
#'
#' @return a list of parameters for Beehave experiment
#' @export
#'
#' @examples
beehave_init <- function(params = NULL){
  if (is.null(params)) {
    out <- list()
  } else {
    stopifnot(
      is.list(params)
    )
    out <- params
  }

  class(out) <- "beehave.experiment"
  return(out)
}
