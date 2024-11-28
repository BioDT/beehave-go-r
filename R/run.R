#' Print Beehave experiment settings
#'
#' @useDynLib beehave.go.r gobeecs
#' @param experiment a beehave experiment created by beehave_init()
#'
#' @return Return 0
#' @export
#'
#' @examples
run_simulation <- function(
  experiment
  ){
  res <- .Call(
    "gobeecs",
    experiment,
    PACKAGE = "beehave.go.r"
  )

  return(0)
}
