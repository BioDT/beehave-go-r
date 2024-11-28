#' Print Beehave experiment settings
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param subset subset of parameters to print
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
print.beehave.experiment <- function(
    experiment,
    subset = NULL) {

    stopifnot( "beehave.experiment" %in% class(experiment) )

    if (is.null(subset)){
    } else {
        experiment <- experiment[subset]
    }

}
