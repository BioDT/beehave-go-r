#' Add flower patches to the Beehave experiment
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param landuse_map a path to the landuse map tif
#' @param locations set of locations for experiments in lat/lon format
#' @param lookup_table a lookup table for the flower patches
#' @param lookup_file a path to the lookup table file
#' @param lookup_file_type format of a lookup table, default is "csv"
#' @param flower_patches_list a list with the definition of flower patches, see details
#' @param type what is the input map or list, default map
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_flower_patches <- function(
    experiment,
    landuse_map = NULL,
    locations = NULL,
    lookup_table = NULL,
    lookup_file = NULL,
    lookup_file_type = "csv",
    flower_patches_list = NULL,
    type = c("map", "list")) {
  stopifnot("beehave.experiment" %in% class(experiment))

  if (type[1] == "map") {
    if (is.null(lookup_table) &&
      is.null(lookup_file)) {
      stop("You must provide at least one of the `lookup_table` or `lookup_file`.")
    }

    if (!is.null(lookup_file) && !is.null(lookup_table)) {
      warning("Both `lookup_file` and `lookup_table` are provided. Only `lookup_table` will be used.")
    }

    if (!is.null(lookup_file)) {
      lookup_table <- read.csv(
        file = file
      )
    }

    if (!is.null(lookup_table)) {
      stopifnot(is.data.frame(lookup_table))
    }
  } else if (type[1] == "list") {
    stopifnot(is.list(flower_patches_list))
  } else {
    stop("Variable `type` must be either: 'map' or 'list'")
  }





  # Keep patches coordinates for visualisation even if not used by Beecs
}
