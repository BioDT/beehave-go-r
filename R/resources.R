add_flower_patches <- function(experiment,
                               landuse_map,
                               locations,
                               lookup_table = NULL,
                               lookup_list = NULL,
                               lookup_file = NULL,
                               lookup_file_type = "csv") {
  stopifnot(is.raster(landuse_map),
            class(experiment) == "beehave.experiment")

  if (is.null(lookup_table) &
      is.null(lookup_list) &
      is.null(file)) {
    stop("You must provide at least one of the `lookup_table`, `lookup_list` or `lookup_file`.")
  }

  if (!is.null(file)) {
    lookup_table <- read.csv(
      file = file
    )
  }

  if (!is.null(lookup_table)) {
    stopifnot(is.data.frame(lookup_table))
  }

  if (!is.null(lookup_list)) {
    stopifnot(is.list(lookup_list))
  }

  # Keep patches coordinates for visualisation even if not used by Beecs
}
