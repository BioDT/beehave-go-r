#' Plot Flower Patches from Beehave Experiment
#'
#' Creates a map visualization of flower patches used in a Beehave experiment.
#' The function plots all patch polygons and the colony location.
#'
#' @param experiment A beehave experiment containing flower patches
#' @param interactive Logical, whether to create an interactive map (default TRUE)
#' @param colors Named vector of colors for different patch types
#'
#' @return A plot object (either a tmap object or a ggplot object depending on interactive parameter)
#' @export
#'
#' @examples
#' \dontrun{
#' # Create an experiment with flower patches
#' experiment <- beehave_init() |>
#'   add_flower_patches_from_map(
#'     landuse_map = "path/to/landuse.tif",
#'     lookup_table = "path/to/lookup.csv",
#'     location = data.frame(lat = 48.0, lon = 7.8)
#'   )
#'
#' # Plot the patches
#' plot_flower_patches(experiment)
#' }
#'
#' @importFrom terra vect project crs
#' @importFrom tmap tm_shape tm_polygons tm_dots tm_compass tm_scale tm_layout tmap_mode
#' @importFrom sf st_as_sf
plot_flower_patches <- function(
  experiment,
  interactive = TRUE,
  colors = NULL
) {
  # Check if experiment has flower patches
  if (
    is.null(experiment$InitialPatches) ||
      is.null(experiment$InitialPatches$Patches) ||
      length(experiment$InitialPatches$Patches) == 0
  ) {
    stop("No flower patches found in the experiment")
  }

  patches <- experiment$InitialPatches$Patches

  # Extract all patch polygons
  patch_polygons <- list()
  patch_types <- character()

  for (i in seq_along(patches)) {
    if (!is.null(patches[[i]]$PatchPolygon)) {
      patch_polygons[[i]] <- patches[[i]]$PatchPolygon
      patch_types[i] <- patches[[i]]$PatchType
    }
  }

  # Filter out NULL entries
  valid_indices <- !sapply(patch_polygons, is.null)
  patch_polygons <- patch_polygons[valid_indices]
  patch_types <- patch_types[valid_indices]

  if (length(patch_polygons) == 0) {
    stop("No valid patch polygons found in the experiment")
  }

  # Combine all polygons into one SpatVector with a type attribute
  all_polygons <- do.call(rbind, patch_polygons)
  terra::values(all_polygons) <- data.frame(PatchType = patch_types)

  # Get colony location from the first patch's distance calculation
  # We need to reconstruct this from the coordinates and distance
  colony_x <- NULL
  colony_y <- NULL

  # Try to get colony location from experiment if available
  if (!is.null(experiment$Colony) && !is.null(experiment$Colony$Location)) {
    colony_x <- experiment$Colony$Location$X
    colony_y <- experiment$Colony$Location$Y
  } else {
    # Fallback: try to estimate from the first patch
    first_patch <- patches[[1]]
    if (!is.null(first_patch$Coords) && !is.null(first_patch$DistToColony)) {
      # This is a rough estimate and may not be accurate
      angle <- runif(1, 0, 2 * pi) # Random angle as we don't know the direction
      colony_x <- first_patch$Coords$X - cos(angle) * first_patch$DistToColony
      colony_y <- first_patch$Coords$Y - sin(angle) * first_patch$DistToColony
    }
  }

  # Create colony point if we have coordinates
  colony_point <- NULL
  if (!is.null(colony_x) && !is.null(colony_y)) {
    colony_point <- terra::vect(
      cbind(colony_x, colony_y),
      crs = terra::crs(all_polygons)
    )
  }

  # Set default colors if not provided
  if (is.null(colors)) {
    unique_types <- unique(patch_types)
    default_colors <- c(
      "#2A6EBBFF",
      "#F0AB00FF",
      "#C50084FF",
      "#7D5CC6FF",
      "#E37222FF",
      "#69BE28FF",
      "#00B2A9FF",
      "#CD202CFF",
      "#747678FF"
    )
    colors <- setNames(
      default_colors[seq_len(min(
        length(unique_types),
        length(default_colors)
      ))],
      unique_types
    )
  }

  # Convert to sf for plotting
  all_polygons_sf <- sf::st_as_sf(all_polygons)

  # Set tmap mode
  if (interactive) {
    tmap::tmap_mode("view")
  } else {
    tmap::tmap_mode("plot")
  }

  # Create the map
  tm <- tmap::tm_shape(all_polygons_sf) +
    tmap::tm_polygons(col = "PatchType", palette = colors, title = "Patch Type")

  # Add colony location if available
  if (!is.null(colony_point)) {
    colony_sf <- sf::st_as_sf(colony_point)
    tm <- tm +
      tmap::tm_shape(colony_sf) +
      tmap::tm_dots(col = "black", size = 0.5, shape = 23, title = "Colony")
  }

  # Add map elements
  tm <- tm +
    tmap::tm_compass() +
    tmap::tm_scale_bar() +
    tmap::tm_layout(
      title = "Flower Patches",
      legend.position = c("right", "bottom")
    )

  return(tm)
}

#' Plot Flower Patches from Direct Patch List
#'
#' Creates a map visualization of flower patches from a list returned by flower_patches_from_map.
#'
#' @param flower_patches A list of flower patches with PatchPolygon elements
#' @param colony_location Optional SpatVector with colony location
#' @param interactive Logical, whether to create an interactive map (default TRUE)
#' @param colors Named vector of colors for different patch types
#'
#' @return A tmap object
#' @export
#'
#' @importFrom terra vect project crs values
#' @importFrom tmap tm_shape tm_polygons tm_dots tm_compass tm_scale tm_layout tmap_mode
#' @importFrom sf st_as_sf
plot_patches <- function(
  flower_patches,
  colony_location = NULL,
  interactive = TRUE,
  colors = NULL
) {
  # Check if we have patches
  if (length(flower_patches) == 0) {
    stop("No flower patches provided")
  }

  # Extract all patch polygons
  patch_polygons <- list()
  patch_types <- character()

  for (i in seq_along(flower_patches)) {
    if (!is.null(flower_patches[[i]]$PatchPolygon)) {
      patch_polygons[[i]] <- flower_patches[[i]]$PatchPolygon
      patch_types[i] <- flower_patches[[i]]$PatchType
    }
  }

  # Filter out NULL entries
  valid_indices <- !sapply(patch_polygons, is.null)
  patch_polygons <- patch_polygons[valid_indices]
  patch_types <- patch_types[valid_indices]

  if (length(patch_polygons) == 0) {
    stop("No valid patch polygons found")
  }

  # Combine all polygons into one SpatVector with a type attribute
  all_polygons <- do.call(rbind, patch_polygons)
  terra::values(all_polygons) <- data.frame(PatchType = patch_types)

  # Set default colors if not provided
  if (is.null(colors)) {
    unique_types <- unique(patch_types)
    default_colors <- c(
      "#2A6EBBFF",
      "#F0AB00FF",
      "#C50084FF",
      "#7D5CC6FF",
      "#E37222FF",
      "#69BE28FF",
      "#00B2A9FF",
      "#CD202CFF",
      "#747678FF"
    )
    colors <- setNames(
      default_colors[seq_len(min(
        length(unique_types),
        length(default_colors)
      ))],
      unique_types
    )
  }

  # Convert to sf for plotting
  all_polygons_sf <- sf::st_as_sf(all_polygons)

  # Set tmap mode
  if (interactive) {
    tmap::tmap_mode("view")
  } else {
    tmap::tmap_mode("plot")
  }

  # Create the map
  tm <- tmap::tm_shape(all_polygons_sf) +
    tmap::tm_polygons(col = "PatchType", palette = colors, title = "Patch Type")

  # Add colony location if available
  if (!is.null(colony_location)) {
    colony_sf <- sf::st_as_sf(colony_location)
    tm <- tm +
      tmap::tm_shape(colony_sf) +
      tmap::tm_dots(col = "black", size = 0.5, shape = 23, title = "Colony")
  }

  # Add map elements
  tm <- tm +
    tmap::tm_compass() +
    tmap::tm_scale_bar() +
    tmap::tm_layout(
      title = "Flower Patches",
      legend.position = c("right", "bottom")
    )

  return(tm)
}
