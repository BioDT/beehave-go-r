#' Add flower patches to the Beehave experiment
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param flower_patches_list a list with the definition of flower patches, see details
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @details
#' The function supports two ways to add flower patches:
#' 1. Using a landuse map with lookup table ("map" type)
#' 2. Using a direct list of flower patches ("list" type)
#'
#' For the "map" type, you need to provide:
#' - A landuse map (GeoTIFF format)
#' - A lookup table that maps landuse codes to flower patch properties
#' - Location(s) for which to extract the flower patches
#'
#' @examples
#' # Create an empty experiment
#' experiment <- beehave_init()
#'
#' add_flower_patches(
#'   experiment,
#'   landuse_map = "path/to/landuse.tif",
#'   locations = data.frame(lat = 48.0, lon = 7.8),
#'   lookup_file = "path/to/lookup.csv"
#' )
#'
#' # Example 2: Using a direct list of flower patches
#' patches <- list(
#'     list(
#'       DistToColony = 1000,
#'       ConstantPatch = list(
#'         Nectar = 5,
#'         Pollen = 1,
#'         NectarConcentration = 1,
#'         DetectionProbability = 0.5
#'       )
#'     ),
#'     list(
#'       DistToColony = 200,
#'       SeasonalPatch = list(
#'         MaxNectar = 20,
#'         MaxPollen = 10,
#'         NectarConcentration = 1.5,
#'         DetectionProbability = 0.2,
#'         SeasonShift = 20
#'       )
#'     )
#'   )
#' add_flower_patches(
#'   experiment,
#'   flower_patches_list = patches,
#'   type = "list"
#' )
#'
add_flower_patches <- function(
  experiment,
  flower_patches_list
) {
  stopifnot("beehave.experiment" %in% class(experiment))

  experiment[["InitialPatches"]][["Patches"]] <- flower_patches_list
  return(experiment)
}


#' Convert landuse map to flower patches
#'
#' @param landuse_map a path to the landuse map tif
#' @param locations set of locations for experiments in lat/lon format
#' @param lookup_table a lookup table for the flower patches, or a path to the lookup table file
#' #'
#' @return Return Beehave experiment list
#' @export
#'
#' @details
#' 1. Using a landuse map with lookup table
#'
#' For the "map" type, you need to provide:
#' - A landuse map (GeoTIFF format)
#' - A lookup table that maps landuse codes to flower patch properties
#' - Location(s) for which to extract the flower patches
#'
#' @examples
#' \dontrun{
#' # Create an empty experiment
#' experiment <- beehave_init()
#'
#' experiment <- experiment |>
#'   add_flower_patches_from_map(
#'     experiment,
#'     landuse_map = "path/to/landuse.tif",
#'     lookup_table = "path/to/lookup.csv",
#'     location = data.frame(lat = 48.0, lon = 7.8)
#'   )
#' }
#'
#' @importFrom terra rast vect project crs cells as.polygons disagg set.values
#' @importFrom terra buffer crop cats values expanse subset centroids crds distance ext mask
#' @importFrom utils read.csv

add_flower_patches_from_map <- function(
  experiment,
  landuse_map,
  lookup_table,
  location,
  buffer_size = 2000,
  polygon_size = 200000
) {
  stopifnot("beehave.experiment" %in% class(experiment))

  experiment[["InitialPatches"]][["Patches"]] <- flower_patches_from_map(
    landuse_map,
    lookup_table,
    location,
    buffer_size,
    polygon_size
  )
  return(experiment)
}


flower_patches_from_map <- function(
  landuse_map,
  lookup_table,
  location,
  buffer_size = 2000,
  polygon_size = 200000
) {
  # Read input map
  input_map <- terra::rast(landuse_map)

  # Convert and project location
  bee_location <- terra::vect(
    location,
    crs = "EPSG:4326"
  ) |>
    terra::project(terra::crs(input_map))

  # Read lookup table
  if (is.character(lookup_table)) {
    lookup_table_df <- utils::read.csv(lookup_table)
  } else {
    lookup_table_df <- lookup_table
  }

  # Extract the categories of interest from the lookup table
  patch_types <- lookup_table_df$PatchType

  # Create buffer around bee location
  clip_buffer <- terra::buffer(
    bee_location,
    width = buffer_size
  )

  # Clip raster to buffer
  location_area <- terra::crop(
    input_map,
    clip_buffer
  )

  # Filter raster to only include categories in the lookup table
  # First, get the mapping of values to categories
  map_categories <- terra::cats(location_area)[[1]]

  # Find values that match our patch types
  valid_values <- map_categories$value[map_categories$category %in% patch_types]

  # If no valid values found, return empty list
  if (length(valid_values) == 0) {
    warning("No matching categories found in the landuse map")
    return(list())
  }

  # Mark non-matching values as NA to exclude them
  non_matching <- unique(terra::values(location_area)) |>
    setdiff(valid_values) |>
    as.numeric()

  terra::set.values(
    location_area,
    unlist(terra::cells(location_area, non_matching)),
    NA
  )

  # Convert raster to polygons and disaggregate
  location_polygons <- terra::as.polygons(location_area) |>
    terra::disagg()

  # Initialize list to hold flower patches
  flower_patches <- list()

  # Process each polygon
  if (length(location_polygons) == 0) {
    warning("No flower patches found in the landuse map")
    return(list())
  }

  # Add initial attributes
  poly_values <- terra::values(location_polygons)
  poly_attrs <- data.frame(
    id = seq_len(nrow(poly_values)),
    PatchType = poly_values,
    size_sqm = terra::expanse(location_polygons)
  )
  terra::values(location_polygons) <- poly_attrs

  # Process polygons by patch type
  for (patch in patch_types) {
    # Get polygons of this type
    type_polys <- terra::subset(
      location_polygons,
      terra::values(location_polygons)$category == patch
    )

    # Skip if none found
    if (nrow(type_polys) == 0) {
      next
    }

    # Process each polygon of this type
    for (i in seq_len(nrow(type_polys))) {
      single_poly <- type_polys[i]
      single_area <- terra::expanse(single_poly)

      if (single_area > polygon_size) {
        # Split large polygons
        sub_polys <- split_polygon(single_poly, polygon_size)

        # Add attributes to subpolygons
        if (length(sub_polys) > 0) {
          n_subpolys <- length(sub_polys)
          sub_vals <- data.frame(
            PatchType = rep(patch, n_subpolys),
            size_sqm = terra::expanse(sub_polys)
          )
          terra::values(sub_polys) <- sub_vals

          # Process each subpolygon
          for (j in 1:n_subpolys) {
            sub_poly <- sub_polys[j]

            # Calculate distance to hive
            dist <- terra::distance(
              terra::centroids(sub_poly),
              bee_location
            )[1]

            # Get coordinates of centroid
            center <- terra::crds(terra::centroids(sub_poly))

            # Get nectar, pollen, and other attributes from lookup table
            patch_info <- lookup_table_df[
              lookup_table_df$PatchType == patch,
            ]

            # Create flower patch entry
            patch_entry <- list(
              DistToColony = dist,
              ConstantPatch = list(
                Nectar = patch_info$quantityNectar_l,
                Pollen = patch_info$quantityPollen_g / 1000,
                NectarConcentration = patch_info$concentration,
                DetectionProbability = 0.2 # values(sub_poly)$size_sqm[1]
              ),
              Coords = list(
                X = center[1],
                Y = center[2]
              ),
              PatchType = patch
            )

            # Add to list
            flower_patches <- c(flower_patches, list(patch_entry))
          }
        }
      } else {
        # Process small polygon directly
        dist <- terra::distance(
          terra::centroids(single_poly),
          bee_location
        )[1]

        center <- terra::crds(terra::centroids(single_poly))

        patch_info <- lookup_table_df[lookup_table_df$PatchType == patch, ]

        patch_entry <- list(
          DistToColony = dist,
          ConstantPatch = list(
            Nectar = patch_info$quantityNectar_l * 1000,
            Pollen = patch_info$quantityPollen_g * 365,
            NectarConcentration = patch_info$concentration,
            DetectionProbability = 0.2 # values(sub_poly)$size_sqm[1]
          ),
          Coords = list(
            X = center[1],
            Y = center[2]
          ),
          PatchType = patch,
          PatchPolygon = single_poly
        )

        flower_patches <- c(flower_patches, list(patch_entry))
      }
    }
  }

  return(flower_patches)
}


# Function to split polygon into grid-based subpolygons
#' Split polygon into grid-based subpolygons
#'
#' @param poly SpatVector of single polygon
#' @param target_size Target size of subpolygons in square meters
#'
#' @return SpatVector of subpolygons
#' @importFrom terra rast ext expanse crs as.polygons mask subset xmax xmin ymax ymin

split_polygon <- function(
  poly,
  target_size
) {
  # Get polygon's extent
  poly_ext <- terra::ext(poly)

  # Calculate grid dimensions based on aspect ratio
  poly_area <- terra::expanse(poly)
  num_cells <- ceiling(poly_area / target_size)

  width <- terra::xmax(poly_ext) - terra::xmin(poly_ext)
  height <- terra::ymax(poly_ext) - terra::ymin(poly_ext)

  aspect <- width / height
  rows <- round(sqrt(num_cells / aspect))
  cols <- round(sqrt(num_cells * aspect))

  # Ensure we have enough cells
  while (rows * cols < num_cells) {
    if (rows <= cols) {
      rows <- rows + 1
    } else {
      cols <- cols + 1
    }
  }

  # Create grid raster
  grid <- terra::rast(
    ext = poly_ext,
    nrows = rows,
    ncols = cols,
    crs = terra::crs(poly)
  )

  # Fill with cell IDs
  terra::values(grid) <- 1:(rows * cols)

  # Mask by original polygon
  grid <- terra::mask(grid, poly)

  # Convert to polygons
  grid_polys <- terra::as.polygons(grid)

  # Remove NAs (cells outside the original polygon)
  grid_polys <- terra::subset(grid_polys, !is.na(terra::values(grid_polys)))

  # Return the grid polygons
  return(grid_polys)
}
