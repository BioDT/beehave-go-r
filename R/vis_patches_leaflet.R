#' Visualize Flower Patches on a Leaflet Map
#'
#' Creates an interactive leaflet map visualization of flower patches used in a Beehave experiment.
#'
#' @param experiment A beehave experiment containing flower patches
#' @param colors Named vector of colors for different patch types
#' @param opacity Numeric, opacity of the flower patches layer (0 to 1, default 0.5)
#'
#' @return A leaflet map object
#' @export
#'
#' @importFrom terra vect project crs
#' @importFrom sf st_as_sf st_transform
#' @importFrom leaflet leaflet addTiles addProviderTiles addPolygons addLayersControl layersControlOptions colorFactor addLegend providers addScaleBar addCircleMarkers
#' @importFrom htmlwidgets onRender
#' @importFrom stats setNames
#' @importFrom grDevices col2rgb rgb
plot_patches_map <- function(experiment, colors = NULL, opacity = 0.8) {
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

  # Convert to sf and transform to WGS84 for leaflet
  sf_polygons <- sf::st_as_sf(all_polygons)
  sf_polygons <- sf::st_transform(sf_polygons, 4326)

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
    # Ensure we have enough colors by recycling if necessary
    if (length(unique_types) > length(default_colors)) {
      default_colors <- rep(default_colors, length.out = length(unique_types))
    }

    colors <- stats::setNames(
      default_colors[seq_len(length(unique_types))],
      unique_types
    )
  }

  # Create color palette function
  pal <- leaflet::colorFactor(
    palette = colors,
    domain = sf_polygons$PatchType
  )

  # Create darker colors for borders
  darken_color <- function(col, factor = 0.7) {
    rgb_vals <- grDevices::col2rgb(col)
    grDevices::rgb(
      red = rgb_vals["red", ] * factor,
      green = rgb_vals["green", ] * factor,
      blue = rgb_vals["blue", ] * factor,
      maxColorValue = 255
    )
  }

  darker_colors <- sapply(colors, darken_color)

  pal_border <- leaflet::colorFactor(
    palette = darker_colors,
    domain = sf_polygons$PatchType
  )

  # Create leaflet map with multiple base layers
  map <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OpenStreetMap") |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "CartoDB Positron"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$OpenTopoMap,
      group = "OpenTopoMap"
    )

  # Add polygons to the map
  map <- map |>
    leaflet::addPolygons(
      data = sf_polygons,
      group = "flower_patches",
      color = ~ pal_border(PatchType),
      fillColor = ~ pal(PatchType),
      weight = 1,
      opacity = opacity,
      fillOpacity = opacity,
      popup = ~ paste("Patch type:", PatchType)
    ) |>
    leaflet::addScaleBar(
      position = "bottomleft"
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = sf_polygons$PatchType,
      title = "Patch Type",
      opacity = 1
    )

  # Add Colony Location
  overlay_groups <- c("flower_patches")
  colony_coords <- NULL

  if (
    !is.null(experiment$Colony) &&
      !is.null(experiment$Colony$Coords)
  ) {
    colony_coords <- experiment$Colony$Coords
  } else if (
    !is.null(experiment$Colony) && !is.null(experiment$Colony$Location)
  ) {
    colony_coords <- experiment$Colony$Location
  }

  if (!is.null(colony_coords)) {
    colony_vect <- terra::vect(
      cbind(colony_coords$X, colony_coords$Y),
      crs = terra::crs(all_polygons)
    )
    colony_sf <- sf::st_as_sf(colony_vect)
    colony_sf <- sf::st_transform(colony_sf, 4326)

    map <- map |>
      leaflet::addCircleMarkers(
        data = colony_sf,
        group = "Colony",
        color = "black",
        fillColor = "white",
        radius = 5,
        weight = 1,
        opacity = 1,
        fillOpacity = 1,
        popup = "Colony Location"
      )
    overlay_groups <- c(overlay_groups, "Colony")
  } else {
    warning("Colony location not found in experiment.")
  }

  map <- map |>
    leaflet::addLayersControl(
      baseGroups = c(
        "OpenStreetMap",
        "Satellite",
        "CartoDB Positron",
        "OpenTopoMap"
      ),
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  # Add custom JS for opacity slider
  js_code <- sprintf(
    "
    function(el, x) {
      var map = this;
      var opacityControl = L.control({position: 'bottomleft'});
      opacityControl.onAdd = function (map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = '<div style=\"background-color: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);\">' +
          '<label for=\"opacity-slider\" style=\"display: block; margin-bottom: 5px; font-weight: bold; font-family: sans-serif;\">Opacity</label>' +
          '<input id=\"opacity-slider\" type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"%s\" style=\"width: 100px;\">' +
          '</div>';
        L.DomEvent.disableClickPropagation(div);
        return div;
      };
      opacityControl.addTo(map);

      document.getElementById('opacity-slider').addEventListener('input', function(e) {
        var value = e.target.value;
        var layerGroup = map.layerManager.getLayerGroup('flower_patches');
        if (layerGroup) {
          layerGroup.eachLayer(function(layer) {
            layer.setStyle({
              fillOpacity: value,
              opacity: value
            });
          });
        }
      });
    }
  ",
    opacity
  )

  map <- map |> htmlwidgets::onRender(js_code)

  return(map)
}
