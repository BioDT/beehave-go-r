library(beehave.go.r)

# Create a weather vector with mostly no foraging (0 hours)
# but 90 random days with 15 hours of foraging time
weather_vector <- rep(0, 365)
collection_days <- 120
weather_vector[sample(70:265, collection_days)] <- sample(
  0:15,
  collection_days,
  replace = TRUE
)

# Source all R scripts
experiment <- beehave_init(add_default = FALSE) |>
  # add_weather_file(
  #   "dev/data/weather_402.txt"
  # ) |>
  add_flower_patches_from_map(
    landuse_map = "dev/data/preidl-etal-RSE-2020_land-cover-classification-germany-2016.tif",
    lookup_table = "dev/data/NectarPollenLookUp.csv",
    location = data.frame(lat = 48.2, lon = 7.80003),
    min_polygon_size = 5000
  ) |>
  add_weather_vector(weather_vector) |>
  add_parameter(
    list(
      # InitialPatches = list(
      #   Patches = list(
      #     list(
      #       DistToColony = 1000,
      #       ConstantPatch = list(
      #         Nectar = 5,
      #         Pollen = 1,
      #         NectarConcentration = 1,
      #         DetectionProbability = 0.5
      #       )
      #     ),
      #     list(
      #       DistToColony = 200,
      #       SeasonalPatch = list(
      #         MaxNectar = 20,
      #         MaxPollen = 10,
      #         NectarConcentration = 1.5,
      #         DetectionProbability = 0.2,
      #         SeasonShift = 20
      #       )
      #     )
      #   )
      # ),
      reporters = c(
        "worker_cohorts",
        "stores"
      )
    )
  )


plot_patches_map(experiment)

print(experiment)

test <- run_simulation(experiment)


bcs_plot_series(test, group = "stores")
