library(beehave.go.r)

weather_vector <- matrix(
  c(
    rep(10, 30),
    rep(0, 335)
  ),
  nrow = 1
)

# Source all R scripts
experiment <- beehave_init(add_default = FALSE) |>
  add_weather_file(
    "dev/data/weather_402.txt"
  ) |>
  add_flower_patches_from_map(
    landuse_map = "dev/data/preidl-etal-RSE-2020_land-cover-classification-germany-2016.tif",
    lookup_table = "dev/data/NectarPollenLookUp.csv",
    location = data.frame(lat = 48.2, lon = 7.8)
  ) |>
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

plot_flower_patches(experiment)
print(experiment)
test <- run_simulation(experiment)

bcs_plot_series(test, group = "stores")
