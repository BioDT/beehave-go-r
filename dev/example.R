library(beehave.go.r)

weather_vector <- matrix(
  c(
    rep(10, 30),
    rep(0, 335)
  ),
  nrow = 1
)

# Source all R scripts
experiment <- beehave_init(add_default = TRUE) |>
  add_weather_file(
    "dev/data/weather_1.txt"
  ) |>
  add_parameter(
    list(
      InitialPatches = list(
        Patches = list(
          list(
            DistToColony = 1000,
            ConstantPatch = list(
              Nectar = 5,
              Pollen = 1,
              NectarConcentration = 1,
              DetectionProbability = 0.5
            )
          ),
          list(
            DistToColony = 200,
            SeasonalPatch = list(
              MaxNectar = 20,
              MaxPollen = 10,
              NectarConcentration = 1.5,
              DetectionProbability = 0.2,
              SeasonShift = 20
            )
          )
        )
      ),
      reporters = c(
        "worker_cohorts",
        "stores"
      )
    )
  )

print(experiment)
test <- beehave.go.r::run_simulation(experiment)

bcs_plot_series(test,
  group = "stores"
)
