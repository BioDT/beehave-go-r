library(beehave.go.r)

weather_vector <- matrix(
  c(
    rep(10, 30),
    rep(0, 335)
  ),
  nrow = 1
)

# Source all R scripts
experiment <- beehave_init() |>
  add_parameter(
    list(
      InitialPopulation = list(Count = 50000),
      Termination = list(MaxTicks = 800)
    )
  ) |>
  add_weather_vector(
    weather_vector = weather_vector
  ) |>
  add_parameter(list(reporters = list("worker_cohorts")))

# print(experiment)

test <- beehave.go.r::run_simulation(experiment)
