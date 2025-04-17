# beehave-go-r

This package is a wrapper around the go-based [beecs](https://github.com/biodt/beecs) honeybee colony simulation model.
The go version is based on the [Beehave model](https://beehave-model.net/) which was used for over 10 years by researchers to study honeybee colony dynamics and behaviour.
`beecs` and `beehave.go.r` is an initiave to provide a modern interface to the original Beehave model and possibly extend its functionality in the future.
`beehave.go.r` is currently in alpha stage of the development and on top of the `beecs` it provides functions for creation of an experiment from landuse map and weather files which were used in the original Beehave model.

## Installation

During the installation a go module will be downloaded and compiled.
For this You need to have a go installed on Your system.
See [Go](https://golang.org/dl/) for more information.

```R
install.packages("remotes")
remotes::install_github("biodt/beehave-go-r")
```

## Usage

The beehave.go.r is made with R pipes in mind as such an experiment can be built in a pipeline.
It is possible to add weather, flower patches, and parameters to the experiment iteratively.
Thanks to this it is also possible to edit existing experiment definition, by adding new or overriding existing parameters.

```R
library(beehave.go.r)

# Define weather collection hours from vector
weather_vector <- matrix(
  c(
    rep(10, 30),
    rep(0, 335)
  ),
  nrow = 1
)

# Define weather collection hours from vector, flower patches and reporters
experiment <- beehave_init(add_default = FALSE) |>
  add_weather_vector(weather_vector) |>
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

# Print experiment definition
print(experiment)

# Run simulation
test <- run_simulation(experiment)

# Visualise results
bcs_plot_series(test, group = "stores")
```

For an example using landuse map, lookup table and colony location, see the [dev/example.R](dev/example.R) file.
In this example weather is being read from original beehave weather file.

## Roadmap (not in any particular order)

- [ ] Visualisation of flower patches
- [ ] Complete visualisation of experiment results based on the user defined reporters
- [ ] Shiny App
- [ ] More examples