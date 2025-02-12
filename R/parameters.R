#' Add parameters to the Beehave experiment
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param params set of input parameters, see details
#'
#' @return Return Beehave experiment list
#' @export
#'
#' @examples
add_parameter <- function(experiment, params) {
  stopifnot(
    is.list(params),
    "beehave.experiment" %in% class(experiment)
  )

  default_params <- get_default_params()

  non_params <- setdiff(names(params), names(default_params))

  if (length(non_params) > 0) {
    warning(paste0(
      "There are elements which do not belong to the models and will not be used: ",
      paste0(non_params, collapse = ", ")
    ))
  }

  experiment[names(params)] <- params

  return(experiment)
}

#' Print default parameters of the Beehave experiment
#'
#' @export
#'
#' @examples
get_default_params <- function() {
  # This print should be beautified with cat and possibility to return default params list should be made

  params <- jsonlite::fromJSON(
    # simplifyVector = FALSE,
    '
{
"WorkingDirectory": {
  "Path": "."
},
"Termination": {
  "MaxTicks":     365,
  "OnExtinction": false
},
"RandomSeed": {
  "Seed": 0
},
"WorkerDevelopment": {
  "EggTime":     3,
  "LarvaeTime":  6,
  "PupaeTime":   12,
  "MaxLifespan": 290
},
"DroneDevelopment": {
  "EggTime":     3,
  "LarvaeTime":  7,
  "PupaeTime":   14,
  "MaxLifespan": 37
},
"WorkerMortality": {
  "Eggs":      0.03,
  "Larvae":    0.01,
  "Pupae":     0.001,
  "InHive":    0.004,
  "MaxMilage": 800
},
"DroneMortality": {
  "Eggs":   0.064,
  "Larvae": 0.044,
  "Pupae":  0.005,
  "InHive": 0.05
},
"AgeFirstForaging": {
  "Base": 21,
  "Min":  7,
  "Max":  50
},
"Foragers": {
  "FlightVelocity": 6.5,
  "FlightCostPerM": 0.000006,
  "NectarLoad":     50,
  "PollenLoad":     0.015,
  "MaxKmPerDay":    7299,
  "SquadronSize":   100
},
"Foraging": {
  "ProbBase":      0.01,
  "ProbHigh":      0.05,
  "ProbEmergency": 0.2,
  "SearchLength": 6630,
  "EnergyOnFlower":  0.2,
  "MortalityPerSec": 0.00001,
  "StopProbability":     0.3,
  "AbandonPollenPerSec": 0.00002
},
"ForagingPeriod": {
  "Files":       "foraging-period/berlin2000.txt",
  "Builtin":     true,
  "RandomYears": false
},
"HandlingTime": {
  "NectarGathering":      1200,
  "PollenGathering":      600,
  "NectarUnloading":      116,
  "PollenUnloading":      210,
  "ConstantHandlingTime": false
},
"Dance": {
  "Slope":                       1.16,
  "Intercept":                   0.0,
  "MaxCircuits":                 117,
  "FindProbability":             0.5,
  "PollenDanceFollowers":        2,
  "MaxProportionPollenForagers": 0.8
},
"EnergyContent": {
  "Honey":   12.78,
  "Scurose": 0.00582
},
"Stores": {
  "IdealPollenStoreDays": 7,
  "MinIdealPollenStore":  250.0,
  "MaxHoneyStoreKg":      50.0,
  "ProteinStoreNurse":    7
},
"HoneyNeeds": {
  "WorkerResting":    11.0,
  "WorkerNurse":      53.42,
  "WorkerLarvaTotal": 65.4,
  "DroneLarva":       19.2,
  "Drone":            10.0
},
"PollenNeeds": {
  "WorkerLarvaTotal": 142.0,
  "DroneLarva":       50.0,
  "Worker":           1.5,
  "Drone":            2.0
},
"Nursing": {
  "MaxBroodNurseRatio":         3.0,
  "ForagerNursingContribution": 0.2,
  "MaxEggsPerDay":              1600,
  "DroneEggsProportion":        0.04,
  "EggNursingLimit":            true,
  "MaxBroodCells":              200000,
  "DroneEggLayingSeasonStart":  115,
  "DroneEggLayingSeasonEnd":    240
},
"InitialPopulation": {
  "Count":     10000,
  "MinAge":    100,
  "MaxAge":    160,
  "MinMilage": 0,
  "MaxMilage": 200
},
"InitialStores": {
  "Honey":  25,
  "Pollen": 100
},
"InitialPatches": {
  "Patches": {
    "PatchConfig": [
    {
      "DistToColony": 1500,
      "ConstantPatch": {
        "Nectar":               20,
        "NectarConcentration":  1.5,
        "Pollen":               1,
        "DetectionProbability": 0.2
      }
    },
  {
      "DistToColony": 500,
      "ConstantPatch": {
        "Nectar":               20,
        "NectarConcentration":  1.5,
        "Pollen":               1,
        "DetectionProbability": 0.2
      }
  }
  ]
}
}
}
'
  )
}

#' Check if parameters differ from default values
#'
#' @param params list of parameters to check
#' @param ignore_params optional character vector of parameter paths to ignore in comparison
#'
#' @return Named logical vector indicating which top-level parameters have changed from defaults
#' @export
#'
#' @examples
#' # Check all parameters
#' has_changed_params(my_params)
#'
#' # Ignore specific parameters in comparison
#' has_changed_params(my_params, ignore_params = c("WorkingDirectory", "RandomSeed"))
has_changed_params <- function(params, ignore_params = character(0)) {
  # Get default parameters
  defaults <- get_default_params()
  # Get top-level parameter names
  param_names <- setdiff(names(defaults), ignore_params)
  # Initialize result vector
  changes <- logical(length(param_names))
  names(changes) <- param_names
  # Compare each top-level parameter
  for (param in param_names) {
    if (param %in% names(params)) {
      # Check if parameter exists and is different from default
      changes[param] <- !identical(params[[param]], defaults[[param]])
    } else {
      # Parameter not present in input, mark as unchanged
      changes[param] <- FALSE
    }
  }

  return(changes)
}
