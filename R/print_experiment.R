#' Print Beehave experiment settings
#'
#' @param experiment a beehave experiment created by beehave_init()
#' @param groups list of parameter groups or specific parameters to show. If NULL, all parameters are shown.
#'              Can be a list with elements:
#'              - groups: character vector of predefined groups ("system", "development", "mortality",
#'                "foraging", "resource_handling", "resource_management", "colony_management")
#'              - params: character vector of specific parameter names
#' @param ignore_params parameters to ignore when checking for changes
#'
#' @return Return Beehave experiment list invisibly
#' @export
#'
#' @examples
#' # Print all parameters
#' print(experiment)
#'
#' # Print only development and mortality related parameters
#' print(experiment, groups = list(groups = c("development", "mortality")))
#'
#' # Print specific parameters
#' print(experiment, groups = list(params = c("WorkerDevelopment", "DroneDevelopment")))
#'
#' # Print combination of groups and specific parameters
#' print(experiment, groups = list(
#'     groups = c("mortality", "development"),
#'     params = c("Foragers", "Nursing")
#' ))
#'
#' # Print ignoring certain parameters when checking for changes
#' print(experiment, ignore_params = c("WorkingDirectory", "RandomSeed"))
print.beehave.experiment <- function(
    experiment,
    groups = NULL,
    ignore_params = character(0)) {
    stopifnot("beehave.experiment" %in% class(experiment))

    # Define parameter groups
    param_groups <- list(
        system = c("WorkingDirectory", "Termination", "RandomSeed"),
        development = c("WorkerDevelopment", "DroneDevelopment"),
        mortality = c("WorkerMortality", "DroneMortality"),
        foraging = c("AgeFirstForaging", "Foragers", "Foraging", "ForagingPeriod"),
        resource_handling = c("HandlingTime", "Dance"),
        resource_management = c("EnergyContent", "Stores", "HoneyNeeds", "PollenNeeds"),
        colony_management = c("Nursing")
    )

    # Get parameters to print
    if (!is.null(groups)) {
        # Validate groups structure
        if (!is.list(groups)) {
            stop("'groups' must be a list with optional elements 'groups' and 'params'")
        }

        # Initialize empty vectors for parameters
        group_params <- character(0)
        specific_params <- character(0)

        # Process predefined groups if specified
        if ("groups" %in% names(groups)) {
            invalid_groups <- setdiff(groups$groups, names(param_groups))
            if (length(invalid_groups) > 0) {
                stop("Invalid group names: ", paste(invalid_groups, collapse = ", "))
            }
            group_params <- unique(unlist(param_groups[groups$groups]))
        }

        # Process specific parameters if specified
        if ("params" %in% names(groups)) {
            specific_params <- groups$params
        }

        # Combine all parameters
        selected_params <- unique(c(group_params, specific_params))

        # Validate all parameters exist
        if (length(selected_params) == 0) {
            stop("No parameters selected. Specify either 'groups' or 'params' in the groups list.")
        }

        invalid_params <- setdiff(selected_params, names(experiment))
        if (length(invalid_params) > 0) {
            warning("Invalid parameters: ", paste(invalid_params, collapse = ", "))
        }

        params <- experiment[selected_params]
    } else {
        params <- experiment
    } # nolint: indentation_linter.

    # Check which parameters have changed from defaults
    changed <- has_changed_params(params, ignore_params)

    # Print header
    cat("\nBeehave Experiment Parameters:\n")
    cat("==========================\n\n")

    # Get all parameter names
    param_names <- names(params)

    for (param in param_names) {
        # Skip parameters that should be ignored
        if (param %in% ignore_params) next

        # Print parameter name with change indicator
        if (param %in% names(changed) && changed[param]) {
            cat(sprintf("* %s (modified):\n", param))
        } else {
            cat(sprintf("  %s:\n", param))
        }

        # Print parameter values with indentation
        param_value <- params[[param]]
        if (is.list(param_value)) {
            # For nested parameters, print each on new line
            for (subparam in names(param_value)) {
                cat(sprintf("    %s: %s\n", subparam, param_value[[subparam]]))
            }
        } else {
            # For simple parameters, print directly
            cat(sprintf("    %s\n", param_value))
        }
        cat("\n")
    }

    invisible(experiment)
}
