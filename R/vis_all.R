#' Visualize Time Series Data from Beehave Simulation
#'
#' Creates a time series plot from Beehave simulation results using echarty.
#' The function can handle multiple variables and automatically creates a line plot
#' for different metrics.
#'
#' @param result A list containing the simulation results.
#' @param group Character string specifying the group name for the plot.
#' @param columns Character vector specifying the column name(s) for the y-axis (metrics).
#' @param colors Character vector specifying the colors for the lines.
#'
#' @return An echarty object containing the time series visualization.
#'
#' @examples
#' \dontrun{
#' # Simple time series plot
#' bcs_plot_series(result, group = "group_name", columns = "column_name")
#'
#' # Multiple metrics
#' bcs_plot_series(result,
#'   group = "group_name",
#'   columns = c("column1", "column2")
#' )
#' }
#'
#' @importFrom echarty ec.init
#' @export
plot_results <- function(
  result,
  experiment,
  group,
  columns = NULL,
  colors = c(
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
) {
  if (length(group) > 1) {
    warning("Group should be a single string. Using only the first one.")
    group <- group[1]
  }

  sub_result <- result[[group]]
  if (is.null(columns)) {
    columns <- setdiff(sub_result$columns, "tick")
  }

  column_all_id <- which(sub_result$columns %in% columns)
  value_min <- min(0, sub_result$data[, column_all_id])
  value_max <- max(sub_result$data[, column_all_id]) + 5

  series_list <- list()
  for (i in seq_along(columns)) {
    column_id <- which(columns[i] == sub_result$columns)

    series_list <- series_list |>
      append(list(
        list(
          name = columns[i],
          type = "line",
          # symbol = "pin",
          showSymbol = FALSE,
          symbolSize = 20,
          color = colors[i],
          emphasis = list(disabled = TRUE),
          data = sub_result$data[, column_id]
        )
      ))
  }

  p <- echarty::ec.init()
  p$x$opts <- list(
    # title = list(text = "Grassland simulation"),
    tooltip = list(
      trigger = "axis" # ,
      # formatter = formatter
    ),
    # legend = list(data = pft_unique),
    xAxis = list(
      type = "category",
      boundaryGap = TRUE,
      name = "Tick",
      nameLocation = "middle",
      nameGap = 25,
      nameTextStyle = list(fontWeight = "bolder"),
      data = sub_result$data[, 1]
    ),
    yAxis = list(
      type = "value",
      boundaryGap = FALSE,
      name = group,
      nameLocation = "middle",
      nameGap = 40,
      nameTextStyle = list(fontWeight = "bolder"),
      min = value_min,
      max = value_max
    ),
    series = series_list
  )
  return(p)
}
