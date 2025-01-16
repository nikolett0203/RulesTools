#' Create a Heatmap for Association Rules
#'
#' Generates a heatmap visualization of association rules,
#' showing relationships between antecedents and consequents based on a specified metric.
#'
#' @param rules An object of class `rules` from the `arules` package.
#' @param metric A character string specifying the metric to use for coloring the heatmap.
#'   Must be one of `"confidence"`, `"support"`, or `"lift"`. Defaults to `"confidence"`.
#' @param graph_title A character string specifying the title of the graph.
#'   Defaults to an empty string (`""`).
#' @param graph_title_size A numeric value specifying the size of the graph title text.
#'   Defaults to `14`.
#' @param x_axis_title A character string specifying the title for the x-axis.
#'   Defaults to `"Antecedents"`.
#' @param x_axis_title_size A numeric value specifying the size of the x-axis title text.
#'   Defaults to `12`.
#' @param x_axis_text_size A numeric value specifying the size of the x-axis text.
#'   Defaults to `11`.
#' @param x_axis_text_angle A numeric value specifying the angle of the x-axis text.
#'   Defaults to `45`.
#' @param y_axis_title A character string specifying the title for the y-axis.
#'   Defaults to `"Consequents"`.
#' @param y_axis_title_size A numeric value specifying the size of the y-axis title text.
#'   Defaults to `12`.
#' @param y_axis_text_size A numeric value specifying the size of the y-axis text.
#'   Defaults to `11`.
#' @param y_axis_text_angle A numeric value specifying the angle of the y-axis text.
#'   Defaults to `0`.
#' @param legend_title A character string specifying the title of the legend. Defaults to the value of `metric`.
#' @param legend_text_size A numeric value specifying the size of the legend text. Defaults to `8`.
#' @param legend_position A character string specifying the position of the legend.
#'   Possible values are `"right"` (default), `"left"`, `"top"`, `"bottom"`, or `"none"`.
#' @param low_color A valid R color or hex color code for the lower bound of the gradient.
#'   Defaults to `"lightblue"`.
#' @param high_color A valid R color or hex color code for the upper bound of the gradient.
#'   Defaults to `"navy"`.
#' @param include_zero A logical value indicating whether to include zero values for missing antecedent-consequent combinations.
#'   Defaults to `FALSE`.
#'
#' @return A `ggplot` object representing the heatmap visualization of the association rules.
#'
#' @import ggplot2
#' @importFrom arules lhs rhs quality
#' @importFrom tidyr complete
#' @importFrom grDevices colors
#' @importFrom magrittr %>%
#'
#' @examples
#' library(arules)
#' library(tidyr)
#' data(BrookTrout)
#'
#' # Discretise data
#' discrete_bt <- dtize_df(BrookTrout, cutoff="median")
#'
#' # Generate rules
#' rules <- apriori(
#'   discrete_bt,
#'   parameter = list(supp = 0.01, conf = 0.5, target = "rules"),
#'   appearance = list(rhs="eDNAConc=high")
#' )
#'
#' # Subset ruleset (too many rules won't fit on the heatmap)
#' rules <- rules %>%
#'   subset(!is.redundant(., measure = "confidence")) %>%
#'   subset(is.significant(., alpha = 0.05)) %>%
#'   sort(by = c("confidence", "lift", "support"))
#'
#' # Create a heatmap of the rules using confidence as the metric
#' rule_heatmap(
#'   rules,
#'   metric = "confidence",
#'   graph_title = "Confidence Heatmap"
#' )
#'
#' # Create a heatmap of the rules using lift as the metric
#' rule_heatmap(
#'   rules,
#'   metric = "lift",
#'   graph_title = "Lift Heatmap",
#'   low_color = "#D4A221",
#'   high_color = "darkgreen"
#' )
#'
#' @export

rule_heatmap <- function(rules,
                         metric = "confidence",
                         graph_title = "",
                         graph_title_size = 14,
                         x_axis_title = "Antecedents",
                         x_axis_title_size = 12,
                         x_axis_text_size = 11,
                         x_axis_text_angle = 45,
                         y_axis_title = "Consequents",
                         y_axis_title_size = 12,
                         y_axis_text_size = 11,
                         y_axis_text_angle = 0,
                         legend_title = metric,
                         legend_text_size = 8,
                         legend_position = "right",
                         low_color = "lightblue",
                         high_color = "navy",
                         include_zero = FALSE) {

  # ensure arguments are correct types
  validate_rules_map(rules)
  validate_metric_map(metric)
  validate_title_map(graph_title)
  validate_title_map(x_axis_title)
  validate_title_map(y_axis_title)
  validate_title_map(legend_title)
  validate_color_map(low_color)
  validate_color_map(high_color)
  validate_text_size(graph_title_size)
  validate_text_size(x_axis_title_size)
  validate_text_size(y_axis_title_size)
  validate_text_size(x_axis_text_size)
  validate_text_size(y_axis_text_size)
  validate_text_size(legend_text_size)
  validate_legend_position(legend_position)
  validate_angle(x_axis_text_angle)
  validate_angle(y_axis_text_angle)
  validate_logical_map(include_zero)

  # isolate antecedents and consequents
  antecedents <- labels(lhs(rules))
  consequents <- labels(rhs(rules))
  metric <- tolower(metric)

  # store in df
  rule_df <- data.frame(
    antecedents = antecedents,
    consequents = consequents,
    metric = quality(rules)[[metric]]
  )

  # if user wants to include 0 in the scale, allow this
  if (include_zero) {
    rule_df <- rule_df %>%
      complete(antecedents, consequents, fill = list(metric = 0))
  }

  colnames(rule_df) <- c("antecedents", "consequents", "metric")

  # generate plot
  ggplot(rule_df, aes(x = antecedents, y = consequents, fill = metric)) +
    geom_tile() +
    scale_fill_gradient(low = low_color, high = high_color, name = legend_title) +
    labs(
      title = graph_title,
      x = x_axis_title,
      y = y_axis_title,
      fill = metric
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = x_axis_title_size),
      axis.text.x = element_text(size = x_axis_text_size, angle = x_axis_text_angle, hjust = 1),
      axis.title.y = element_text(size = y_axis_title_size),
      axis.text.y = element_text(size = y_axis_text_size, angle = y_axis_text_angle),
      plot.title = element_text(size = graph_title_size, hjust = 0.5),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_text_size + 2),
      legend.position = tolower(legend_position),
      panel.grid = element_blank()
    )
}


#' @noRd
#' @title Validate Rules Object
#' @description Validates that the input is a non-empty `rules` object.
#' @param rules An object to check for class `rules`.
#' @return None. Throws an error if the input is not a valid `rules` object.

validate_rules_map <- function(rules) {
  if (!inherits(rules, "rules")) {
    stop("Input must be an object of class 'rules'. Please provide a valid rule set.")
  }

  if (length(rules) == 0) {
    stop("`rules` object is empty. Please provide a non-empty ruleset.")
  }
}


#' @noRd
#' @title Validate Metric
#' @description Validates that the metric is one of the allowed values: `"confidence"`, `"support"`, or `"lift"`.
#' @param metric A character string specifying the metric.
#' @return None. Throws an error if the metric is invalid.

validate_metric_map <- function(metric) {
  valid_metrics <- c("confidence", "support", "lift")

  if (!is.character(metric) || length(metric) != 1 || !tolower(metric) %in% valid_metrics) {
    stop("'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")
  }
}


#' @noRd
#' @title Validate Graph Title
#' @description Validates that the graph title is a single non-NA character string or `NULL`.
#' @param graph_title The title of the graph.
#' @return None. Throws an error if the graph title is invalid.

validate_title_map <- function(graph_title) {
  if (is.null(graph_title)) {
    return()
  }

  if (!is.character(graph_title) || length(graph_title) != 1 || is.na(graph_title)) {
    stop("The graph and axis titles must be either NULL or a single non-NA character string.")
  }
}


#' @noRd
#' @title Validate Color Input
#' @description Validates that the color input is a valid hex color code or an R color name.
#' @param color A color specified as a hex code or an R color name.
#' @return None. Throws an error if the color is invalid.

validate_color_map <- function(color) {
  hex_pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$"

  if (is.character(color) && length(color) == 1) {
    if (grepl(hex_pattern, color)) {
      return(TRUE)
    }
    if (color %in% colors()) {
      return(TRUE)
    }
  }

  stop(
    paste(
      "The input is not a valid hex color code or R color name.",
      "Please provide a valid hex code (e.g., '#FFFFFF')",
      "or a recognized R color name (e.g., 'red').",
      sep = " "
    )
  )
}


#' @noRd
#' @title Validate Logical Input
#' @description Validates that the input is a single logical value (`TRUE` or `FALSE`).
#' @param input The logical input to validate.
#' @return None. Throws an error if the input is not a valid logical value.

validate_logical_map <- function(input) {
  if (length(input) != 1 || !is.logical(input) || is.na(input)) {
    stop("'include_zero' must be either 'TRUE' or 'FALSE'.")
  }
}


#' @noRd
#' @title Validate Text Size
#' @description Validates that the input is a single, positive, numeric value that is not infinite.
#' @param size The text size input to validate.
#' @return None. Throws an error if the input is not a valid text size.

validate_text_size <- function(size) {
  if (!is.numeric(size) || length(size) != 1 || size <= 0 || is.infinite(size)) {
    stop("Text sizes must be single, non-infinite, positive, and numeric values.")
  }
}


#' @noRd
#' @title Validate Legend Position
#' @description Validates that the input is a single character string representing a valid legend position.
#' @param position The legend position input to validate.
#' @return None. Throws an error if the input is not a valid legend position.

validate_legend_position <- function(position) {
  if (!is.character(position) || length(position) != 1) {
    stop("Legend position must be a single character string.")
  }

  valid_positions <- c("right", "left", "top", "bottom", "none")
  position <- tolower(position)

  if (!position %in% valid_positions) {
    stop("Invalid legend position. Choose from 'right', 'left', 'top', 'bottom', or 'none'.")
  }
}


#' @noRd
#' @title Validate Angle
#' @description Validates that the input is a single numeric value between 0 and 360 degrees.
#' @param angle The angle input to validate.
#' @return None. Throws an error if the input is not a valid angle.

validate_angle <- function(angle) {
  if (!is.numeric(angle) || length(angle) != 1 || angle < 0 || angle > 360) {
    stop("Axis text angle must be a numeric value between 0 and 360 degrees.")
  }
}
