#' Create an Euler Diagram for Association Rules
#'
#' Generates an Euler diagram visualization for up to 4 sets of association rules.
#' The function displays the relationships between rule sets with customizable colors, transparency, and labels.
#'
#' @param rules A list of `rules` objects from the `arules` package.
#'   The list must contain between 2 and 4 `rules` objects.
#' @param fill_color A character vector of valid R color names or hex color codes for filling the sets.
#'   If `NULL`, default colors `c("red", "blue", "green", "purple")` will be used. Defaults to `NULL`.
#' @param fill_alpha A numeric value between 0 and 1 specifying the transparency of the fill colors. Defaults to `0.5`.
#' @param stroke_color A character string specifying the color of the set borders. Defaults to `"black"`.
#' @param stroke_size A positive numeric value specifying the size of the set borders. Defaults to `1`.
#' @param title A character string specifying the title of the Euler diagram. Defaults to `NULL`.
#' @param name_color A character string specifying the color of the set names. Defaults to `"black"`.
#' @param name_size A positive numeric value specifying the font size of the set names. Defaults to `12`.
#' @param text_color A character string specifying the color of the quantity labels (counts) in the diagram. Defaults to `"black"`.
#' @param text_size A positive numeric value specifying the font size of the quantities (counts). Defaults to `11`.
#'
#' @return A `plot` object displaying the Euler diagram visualization.
#'
#' @import eulerr
#' @importFrom arules labels
#' @importFrom grDevices colors
#'
#' @examples
#' library(arules)
#' data(BrookTrout)
#'
#' # Discretize the BrookTrout dataset
#' discrete_bt <- dtize_df(BrookTrout, cutoff = "median")
#'
#' # Generate the first set of rules with a confidence threshold of 0.5
#' rules1 <- apriori(
#'   discrete_bt,
#'   parameter = list(supp = 0.01, conf = 0.5, target = "rules")
#' )
#'
#' # Generate the second set of rules with a higher confidence threshold of 0.6
#' rules2 <- apriori(
#'   discrete_bt,
#'   parameter = list(supp = 0.01, conf = 0.6, target = "rules")
#' )
#'
#' # Create an Euler diagram to visualize the intersections between the rule sets
#' rule_euler(
#'   rules = list(conf0.5 = rules1, conf0.6 = rules2),
#'   title = "Euler Diagram of BrookTrout Rule Sets",
#'   fill_color = c("#7832ff", "lightgreen"),
#'   stroke_color = "darkblue"
#' )
#'
#' @export

rule_euler <- function(rules,
                      fill_color = NULL,
                      fill_alpha = 0.5,
                      stroke_color = "black",
                      stroke_size = 1,
                      title = NULL,
                      name_color = "black",
                      name_size = 12,
                      text_color = "black",
                      text_size = 11,
                      show_legend = FALSE,
                      legend_position = "bottom") {

  validate_rules_euler(rules)
  validate_title_euler(title)
  validate_numeric_euler(stroke_size, "stroke_size")
  validate_numeric_euler(name_size, "name_size")
  validate_numeric_euler(text_size, "text_size")
  validate_alpha_euler(fill_alpha)
  validate_color_euler(stroke_color, "stroke_color")
  validate_color_euler(name_color, "name_color")
  validate_color_euler(text_color, "text_color")
  validate_legend_pos_euler(legend_position)
  validate_logical_euler(show_legend)

  for(i in seq_along(fill_color)){
    validate_color_euler(fill_color[[i]], "fill_color")
  }

  sets <- lapply(rules, labels)

  if (is.null(names(rules)) || any(names(rules) == "" | is.na(names(rules)))) {
    names(sets) <- paste0("Set ", seq_along(sets))
  } else {
    names(sets) <- names(rules)
  }

  if (is.null(fill_color)) {
    default_colors <- c("red", "blue", "green", "purple")
    fill_color <- default_colors[seq_along(sets)]
  }

  euler_input <- list()
  for (i in seq_along(sets)) {
    euler_input[[names(sets)[i]]] <- sets[[i]]
  }

  fit <- euler(euler_input)

  if(show_legend){
    legend_params = list(side = tolower(legend_position))
    label_params = NULL
  } else {
    legend_params = NULL
    label_params = list(col = name_color, fontsize = name_size)
  }

  plot <- plot(
    fit,
    fills = list(fill = fill_color, alpha = fill_alpha),
    edges = list(col = stroke_color, lwd = stroke_size),
    labels = label_params,
    quantities = list(col = text_color, fontsize = text_size),
    main = title,
    legend = legend_params
  )

  return(plot)
}


#' @noRd
#' @title Validate Rules Input for Euler Diagram
#' @description Validates that the input is a list containing between 2 and 4 `rules` objects.
#' @param rules A list of `rules` objects.
#' @return None. Throws an error if the input is not a valid list of `rules` objects.

validate_rules_euler <- function(rules) {
  if (!is.list(rules)) {
    stop("'rules' objects must be provided as a list.")
  }
  if (length(rules) < 2 || length(rules) > 4) {
    stop("You must provide between 2 and 4 'rules' objects.")
  }
  if (any(sapply(rules, is.null))) {
    stop("The list contains NULL values. Please provide valid 'rules' objects.")
  }
  if (!all(sapply(rules, function(x) inherits(x, "rules")))) {
    stop("All elements in the list must be 'rules' objects.")
  }
}


#' @noRd
#' @title Validate Graph Title
#' @description Validates that the graph title is either `NULL` or a single non-NA character string.
#' @param graph_title The title of the graph, specified as a character string or `NULL`.
#' @return None. Throws an error if the graph title is invalid.

validate_title_euler <- function(graph_title){
  if (is.null(graph_title))
    return()
  if (!is.character(graph_title) || length(graph_title) != 1 || is.na(graph_title))
    stop("The graph title must be either NULL or a single non-NA character string.")
}


#' @noRd
#' @title Validate Numeric Input
#' @description Validates that the input is a finite, positive numeric value.
#' @param param The numeric value to validate.
#' @param param_name The name of the parameter (used in error messages).
#' @return None. Throws an error if the input is not a valid numeric value.

validate_numeric_euler <- function(param, param_name) {
  if (!is.numeric(param) || length(param) != 1 || is.na(param) || !is.finite(param) || param < 0) {
    stop(paste0("'", param_name, "' must be a finite positive numeric value."))
  }
}


#' @noRd
#' @title Validate Alpha Transparency
#' @description Validates that the alpha transparency value is a number between 0 and 1.
#' @param alpha A numeric value for transparency.
#' @return None. Throws an error if the alpha value is not between 0 and 1.

validate_alpha_euler <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1) {
    stop("`fill_alpha` must be a single numeric value between 0 and 1.")
  }
}


#' @noRd
#' @title Validate Color Input
#' @description Validates that the color input is a valid hex color code or an R color name.
#' @param color A color specified as a hex code or an R color name.
#' @param param_name The name of the parameter (used in error messages).
#' @return None. Throws an error if the color is invalid.

validate_color_euler <- function(color, param_name) {

  hex_pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$"

  if (is.character(color) && length(color) == 1) {
    if (grepl(hex_pattern, color))
      return(TRUE)

    if (color %in% colors())
      return(TRUE)
  }

  stop(paste0("'", param_name, "' must be a valid 6-8 digit hex color code (e.g., '#FFFFFF') or R color name (e.g., 'red')."))
}


#' @noRd
#' @title Validate Legend Position
#' @description Validates that the input is a single character string representing a valid legend position.
#' @param position The legend position input to validate.
#' @return None. Throws an error if the input is not a valid legend position.

validate_legend_pos_euler <- function(position) {
  if (!is.character(position) || length(position) != 1) {
    stop("Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'.")
  }

  valid_positions <- c("right", "left", "top", "bottom")
  position <- tolower(position)

  if (!position %in% valid_positions) {
    stop("Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'.")
  }
}


#' @noRd
#' @title Validate Logical Input
#' @description Validates that the input is a single logical value (`TRUE` or `FALSE`).
#' @param input The logical input to validate.
#' @return None. Throws an error if the input is not a valid logical value.

validate_logical_euler <- function(input) {
  if (length(input) != 1 || !is.logical(input) || is.na(input)) {
    stop("'show_legend' must be either 'TRUE' or 'FALSE'.")
  }
}
