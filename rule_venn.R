library(eulerr)
library(arules)
library(ggplot2)

rule_venn <- function(rules, 
                      fill_color = NULL, 
                      fill_alpha = 0.5, 
                      stroke_color = "black", 
                      stroke_size = 1, 
                      title = NULL,
                      name_color = "black", 
                      name_size = 12,
                      text_color = "black", 
                      text_size = 11) {
  
  validate_rules_venn(rules)
  validate_title_venn(title)
  validate_numeric_venn(stroke_size, "stroke_size")
  validate_numeric_venn(name_size, "name_size") 
  validate_numeric_venn(text_size, "text_size") 
  validate_alpha_venn(fill_alpha)
   
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
  
  plot <- plot(
    fit,
    fills = list(fill = fill_color, alpha = fill_alpha),
    edges = list(col = stroke_color, lwd = stroke_size),
    labels = list(col = name_color, fontsize = name_size),
    quantities = list(col = text_color, fontsize = text_size),
    main = title,
  )
  
  return(plot)
}


validate_rules_venn <- function(rules) {
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

validate_title_venn <- function(graph_title){
  if (is.null(graph_title))
    return()
  if (!is.character(graph_title) || length(graph_title) != 1 || is.na(graph_title))
    stop("The graph title must be either NULL or a single non-NA character string.")
}

validate_numeric_venn <- function(param, param_name) {
  if (!is.numeric(param) || length(param) != 1 || is.na(param) || !is.finite(param) || param < 0) {
    stop(paste0("'", param_name, "' must be a finite positive numeric value."))
  }
}

validate_alpha_venn <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1) {
    stop("`fill_alpha` must be a single numeric value between 0 and 1.")
  }
}
