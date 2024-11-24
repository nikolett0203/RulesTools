library(arules)
library(ggplot2)

# add ... to pass more lower level arguments?

rule_map <- function(rules, 
                     metric = "confidence",
                     graph_title = "",
                     low_color = "lightblue",
                     high_color = "navy",
                     include_zero = FALSE){
  
  validate_rules(rules)
  validate_metric(metric)
  validate_title(graph_title)
  validate_color(low_color)
  validate_color(high_color)
  validate_logical(include_zero)
  
  antecedents <- labels(lhs(rules))
  consequents <- labels(rhs(rules))

  metric <- tolower(metric)
  
  rule_df <- data.frame(
    antecedents = antecedents,
    consequents = consequents,
    metric = quality(rules)[[metric]]
  )
  
  if(include_zero){
    rule_df <- rule_df %>%
     complete(antecedents, consequents, fill = list(metric = 0))
  }

  colnames(rule_df) <- c("antecedents", "consequents", "metric")
  
  ggplot(rule_df, aes(x = antecedents, y = consequents, fill = metric)) +
    geom_tile() +
    scale_fill_gradient(low = low_color, high = high_color) +
    labs(title = graph_title, x = "Antecedents", y = "Consequents", fill = metric) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank())
}

validate_rules <- function(rules){
  
  if(!inherits(rules, "rules"))
    stop("Input must be of class 'rules'. Please provide a valid rule set.")
  
}

validate_metric <- function(metric){
  
  valid_metrics <- c("confidence", "support", "lift")
  
  if (!is.character(metric) || !tolower(metric) %in% valid_metrics)
    stop("'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")
  
}

validate_title <- function(graph_title){
  
  if (is.null(graph_title))
    return()
  
  if (!is.character(graph_title) || length(graph_title) != 1 || is.na(graph_title))
    stop("The graph title must be either NULL or a single non-NA character string.")
  
}

validate_color <- function(color) {

  hex_pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$"
  
  if(is.character(color) && length(color) == 1){
    if (grepl(hex_pattern, color))
      return(TRUE)
    
    if (color %in% colors())
      return(TRUE)
  }

  stop(paste(
    "The input is not a valid hex color code or R color name.",
    "Please provide a valid hex code (e.g., '#FFFFFF')",
    "or a recognized R color name (e.g., 'red').",
    sep = " "
  ))
   
}

validate_logical <- function(input){
  if(length(input) != 1 || !is.logical(input) || is.na(input))
    stop("'include_zero' must be either 'TRUE' or 'FALSE'.")
}

