library(eulerr)
library(arules)
library(ggplot2)

rule_venn <- function(rules, 
                      fill_color = NULL, 
                      fill_alpha = 0.5, 
                      stroke_color = "black", 
                      stroke_size = 1, 
                      set_name_color = "black", 
                      set_name_size = 6,
                      text_color = "black", 
                      text_size = 4) {
  
  validate_rules_venn(rules)
  
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
  
  print(fit$quantities)
  
  plot <- plot(
    fit,
    fills = list(fill = fill_color, alpha = fill_alpha),
    edges = list(col = stroke_color, lwd = stroke_size),
    labels = list(col = set_name_color, fontsize = set_name_size),
    quantities = list(col = text_color, fontsize = text_size)
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


