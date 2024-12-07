library(ggvenn)
library(arules)

source=("./rule_by_rule.R")

rule_venn <- function(rules, 
                       min_overlap = 1, 
                       show_elements = FALSE, 
                       show_percentage = TRUE, 
                       fill_color = NULL, 
                       fill_alpha = 0.5, 
                       stroke_color = "black", 
                       stroke_size = 1, 
                       set_name_color = "black", 
                       text_color = "black", 
                       text_size = 4, 
                       digits = 1){
  
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
  
  ggvenn(
    sets,
    show_elements = show_elements,
    show_percentage = show_percentage,
    digits = digits,
    fill_color = fill_color,
    fill_alpha = fill_alpha,
    stroke_color = stroke_color,
    stroke_size = stroke_size,
    set_name_color = set_name_color,
    text_color = text_color,
    text_size = text_size
  )
  
  
  
}

validate_rules_venn <- function(rules){
  
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

