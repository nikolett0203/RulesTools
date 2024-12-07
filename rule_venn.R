library(ggvenn)
library(arules)

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

