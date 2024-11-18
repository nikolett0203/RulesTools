rule_by_rule <- function(...,  # would it be an issue if someone did TRUE TRUE FALSE for ex?
                         display = TRUE, # can suppress printing if necessary
                         filename = NULL) { # if filename !NULL, will save as csv

  # collect arguments
  rules <- list(...)
  
  # ensure arguments are named to simplify parsing
  rule_names <- names(rules)
  validate_names(rule_names)
  
  # ensure optional parameters are correct types
  validate_options(display, filename)
  
  # ensure enough rule-type objects provided
  validate_rules(rules)

  # gather counts and formatted rules for each ruleset
  counts <- sapply(rules, length)
  labels <- sapply(rules, labels)
  
  # find rules common to all sets by repeatedly intersecting rules together
  common <- Reduce(intersect, labels)
  
  print(common)
  
  n <- length(counts)
  
  # something like this would be necessary
  for(k in 1:n){
    combs<-(combn(n, k))
    num <- ncol(combs)
    for (j in 1:num){
      indices <- combs[, j]
      print(Reduce(intersect, labels[indices]))
      print("DONE")
    }
  }
  
}

validate_names <- function(name_list){
  if (is.null(name_list) || any(name_list == "")) {
    stop("Please provide names for all arguments, including 'display', 'filename', and all rule sets.")
  }
}

validate_options <- function(display, filename){
 
  if(length(display) != 1 || !is.logical(display) || is.na(display)){
    stop("'display' must be either TRUE or FALSE.")
  }
  
  if(is.null(filename)){
    return()
  }
  
  if (!is.character(filename) || length(filename) != 1 || nchar(filename) == 0) {
    stop("'filename' must be a non-empty string or NULL.")
  }
  
}

validate_rules <- function (rule_list){
  
  # ensure enough arguments to do comparison
  if(length(rule_list) < 2)
    stop("At least two rule sets are required to find overlaps. Please provide at least two rule sets.")
  
  # ensure rule_list is indeed a list of rules objects
  if(!all(sapply(rule_list, inherits, "rules")))
    stop("All inputs must be of class 'rules'. Please provide valid rule sets.")
    
}

find_intersections <- function (rules, rule_names){
  
  #Generate all combinations of the provided rule sets.
  #Compute intersections for each combination.
  #Store and display the results for each combination, including individual rule sets.
  
}

# function to compare indefinite number of rules by finding common rules and displaying their interestingness measures
# params: indefinite number of rules objects (must be named arguments)
rule_by_rule2 <- function (...) {
  
  # collect arguments
  rules <- list(...)
  
  verify_inputs(rules)
  
  # obtain counts, isolate rules without interestingness data
  counts <- sapply(rules, length)
  labels <- sapply(rules, labels)
  names <- names(rules)
  
  # find common rules by repeatedly intersecting rule lists together
  common <- Reduce(intersect, labels)
  
  # inform user if no common rules found
  # TEST THIS
  if (length(common) == 0) {
    print("No common rules found.")
    return(NULL)
  }
  
  # initialise dataframe
  df <- data.frame(Rules = common)
  
  # iterate over all rules and collect interestingness measures in df 
  for (i in seq_along(rules)) {
    
    # find common rules in original rule objects
    crules <- rules[[i]][labels[[i]] %in% common]
    # get the quality dataframe from a particular set
    quality <- quality(crules)
    
    # add support/conf/lift to df using user-inputted names for origin set
    df[paste0("Support_", names[i])] <- quality$support
    df[paste0("Confidence_", names[i])] <- quality$confidence
    df[paste0("Lift_", names[i])] <- quality$lift
  }
  
  print("Number of rules in provided sets:")
  print(counts)
  cat("\n")
  
  print("Number of rules in common:")
  print(length(common))
  cat("\n")
  
  print("Common rules:")  
  print(df)
  
  return(df)
  
}

# helper function to verify that function arguments are arules objects and named
# @param rule_list: a list containing items to be verified as rules
verify_inputs <- function(rule_list){
  
  # check if all arguments are rules
  if(!all(sapply(rule_list, inherits, "rules"))) {
    stop("Arguments must be objects of class 'rules'")
  }
  
  # make sure user gives names
  if (is.null(names(rule_list)) || any(names(rule_list) == "")) {
    stop("Please provide names for all arguments.")
  }
  
}

# helper function to reformat rules by extracting labels and adding newlines
# @ param rule: single rule object to be reformatted and extracted
edit_rule <- function(rule) {
  return(paste0(labels(rule), "\n"))
}
