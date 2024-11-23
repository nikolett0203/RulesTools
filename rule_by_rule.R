# add conf, support, lift?
# foreach, doParallel
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
  labels <- sapply(rules, labels, simplify = FALSE)
  
  # generate intersection data
  intersections <- find_intersections(labels, rule_names)
  
  # print results
  if(display)
    print(intersections)

  # write to file
  if (!is.null(filename)) {
    if (!grepl("\\.csv$", filename)) {
      warning("Filename does not have a .csv extension. Appending .csv to the filename.")
      filename <- paste0(filename, ".csv")
    }
    write_data(intersections, filename)
  }
  
  #return(intersections)
  
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

get_intersection_key <- function(indices, rule_names){
  paste(rule_names[indices], collapse = " & ")
}

find_intersections <- function (rules, rule_names){
  
  # structure to store our intersection data
  intersections <- list()
  
  indices <- seq_along(rules)
  
  # add non-intersection data first (unmodified rules)
  # set rule name as the key and assign rulesets
  for (i in indices) {
    intersections[[rule_names[i]]] <- rules[[i]]
  }
  
  # now we go into the loop to generate intersections
  n <- length(rules)
  # generate all 2-sets, 3-sets, etc.
  # we already inputted 1-sets
  for (k in 2:n){
    # each row corresponds to a position in the combination and number of rows in size of combo, k
    # each column represents one combination of elements, number of columns is total number of combinations
    combs <- combn(indices, k)
    for (j in 1:ncol(combs)){
      # get each combination of the k-set
      curr_comb <- combs[,j]
      # get name for intersection
      key <- get_intersection_key(curr_comb, rule_names)
      # calculate common rules
      insct_value <- Reduce(intersect, rules[curr_comb])
      # explicitly state if nothing is common
      if (length(insct_value) == 0) {
        insct_value <- "No common rules"
      }
      # store data in list
      intersections[[key]] <- insct_value
    }
  }
  return(intersections)
}

pad_rules <- function(rules, max_length) {
  length(rules) <- max_length
  return(rules)
}


write_data <- function(intersections, filename){
  
  max_length <- max(sapply(intersections, length))
  
  intersection_df <- data.frame(
    lapply(intersections, pad_rules, max_length = max_length),
    stringsAsFactors = FALSE
  )
  
  colnames(intersection_df) <- names(intersections)
  
  write.csv(intersection_df, file = filename, row.names = FALSE, na = "")
  message(paste("Results saved to", filename))
  
}
