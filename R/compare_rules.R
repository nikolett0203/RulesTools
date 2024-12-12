#' Compare Association Rule Sets and Find Their Intersections
#'
#' This function compares multiple sets of association rules, identifies intersections,
#' and optionally displays the results or writes them to a CSV file.
#'
#' @param ... Named association rule sets (objects of class `rules`).
#' @param display Logical. If `TRUE`, prints the intersection results. Default is `TRUE`.
#' @param filename Character string. If provided, writes the results to a CSV file. Default is `NULL`.
#'
#' @return A list containing the intersections of the provided rule sets.
#'
#' @examples
#' library(arules)
#' data(Groceries)
#' rules1 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
#' rules2 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.6, target = "rules"))
#' compare_rules(r1 = rules1, r2 = rules2, display = TRUE, filename = "intersections.csv")
#' 
#' @export

compare_rules <- function(...,
                          display = TRUE,
                          filename = NULL) {
  
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
  if (display) {
    print(intersections)
  }
  
  # write to file
  if (!is.null(filename)) {
    if (!grepl("\\.csv$", filename)) {
      warning("Filename does not have a .csv extension. Appending .csv to the filename.")
      filename <- paste0(filename, ".csv")
    }
    write_data(intersections, filename)
  }
}


#' @noRd
#' @title Validate Argument Names
#' @description Ensures that all arguments have unique names and are properly named.
#' @param name_list A character vector of argument names.
#' @return None. Throws an error if names are missing or duplicated.

validate_names <- function(name_list) {
  if (is.null(name_list) || any(name_list == "")) {
    stop("Please provide names for all arguments, including 'display', 'filename', and all rule sets.")
  }
  
  if (anyDuplicated(name_list)) {
    stop("Duplicate names are not allowed. Please provide unique names for all rule sets.")
  }
}


#' @noRd
#' @title Validate Optional Parameters
#' @description Checks that the `display` and `filename` parameters are valid.
#' @param display Logical. Whether to display the intersection results.
#' @param filename A string or NULL. The filename to save the results.
#' @return None. Throws an error if parameters are invalid.

validate_options <- function(display, filename) {
  if (length(display) != 1 || !is.logical(display) || is.na(display)) {
    stop("'display' must be either TRUE or FALSE.")
  }
  
  if (is.null(filename)) {
    return()
  }
  
  if (!is.character(filename) || length(filename) != 1 || nchar(filename) == 0) {
    stop("'filename' must be a non-empty string or NULL.")
  }
}


#' @noRd
#' @title Validate Rule Sets
#' @description Ensures that at least two rule sets are provided and they are of class `rules`.
#' @param rule_list A list of rule sets.
#' @return None. Throws an error if rule sets are invalid.

validate_rules <- function(rule_list) {
  
  # ensure enough arguments to do comparison
  if (length(rule_list) < 2) {
    stop("At least two rule sets are required to find overlaps. Please provide at least two rule sets.")
  }
  
  # ensure rule_list is indeed a list of rules objects
  if (!all(sapply(rule_list, inherits, "rules"))) {
    stop("All inputs must be of class 'rules'. Please provide valid rule sets.")
  }
}


#' @noRd
#' @title Generate Intersection Key
#' @description Creates a string key representing the intersection of rule sets.
#' @param indices A numeric vector of indices.
#' @param rule_names A character vector of rule set names.
#' @return A string representing the intersection key.

get_intersection_key <- function(indices, rule_names) {
  paste(rule_names[indices], collapse = " & ")
}


#' @noRd
#' @title Find Intersections Between Rule Sets
#' @description Computes intersections between multiple rule sets.
#' @param rules A list of rule sets' labels.
#' @param rule_names A character vector of rule set names.
#' @return A list of intersections between rule sets.

find_intersections <- function(rules, rule_names) {
  
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
  for (k in 2:n) {
    # each row corresponds to a position in the combination and number of rows in size of combo, k
    # each column represents one combination of elements, number of columns is total number of combinations
    combs <- combn(indices, k)
    for (j in 1:ncol(combs)) {
      # get each combination of the k-set
      curr_comb <- combs[, j]
      
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


#' @noRd
#' @title Pad Rules to Match Maximum Length
#' @description Pads a vector of rules with `NA` to match a specified length.
#' @param rules A vector of rules.
#' @param max_length The desired length to pad to.
#' @return A vector of rules padded with `NA` values.

pad_rules <- function(rules, max_length) {
  length(rules) <- max_length
  return(rules)
}


#' @noRd
#' @title Write Intersection Data to CSV
#' @description Saves intersection data to a CSV file.
#' @param intersections A list of intersections between rule sets.
#' @param filename The name of the file to save the data to.
#' @return None. Writes the intersections to a CSV file and prints a message.

write_data <- function(intersections, filename) {
  
  max_length <- max(sapply(intersections, length))
  
  intersection_df <- data.frame(
    lapply(intersections, pad_rules, max_length = max_length),
    stringsAsFactors = FALSE
  )
  
  colnames(intersection_df) <- names(intersections)
  
  write.csv(intersection_df, file = filename, row.names = FALSE, na = "")
  message(paste("Results saved to", filename))
}
