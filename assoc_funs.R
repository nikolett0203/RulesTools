# helper function to discretise data along provided splits
# @param data: dataframe that user intends to discretise
# @param split: vector containing split thresholds (must be written in order corresponding to column order in data)
# @param new_df: dataframe initialised with the number of columns needed for new discretised data
dtize <- function (data, split, new_df) {
  
  for(col in colnames(split)){
    cut <- c(-Inf, split[[col]], Inf)
    newcol <- discretize(data[[col]], method="fixed", breaks=cut, right=TRUE, labels = c("low", "high"))
    new_df[[col]] <- newcol
  }
  
  return(new_df)
}

dtize_col <- function (column,
                       splits="median",
                       labels=c("low", "high"),
                       right=TRUE,
                       infinity=TRUE){
  # error check that right and infinity are also okay?
  # handling na values
  
  # step 1: validate that input column is a non-empty, numeric vector
  if(!is.vector(column))
    stop("`column` must be a vector. Please provide a non-empty numeric vector.")
  if(!is.numeric(column))
    stop("`column` must be numeric. Please provide a non-empty numeric vector.")
  if (length(column) == 0) 
    stop("`column` is empty. Please provide a non-empty numeric vector.")

  # ensure function is case insensitive
  if (is.character(splits))
    splits <- tolower(splits)
  
  # validate split method
  if(splits=="median"){
    cutoffs <- median(column)
  } else if (splits=="mean"){
    cutoffs <- mean(column)
  } else if (is.vector(splits) && is.numeric(splits) && length(splits)!= 0){
    cutoffs <- splits
  } else {
    stop("`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  }
  
  # make sure cutoffs are sorted in increasing order

  splits <- sort(splits)
  
  # add infinite bounds if user selects this option
  if (infinity) {
    cutoffs <- c(-Inf, cutoffs, Inf)
  } else {
    
    # check that there are at least two cutoff points
    if(length(cutoffs) < 2)
      stop("Please provide at least two split points (upper and lower bound) if `infinity = FALSE`.")
    
    # provide warning if values are beyond upper or lower bounds
    # nas will occur
    if(right){
      if (min(column) <= min(cutoffs) || max(column) > max(cutoffs))
        warning("Some values fall outside the specified cutoff range. These values will be replaced by NA.")
    }else{
      if (min(column) < min(cutoffs) || max(column) >= max(cutoffs))
        warning("Some values fall outside the specified cutoff range. These values will be replaced by NA.")    
    }

  }
  
  # check that number of labels matches number of intervals
  
  
  
}






# function to compare two rulesets by displaying common rules and their interestingness values
# @param rules1: first rules object to be compared (must be named argument)
# @param rules2: second rules object to be compared (must be named argument)
rule_by_two <- function (rules1, rules2) {
  
  # isolate the rules
  labels1 <- labels(rules1)
  labels2 <- labels(rules2)
  
  # find common rules
  common <- intersect(labels1, labels2)
  
  # collect interestingness measures for each rule from each dataset
  crules1 <- rules1[labels1 %in% common]
  crules2 <- rules2[labels2 %in% common]
  quality1 <- quality(crules1)
  quality2 <- quality(crules2) 
  
  # store in dataframe
  df <- data.frame(
    Rules = common,
    Support_1 = quality1$support,
    Support_2 = quality2$support,
    Confidence_1 = quality1$confidence,
    Confidence_2 = quality2$confidence,
    Lift_1 = quality1$lift,
    Lift_2 = quality2$lift
  )  
  
  print(sprintf("Number of rules in common: %d", length(common)))
  print(df)

}

# function to compare indefinite number of rules by finding common rules and displaying their interestingness measures
# params: indefinite number of rules objects (must be named arguments)
rule_by_rule <- function (...) {
  
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
