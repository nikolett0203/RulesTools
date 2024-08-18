dtize <- function (data, split, new_df) {
  
  for(col in colnames(split)){
    cut <- c(-Inf, split[[col]], Inf)
    newcol <- discretize(data[[col]], method="fixed", breaks=cut, right=TRUE, labels = c("low", "high"))
    new_df[[col]] <- newcol
  }
  
  return(new_df)
}





# compare two rules
rule_by_two <- function (rules1, rules2) {
  
  # add error checking, i.e. what if no rules are in common, valid rule objects
  
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





# compare indefinite number of rules
rule_by_rule <- function (...) {
  
  # collect arguments
  rules <- list(...)
  
  # check if all arguments are rules
  if(!all(sapply(rules, inherits, "rules"))) {
    stop("Arguments must be objects of class 'rules'")
  }
  
  # make sure user gives names
  if (is.null(names(rules)) || any(names(rules) == "")) {
    stop("Please provide names for all arguments.")
  }
  
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
  
}

