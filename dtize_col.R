dtize_col <- function (column,
                       splits="median",
                       labels=c("low", "high"),
                       right=TRUE,
                       infinity=TRUE,
                       lowest=TRUE,
                       na_fill="none") {
  
  # check if all logical parameters have acceptable values
  if (invalid_logical(right))
    stop("`right` must be either TRUE or FALSE.")
  if (invalid_logical(infinity))
    stop("`infinity` must be either TRUE or FALSE.")
  if (invalid_logical(lowest))
    stop("`lowest` must be either TRUE or FALSE.")
  
  # validate that input column is a non-empty, numeric vector
  if(invalid_vector(column))
    stop("`column` must be a non-empty numeric vector.")
  
  # ensure function is case insensitive
  if (is.character(splits))
    splits <- tolower(splits)
  
  # validate that split is a nonempty, non-NA numeric vector or "mean/median"
  # MAKE SURE NO NAS
  if (identical(splits, "median")) {
    cutoffs <- median(column, na.rm = TRUE)
  } else if (identical(splits, "mean")) {
    cutoffs <- mean(column, na.rm = TRUE)
  } else if (!invalid_vector(splits)) {
    if(any(is.na(splits)))
      stop("`splits` cannot contain NA values.")
    cutoffs <- splits
  } else {
    stop("`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  }
  
  if(any(duplicated(cutoffs)))
    stop("`split` cannot contain duplicate values. Please ensure all values are unique.")
  if(any(is.infinite(cutoffs)) && infinity==TRUE)
    stop("`splits` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `splits`.")
  
  # make sure cutoffs are sorted in increasing order
  cutoffs <- sort(cutoffs)
  
  # add infinite bounds if user selects this option
  if (infinity) {
    cutoffs <- c(-Inf, cutoffs, Inf)
  } else {
    
    # check that there are at least two cutoff points
    if(length(cutoffs) < 2)
      stop("Please provide at least two split points if infinity is FALSE.")
    
    # provide warning if values are beyond upper or lower bounds (or else NAs will occur)
    if(right){
      if(max(column) > max(cutoffs))
        stop("Values in `column` exceed the maximum split. Please ensure all values are within the defined range.")
    }else{
      if(max(column) >= max(cutoffs))
        stop("Values in `column` exceed the maximum split. Please ensure all values are within the defined range.")
    }
    
    if(lowest || !right){
      if(min(column) < min (cutoffs))
        stop("Values in `column` fall below the minimum split. Please ensure all values are within the defined range.")
    }else{
      if(min(column) <= min (cutoffs))
        stop("Values in `column` fall below the minimum split. Please ensure all values are within the defined range.")        
    }
    
  }
  
  #check that labels doesn't contain null or NAs
  if(is.null(labels))
    stop("`labels` cannot be NULL. Please provide valid labels for the intervals.")
  if(any(is.na(labels)))
    stop("`labels` contains NA values. Please provide non-NA labels for the intervals.")
  
  # check that number of labels matches number of intervals
  num_labels = length(labels)
  num_intervals = length(cutoffs) - 1
  if (num_intervals != num_labels) 
    stop(sprintf("%d labels required for discretisation, but %d given. Please provide one label for each interval.", num_intervals, num_labels))
  
  # ensure na_fill is case insensitive 
  if(is.character(na_fill))
    tolower(na_fill)
  
  # impute missing values
  if(any(is.na(column))){
    if(identical(na_fill, "none")){
      warning("`column` contains NA values, but no imputation method was chosen (`na_fill = 'none'`). NA values will remain in the output.")
    }else if(identical(na_fill, "mean")){
      column <- (ifelse(is.na(column), mean(column, na.rm = TRUE), column))
    }else if(identical(na_fill, "median")){
      column <- (ifelse(is.na(column), median(column, na.rm = TRUE), column))      
    }else if(identical(na_fill, "pmm")){
      temp_column <- mice(data.frame(column), method="pmm")
      column <- complete(temp_column)
    }else{
      stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
    }   
  }
  
  # call helper   
  return(cut(column,
             breaks = cutoffs,
             labels = labels, 
             right = right,  
             include.lowest = lowest))
  
}

# helper function to check if logical arguments (right, infinity, lowest) are valid types
invalid_logical <- function (input){
  length(input) != 1 || !is.logical(input) || is.na(input)
}

# helper function to check if vector arguments (column and splits) are valid types
invalid_vector <- function(input){
  !is.vector(input) || !is.numeric(input) || length(input) == 0
}
