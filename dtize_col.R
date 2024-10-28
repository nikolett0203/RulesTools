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
  
  # validate that split is a nonempty, non-NA numeric vector or "mean/median"
  # MAKE SURE NO NAS
  cutoffs <- check_invalid_splits(column, splits, infinity)
  
  if(any(duplicated(cutoffs)))
    stop("`split` cannot contain duplicate values. Please ensure all values are unique.")

  # make sure cutoffs are sorted in increasing order
  cutoffs <- sort(cutoffs)
  
  # add infinite bounds if user selects this option
  if (infinity) {
    if(any(is.infinite(cutoffs)))
       stop("`splits` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `splits`.")
    cutoffs <- c(-Inf, cutoffs, Inf)
  } else {
    check_invalid_bounds(column, cutoffs, right, lowest)
  }
  
  check_invalid_labels(labels, cutoffs)
  
  # fill na values
  filled_column <- impute_na(column, na_fill)
  
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



# helper function to impute missing values
# what happens if values are infinite?
impute_na <- function(column, na_fill){
  
  # ensure na_fill is case insensitive 
  if(is.character(na_fill))
    na_fill <- tolower(na_fill)
  
  if(!any(is.na(column)))
    return(column)
  
  if(identical(na_fill, "none")){
    warning("`column` contains NA values, but no imputation method was chosen (`na_fill = 'none'`). NA values will remain in the output.")
    return(column)
  }else if(identical(na_fill, "mean")){
    return((ifelse(is.na(column), mean(column, na.rm = TRUE), column)))
  }else if(identical(na_fill, "median")){
    return((ifelse(is.na(column), median(column, na.rm = TRUE), column)))      
  }else if(identical(na_fill, "pmm")){
    temp_column <- mice(data.frame(column), method="pmm")
    return(complete(temp_column))
  }else{
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
  }    
  
}


# helper function to check whether cutoff points produce valid splits
check_invalid_bounds <- function(column, cutoffs, right, lowest){
  
  # check that there are at least two cutoff points
  if(length(cutoffs) < 2)
    stop("Please provide at least two split points if infinity is FALSE.")
  
  max_col <- max(column, na.rm=TRUE)
  min_col <- min(column, na.rm=TRUE)
  max_cutoffs <- max(cutoffs)
  min_cutoffs <- min(cutoffs)
  
  # provide warning if values are beyond upper or lower bounds (or else NAs will occur)
  if(right){
    if(max_col > max_cutoffs)
      stop("Values in `column` exceed the maximum split. Please ensure all values are within the defined range.")
  }else{
    if(max_col >= max_cutoffs)
      stop("Values in `column` exceed the maximum split. Please ensure all values are within the defined range.")
  }
  
  if(lowest || !right){
    if(min_col < min_cutoffs)
      stop("Values in `column` fall below the minimum split. Please ensure all values are within the defined range.")
  }else{
    if(min_col <= min_cutoffs)
      stop("Values in `column` fall below the minimum split. Please ensure all values are within the defined range.")        
  }
  
}


check_invalid_labels <- function(labels, cutoffs){
  
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
  
}


## if median is chosen we should just put the lowest value as the split as long as infinity is not chosen


check_invalid_splits <- function(column, splits, infinity){
  
  # ensure function is case insensitive
  if (is.character(splits))
    splits <- tolower(splits)
  
  if (identical(splits, "median")) {
    if(infinity){
      return(median(column, na.rm = TRUE))
    } else{
      return(c(min(column, na.rm = TRUE), median(column, na.rm = TRUE), max(column, na.rm = TRUE)))
    }
  } else if (identical(splits, "mean")) {
    if(infinity){
      return(mean(column, na.rm = TRUE))
    } else{
      return(c(min(column, na.rm = TRUE), mean(column, na.rm = TRUE), max(column, na.rm = TRUE)))
    }
  } else if (!invalid_vector(splits)) {
    if(any(is.na(splits)))
      stop("`splits` cannot contain NA values.")
    return(splits)
  } else {
    stop("`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  }
  
}
