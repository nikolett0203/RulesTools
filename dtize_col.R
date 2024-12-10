#library mice

#' Discretize a Numeric Column
#'
#' Discretizes a numeric vector into categories based on specified cutoff points.
#' The function handles missing values, allows for infinite bounds, and supports 
#' predefined cutoffs such as the mean or median.
#'
#' @param column A numeric vector to discretize.
#' @param cutoff A numeric vector specifying cutoff points, or a string ("mean" or "median").
#' @param labels A character vector specifying labels for the resulting categories.
#' @param include_right Logical. If `TRUE`, intervals are closed on the right (default `TRUE`).
#' @param infinity Logical. If `TRUE`, extends cutoffs to `-Inf` and `Inf` (default `TRUE`).
#' @param include_lowest Logical. If `TRUE`, the lowest interval is closed on the left (default `TRUE`).
#' @param na_fill A string specifying the method to impute missing values: "none", "mean", or "median" (default "none").
#'
#' @return A factor with the same length as `column`, where each value is categorized based on the cutoffs.
#' @examples
#' # Example with predefined cutoffs
#' dtize_col(c(1, 2, 3, 4, 5), cutoff = c(2, 4), labels = c("low", "medium", "high"))
#'
#' # Example with median as cutoff
#' dtize_col(c(1, 2, 3, 4, 5), cutoff = "median", labels = c("low", "high"))
#'
#' # Example with missing value imputation
#' dtize_col(c(1, 2, NA, 4, 5), cutoff = "mean", labels = c("low", "high"), na_fill = "mean")
#'
#' @export

dtize_col <- function(column,
                      cutoff = "median",
                      labels = c("low", "high"),
                      include_right = TRUE,
                      infinity = TRUE,
                      include_lowest = TRUE,
                      na_fill = "none") {
  
  # check if all logical parameters have acceptable values
  if (check_invalid_logical(include_right)) {
    stop("`include_right` must be either TRUE or FALSE.")
  }
  if (check_invalid_logical(infinity)) {
    stop("`infinity` must be either TRUE or FALSE.")
  }
  if (check_invalid_logical(include_lowest)) {
    stop("`include_lowest` must be either TRUE or FALSE.")
  }
  
  # validate that input column is a non-empty, numeric vector
  if (check_invalid_vector(column)) {
    stop("`column` must be a non-empty numeric vector.")
  }
  
  # validate that cutoff is a non-empty, non-NA numeric vector or 'mean'/'median'
  cutoffs <- check_invalid_cutoff(column, cutoff, infinity)
  
  if (any(duplicated(cutoffs))) {
    stop("`cutoff` cannot contain duplicate values. Please ensure all values are unique.")
  }
  
  # make sure cutoffs are sorted in increasing order
  cutoffs <- sort(cutoffs)
  
  # add infinite bounds if user selects this option
  if (infinity) {
    if (any(is.infinite(cutoffs))) {
      stop("`cutoff` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `cutoff`.")
    }
    cutoffs <- c(-Inf, cutoffs, Inf)
  } else {
    check_invalid_bounds(column, cutoffs, include_right, include_lowest)
  }
  
  check_invalid_labels(labels, cutoffs)
  
  # fill NA values
  filled_column <- impute_na(column, na_fill)
  
  return(cut(filled_column,
             breaks = cutoffs,
             labels = labels,
             right = include_right,
             include.lowest = include_lowest))
}


# helper function to check if logical arguments (include_right, infinity, include_lowest) are valid types
check_invalid_logical <- function(input) {
  length(input) != 1 || !is.logical(input) || is.na(input)
}


# helper function to check if vector arguments (column and cutoff) are valid types
check_invalid_vector <- function(input) {
  !is.vector(input) || !is.numeric(input) || length(input) == 0
}


# helper function to check whether cutoff points produce valid cutoff
check_invalid_bounds <- function(column, cutoffs, include_right, include_lowest) {
  
  # check that there are at least two cutoff points
  if (length(cutoffs) < 2) {
    stop("Please provide at least two cutoff points if infinity is FALSE.")
  }
  
  max_col <- max(column, na.rm = TRUE)
  min_col <- min(column, na.rm = TRUE)
  max_cutoffs <- max(cutoffs)
  min_cutoffs <- min(cutoffs)
  
  # provide warning if values are beyond upper or lower bounds (or else NAs will occur)
  if (include_right) {
    if (max_col > max_cutoffs) {
      stop("Values in `column` exceed the maximum cutoff. Please ensure all values are within the defined range.")
    }
  } else {
    if (max_col >= max_cutoffs) {
      stop("Values in `column` exceed the maximum cutoff. Please ensure all values are within the defined range.")
    }
  }
  
  if (include_lowest || !include_right) {
    if (min_col < min_cutoffs) {
      stop("Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range.")
    }
  } else {
    if (min_col <= min_cutoffs) {
      stop("Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range.")        
    }
  }
}


# helper function to check the validity of labels
check_invalid_labels <- function(labels, cutoffs) {
  
  # check that labels don't contain NULL or NAs
  if (is.null(labels)) {
    stop("`labels` cannot be NULL. Please provide valid labels for the intervals.")
  }
  if (any(is.na(labels))) {
    stop("`labels` contains NA values. Please provide non-NA labels for the intervals.")
  }
  if (!is.vector(labels)) {
    stop("`labels` must be a vector.")
  }
  
  # check that the number of labels matches the number of intervals
  num_labels <- length(labels)
  num_intervals <- length(cutoffs) - 1
  
  if (num_intervals != num_labels) {
    stop(sprintf(
      "%d labels required for discretisation, but %d given. Please provide one label for each interval.",
      num_intervals, num_labels
    ))
  }
}


# helper function to validate and generate cutoff points
check_invalid_cutoff <- function(column, cutoff, infinity) {
  
  # ensure function is case-insensitive
  if (is.character(cutoff)) {
    cutoff <- tolower(cutoff)
  }
  
  if (identical(cutoff, "median")) {
    if (infinity) {
      return(median(column, na.rm = TRUE))
    } else {
      return(c(min(column, na.rm = TRUE), median(column, na.rm = TRUE), max(column, na.rm = TRUE)))
    }
  } else if (identical(cutoff, "mean")) {
    if (infinity) {
      return(mean(column, na.rm = TRUE))
    } else {
      return(c(min(column, na.rm = TRUE), mean(column, na.rm = TRUE), max(column, na.rm = TRUE)))
    }
  } else if (!check_invalid_vector(cutoff)) {
    if (any(is.na(cutoff))) {
      stop("`cutoff` cannot contain NA values.")
    }
    return(cutoff)
  } else {
    stop("`cutoff` must be either `median`, `mean`, or a non-empty numeric vector.")
  }
}


# helper function to impute missing values
impute_na <- function(column, na_fill) {
  
  # ensure na_fill is case-insensitive
  if (is.character(na_fill)) {
    na_fill <- tolower(na_fill)
  }
  
  if (!any(is.na(column))) {
    return(column)
  }
  
  finite_values <- column[is.finite(column)]
  
  if (identical(na_fill, "none")) {
    warning("`column` contains NA values, but no imputation method was chosen (`na_fill = 'none'`). NA values will remain in the output.")
    return(column)
  } else if (identical(na_fill, "mean")) {
    return(ifelse(is.na(column), mean(finite_values, na.rm = TRUE), column))
  } else if (identical(na_fill, "median")) {
    return(ifelse(is.na(column), median(finite_values, na.rm = TRUE), column))
  } else {
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'.")
  }
}
