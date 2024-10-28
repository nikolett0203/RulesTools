library(testthat)

source("./dtize_col.R")

####### TEST INPUTS #######

# vectors
valid_vec <-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
na_vec <- c(1, 2, 3, 4, 5, NA, 7, 8, 9, NA)
non_num_vec <- c("Zurich", "Budapest", "Vienna", "Frankfurt", "Prague")
empty_vec <- numeric(0)
inf_vec <- c(-Inf, Inf)
split_vec <- c(1, 4, 8, 10)
mixed_inf_vec <-c(-Inf, 5, 10)
mixed_inf_vec2 <-c(1, 5, Inf)

# other data types
test_matrix <- matrix(1:4, nrow = 2, ncol = 2)
test_list <- list(a=1, b=2, c='Jerry')
test_df <- data.frame(
  column1 = c(1, 2, 3, 4, 5),
  column2 = c('A', 'B', 'C', 'D', 'E'),
  column3 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

####### TESTS #######

test_that("invalid_logical() handles invalid inputs for right, infinity, and lowest correctly", {
  
  # non-logical values
  expect_true(invalid_logical(NULL))
  expect_true(invalid_logical(100))
  expect_true(invalid_logical("wildflower"))
  expect_true(invalid_logical(test_list))
  
  # length != 1
  expect_true(invalid_logical(c(TRUE, FALSE)))
  expect_true(invalid_logical(logical(0)))
  
  # NA values
  expect_true(invalid_logical(NA))
  
  # valid inputs
  expect_false(invalid_logical(TRUE))
  expect_false(invalid_logical(FALSE))
  
})

test_that("dtize_col verifies logical parameters correctly",{
  
  # non-logical values
  expect_error(dtize_col(valid_col, right=test_df),
               regexp=("`right` must be either TRUE or FALSE."))
  expect_error(dtize_col(valid_col, infinity=Inf),
               regexp=("`infinity` must be either TRUE or FALSE."))
  expect_error(dtize_col(valid_col, lowest="world is so small till it ain't"),
               regexp=("`lowest` must be either TRUE or FALSE."))
  
  # length > 1 or <1
  expect_error(dtize_col(valid_col, infinity=logical(0)),
               regexp=("`infinity` must be either TRUE or FALSE."))
  expect_error(dtize_col(valid_col, lowest=c(TRUE, TRUE, FALSE, TRUE)),
               regexp=("`lowest` must be either TRUE or FALSE."))  
  
  # NA values
  expect_error(dtize_col(valid_col, right=c(NA, NA)),
               regexp=("`right` must be either TRUE or FALSE."))
  expect_error(dtize_col(valid_col, infinity=NA),
               regexp=("`infinity` must be either TRUE or FALSE."))
  
  # acceptable values
  expect_no_error(dtize_col(valid_vec, right=FALSE))
  expect_no_error(dtize_col(7, lowest=FALSE))
  expect_no_error(dtize_col(na_vec, na_fill = "median"))
  
})





# should return true when vector is invalid
test_that("invalid_vector() handles invalid column inputs correctly", {
  
  expect_true(invalid_vector(test_df))
  expect_true(invalid_vector(test_matrix))
  expect_true(invalid_vector(test_list))
  expect_true(invalid_vector(non_num_vec))
  expect_true(invalid_vector(empty_vec))
  expect_true(invalid_vector(NULL))
  expect_true(invalid_vector(NA))

  expect_false(invalid_vector(valid_vec))
  expect_false(invalid_vector(0))
  expect_false(invalid_vector(inf_vec))
  expect_false(invalid_vector(na_vec))
  
})

# should print error message when column inputs are invalid
test_that("dtize_col handles invalid column inputs correctly", {
  
  expect_error(dtize_col(test_df), 
               regexp = "`column` must be a non-empty numeric vector.")
  expect_error(dtize_col(test_matrix), 
               regexp = "`column` must be a non-empty numeric vector.")
  expect_error(dtize_col(test_list), 
               regexp = "`column` must be a non-empty numeric vector.")
  expect_error(dtize_col(non_num_vec), 
               regexp = "`column` must be a non-empty numeric vector.")
  expect_error(dtize_col(empty_vec), 
               regexp = "`column` must be a non-empty numeric vector.")
  expect_error(dtize_col(NULL), 
               regexp = "`column` must be a non-empty numeric vector.")
  expect_error(dtize_col(NA), 
               regexp = "`column` must be a non-empty numeric vector.")
  
  expect_no_error(dtize_col(valid_vec))
  expect_no_error(dtize_col(0))
  expect_no_error(dtize_col(inf_vec, labels=c("infinity")))
  #expect_no_error(dtize_col(na_vec))
  
})

test_that("check_invalid_splits() accepts only valid cutoff vectors",{
  
  # check for invalid input types
  expect_error(check_invalid_splits(valid_vec, test_matrix, FALSE),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(check_invalid_splits(valid_vec, non_num_vec, TRUE),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(check_invalid_splits(valid_vec, empty_vec, FALSE),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(check_invalid_splits(valid_vec, NULL, TRUE),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(check_invalid_splits(valid_vec, NA, FALSE),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(check_invalid_splits(valid_vec, "mode", TRUE),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  
  expect_error(check_invalid_splits(valid_vec, na_vec, FALSE),
               regexp = "`splits` cannot contain NA values.")
  
  expect_equal(check_invalid_splits(valid_vec, "median", FALSE), c(1, 5.5, 10))
  expect_equal(check_invalid_splits(na_vec, "mEdIaN", TRUE), 4.5)
  expect_equal(check_invalid_splits(valid_vec, "MeAn", FALSE), c(1, 5.5, 10))
  expect_equal(check_invalid_splits(na_vec, "mean", TRUE), 4.875)
  expect_equal(check_invalid_splits(valid_vec, split_vec, FALSE), split_vec)
  
})


test_that("dtize_col handles invalid split types correctly", {
  
  #test if function throws an error when column is not a vector
  expect_error(dtize_col(valid_vec, splits = "mode"), 
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(dtize_col(valid_vec, splits = non_num_vec), 
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(dtize_col(valid_vec, splits = empty_vec), 
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(dtize_col(valid_vec, splits = test_df),          # not a vector
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")  
  expect_error(dtize_col(valid_vec, splits = NULL),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")    
  expect_error(dtize_col(valid_vec, splits = na_vec),
               regexp = "`splits` cannot contain NA values.")  
  
  expect_equal(dtize_col(valid_vec, splits="MEDIAN", right=TRUE, lowest=TRUE, infinity=FALSE), 
               factor(c("low", "low", "low", "low", "low", "high", "high", "high", "high", "high"), 
                      levels = c("low", "high")))
  expect_equal(
    suppressWarnings(dtize_col(na_vec, splits = "medIAN", infinity = TRUE, na_fill = "none")), 
    factor(c("low", "low", "low", "low", "high", NA, "high", "high", "high", NA), 
           levels = c("low", "high"))
  )
  expect_equal(dtize_col(valid_vec, splits="MeAN", right=TRUE, lowest=TRUE, infinity=FALSE), 
               factor(c("low", "low", "low", "low", "low", "high", "high", "high", "high", "high"), 
                      levels = c("low", "high")))
  expect_equal(
    suppressWarnings(dtize_col(na_vec, splits = "medIAN", infinity = TRUE, na_fill = "none")), 
    factor(c("low", "low", "low", "low", "high", NA, "high", "high", "high", NA), 
           levels = c("low", "high"))
  )
  expect_equal(dtize_col(valid_vec, splits=split_vec, labels=c("low", "med", "high"), right=TRUE, lowest=TRUE, infinity=FALSE), 
               factor(c("low", "low", "low", "low", "med", "med", "med", "med", "high", "high"), 
                      levels = c("low", "med", "high")))
  
})

test_that("dtize_col checks for duplicate split values",{ 
  
  expect_error(dtize_col(valid_vec, splits=c(1,1,10), infinity=FALSE),
               regexp=("`split` cannot contain duplicate values. Please ensure all values are unique."))
  expect_error(dtize_col(valid_vec, splits=c(5,5,5,5), labels=c("one", "two", "three", "four", "five")),
               regexp=("`split` cannot contain duplicate values. Please ensure all values are unique."))
  # if infinity is TRUE but user also has infinity in splits
  expect_error(dtize_col(valid_vec, splits=c(-Inf, Inf), labels=c("1", "2", "3")),
               regexp=("`splits` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `splits`."))
  
})

test_that("check_invalid_bounds() handles exceeded boundaries correctly",{
  
  expect_error(check_invalid_bounds(column=valid_vec, cutoffs=5, right=TRUE, lowest=TRUE),
               regexp = ("Please provide at least two split points if infinity is FALSE."))
  expect_error(check_invalid_bounds(column=na_vec, cutoffs=c(1, 5, 8), right=TRUE, lowest=TRUE),
               regexp = ("Values in `column` exceed the maximum split. Please ensure all values are within the defined range."))
  expect_error(check_invalid_bounds(column=valid_vec, cutoffs=c(1,5,10), right=FALSE, lowest=TRUE),
               regexp = ("Values in `column` exceed the maximum split. Please ensure all values are within the defined range."))
  expect_error(check_invalid_bounds(column=valid_vec, cutoffs=c(1,5,10), right=TRUE, lowest=FALSE),
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  expect_error(check_invalid_bounds(column=valid_vec, cutoffs=c(2,5,11), right=FALSE, lowest=FALSE),
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  expect_error(check_invalid_bounds(column=valid_vec, cutoffs=c(2,5,10), right=TRUE, lowest=TRUE),
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  
  expect_no_error(check_invalid_bounds(column=na_vec, cutoffs=c(1, 5, 10), right=TRUE, lowest=TRUE))
  expect_no_error(check_invalid_bounds(column=na_vec, cutoffs=c(1, 5, 11), right=FALSE, lowest=FALSE))
  expect_no_error(check_invalid_bounds(column=na_vec, cutoffs=c(0, 5, 10), right=TRUE, lowest=FALSE))
  expect_no_error(check_invalid_bounds(column=na_vec, cutoffs=c(1, 5, 11), right=FALSE, lowest=TRUE))
  
  })



test_that("dtize_col handles boundaries correctly", {
  
  #right-closed finite upper boundary exceeded
  expect_error(dtize_col(valid_vec, splits = c(1, 5, 9), right=TRUE, infinity=FALSE), 
               regexp = ("Values in `column` exceed the maximum split. Please ensure all values are within the defined range."))
  #left-closed finite upper boundary exceeded
  expect_error(dtize_col(valid_vec, splits = c(1, 5, 10), right=FALSE, infinity=FALSE), 
               regexp = ("Values in `column` exceed the maximum split. Please ensure all values are within the defined range."))
  #right-closed finite lower boundary exceeded
  expect_error(dtize_col(valid_vec, splits = c(1, 5, 11), right=TRUE, infinity=FALSE, lowest = FALSE), 
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  #left-closed finite lower boundary exceeded
  expect_error(dtize_col(valid_vec, splits = c(2, 5, 11), right=FALSE, infinity=FALSE, lowest = FALSE), 
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  #right-closed + lowest included lower boundary exceeded
  expect_error(dtize_col(valid_vec, splits = c(2, 5, 11), right=TRUE, infinity=FALSE, lowest = TRUE), 
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  #left-closed + lowest included lower boundary exceeded
  expect_error(dtize_col(valid_vec, splits = c(2, 5, 11), right=FALSE, infinity=FALSE, lowest = TRUE), 
               regexp = ("Values in `column` fall below the minimum split. Please ensure all values are within the defined range."))
  #only one boundary, no infinity
  expect_error(dtize_col(valid_vec, splits = c(7), right=TRUE, infinity=FALSE, lowest=TRUE), 
               regexp = ("Please provide at least two split points if infinity is FALSE.")) 
  
  #infinity vector with infinity=TRUE
  expect_error(dtize_col(valid_vec, splits=mixed_inf_vec, infinity=TRUE),
               regexp=("`splits` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `splits`."))
  expect_error(dtize_col(valid_vec, splits=mixed_inf_vec2, infinity=TRUE),
               regexp=("`splits` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `splits`."))
  expect_error(dtize_col(valid_vec, splits=inf_vec, labels="only", infinity=TRUE),
               regexp=("`splits` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `splits`."))
  
})

test_that("check_invalid_labels() handles weird labelling correctly",{
  
  expect_error(check_invalid_labels(NULL, cutoffs=split_vec),
               regexp=("`labels` cannot be NULL. Please provide valid labels for the intervals."))
  expect_error(check_invalid_labels(labels=c(NA, NA, NA), cutoffs=split_vec),
               regexp=("`labels` contains NA values. Please provide non-NA labels for the intervals."))
  expect_error(check_invalid_labels(labels=empty_vec, cutoffs=split_vec),
               regexp=("3 labels required for discretisation, but 0 given. Please provide one label for each interval."))
  expect_error(check_invalid_labels(labels=valid_vec, cutoffs=split_vec),
               regexp=("3 labels required for discretisation, but 10 given. Please provide one label for each interval."))  
  expect_error(check_invalid_labels(labels=test_df, cutoffs=split_vec),
               regexp=("`labels` must be a vector."))  
    
  expect_no_error(check_invalid_labels(labels=c("spongebob", "patrick", "squidward"), cutoffs=split_vec))
  expect_no_error(check_invalid_labels(labels=c("orion"), cutoffs=inf_vec))
  expect_no_error(check_invalid_labels(labels=c(TRUE, FALSE), cutoffs=c(1, 5, 10)))
  expect_no_error(check_invalid_labels(labels=c(1, 2, 3), cutoffs=split_vec))
  
})

test_that("dtize_col handles mismatched labels correctly", {
  
  # too few labels
  expect_error(dtize_col(valid_vec, splits = split_vec, labels=c("high"), right=TRUE, infinity=FALSE), 
               regexp = ("3 labels required for discretisation, but 1 given. Please provide one label for each interval."))
  # too many labels
  expect_error(dtize_col(valid_vec, splits = split_vec, labels=c("low", "medium", "high", "extra high"), right=TRUE, infinity=FALSE), 
               regexp = ("3 labels required for discretisation, but 4 given. Please provide one label for each interval."))
  # no labels
  expect_error(dtize_col(valid_vec, splits = split_vec, labels=NULL, right=TRUE, infinity=FALSE), 
               regexp = ("`labels` cannot be NULL. Please provide valid labels for the intervals."))
  # na labels
  expect_error(dtize_col(valid_vec, splits = split_vec, labels=NA, right=TRUE, infinity=FALSE), 
               regexp = ("`labels` contains NA values. Please provide non-NA labels for the intervals."))
  
  # valid labels
  expect_no_error(dtize_col(valid_vec, splits = split_vec, labels=c("low", "medium", "high"), right=TRUE, infinity=FALSE))
  expect_no_error(dtize_col(valid_vec, splits = split_vec, labels=c("one", "two", "three", "four", "five"), right=TRUE, infinity=TRUE))  

})

