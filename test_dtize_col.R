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


