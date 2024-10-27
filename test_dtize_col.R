library(testthat)

source("./dtize_col.R")

####### TEST INPUTS #######

# vectors
valid_vec <-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
na_vec <- c(1, 2, 3, 4, 5, NA, 7, 8, 9, NA)
non_num_vec <- c("Zurich", "Budapest", "Vienna", "Frankfurt", "Prague")
empty_vec <- numeric(0)
inf_vec <- c(-Inf, Inf)

# other data types
test_matrix <- matrix(1:4, nrow = 2, ncol = 2)
test_list <- list(a=1, b=2, c='Jerry')
test_df <- data.frame(
  column1 = c(1, 2, 3, 4, 5),
  column2 = c('A', 'B', 'C', 'D', 'E'),
  column3 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

####### TESTS #######

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
