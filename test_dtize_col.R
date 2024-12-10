library(testthat)
library(mice)

source("./dtize_col.R")

####### TEST INPUTS #######

# vectors
valid_vec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
na_vec <- c(1, 2, 3, 4, 5, NA, 7, 8, 9, NA)
non_num_vec <- c("Zurich", "Budapest", "Vienna", "Frankfurt", "Prague")
empty_vec <- numeric(0)
inf_vec <- c(-Inf, Inf)
split_vec <- c(1, 4, 8, 10)
mixed_inf_vec <- c(-Inf, 5, 10)
mixed_inf_vec2 <- c(1, 5, Inf)

# other data types
test_matrix <- matrix(1:4, nrow = 2, ncol = 2)
test_list <- list(a = 1, b = 2, c = 'Jerry')
test_df <- data.frame(
  column1 = c(1, 2, 3, 4, 5),
  column2 = c('A', 'B', 'C', 'D', 'E'),
  column3 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

####### TESTS #######

test_that("invalid_logical() handles invalid inputs for include_right, infinity, and include_lowest correctly", {
  
  # non-logical values
  expect_true(check_invalid_logical(NULL))
  expect_true(check_invalid_logical(100))
  expect_true(check_invalid_logical("wildflower"))
  expect_true(check_invalid_logical(test_list))
  
  # length != 1
  expect_true(check_invalid_logical(c(TRUE, FALSE)))
  expect_true(check_invalid_logical(logical(0)))
  
  # NA values
  expect_true(check_invalid_logical(NA))
  
  # valid inputs
  expect_false(check_invalid_logical(TRUE))
  expect_false(check_invalid_logical(FALSE))
})


test_that("dtize_col() verifies logical parameters correctly", {
  
  # non-logical values
  expect_error(
    dtize_col(valid_col, include_right = test_df),
    regexp = "`include_right` must be either TRUE or FALSE."
  )
  expect_error(
    dtize_col(valid_col, infinity = Inf),
    regexp = "`infinity` must be either TRUE or FALSE."
  )
  expect_error(
    dtize_col(valid_col, include_lowest = "world is so small till it ain't"),
    regexp = "`include_lowest` must be either TRUE or FALSE."
  )
  
  # length > 1 or < 1
  expect_error(
    dtize_col(valid_col, infinity = logical(0)),
    regexp = "`infinity` must be either TRUE or FALSE."
  )
  expect_error(
    dtize_col(valid_col, include_lowest = c(TRUE, TRUE, FALSE, TRUE)),
    regexp = "`include_lowest` must be either TRUE or FALSE."
  )  
  
  # NA values
  expect_error(
    dtize_col(valid_col, include_right = c(NA, NA)),
    regexp = "`include_right` must be either TRUE or FALSE."
  )
  expect_error(
    dtize_col(valid_col, infinity = NA),
    regexp = "`infinity` must be either TRUE or FALSE."
  )
  
  # acceptable values
  expect_no_error(dtize_col(valid_vec, include_right = FALSE))
  expect_no_error(dtize_col(7, include_lowest = FALSE))
  expect_no_error(dtize_col(na_vec, na_fill = "median"))
})


test_that("check_invalid_vector() handles invalid column inputs correctly", {
  
  # non-vector, non-numeric, and/or empty values
  expect_true(check_invalid_vector(test_df))
  expect_true(check_invalid_vector(test_matrix))
  expect_true(check_invalid_vector(test_list))
  expect_true(check_invalid_vector(non_num_vec))
  expect_true(check_invalid_vector(empty_vec))
  expect_true(check_invalid_vector(NULL))
  expect_true(check_invalid_vector(NA))
  
  # valid vectors
  expect_false(check_invalid_vector(valid_vec))
  expect_false(check_invalid_vector(0))
  expect_false(check_invalid_vector(inf_vec))
  expect_false(check_invalid_vector(na_vec))
})


test_that("dtize_col() handles invalid column inputs correctly", {
  
  # empty, non-numeric, non-vector values
  expect_error(
    dtize_col(test_df), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  expect_error(
    dtize_col(test_matrix), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  expect_error(
    dtize_col(test_list), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  expect_error(
    dtize_col(non_num_vec), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  expect_error(
    dtize_col(empty_vec), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  expect_error(
    dtize_col(NULL), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  expect_error(
    dtize_col(NA), 
    regexp = "`column` must be a non-empty numeric vector."
  )
  
  # valid inputs
  expect_no_error(dtize_col(valid_vec))
  expect_no_error(dtize_col(0))
  expect_no_error(dtize_col(inf_vec, labels = c("infinity")))
})


test_that("check_invalid_cutoff() accepts only valid cutoff vectors", {
  
  # invalid cutoff inputs
  expect_error(
    check_invalid_cutoff(valid_vec, test_matrix, FALSE),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    check_invalid_cutoff(valid_vec, non_num_vec, TRUE),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    check_invalid_cutoff(valid_vec, empty_vec, FALSE),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    check_invalid_cutoff(valid_vec, NULL, TRUE),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    check_invalid_cutoff(valid_vec, NA, FALSE),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    check_invalid_cutoff(valid_vec, "mode", TRUE),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  
  # check for NAs in vector cutoffs
  expect_error(
    check_invalid_cutoff(valid_vec, na_vec, FALSE),
    regexp = "`cutoff` cannot contain NA values."
  )
  
  # ensure mean and median are calculated correctly
  expect_equal(check_invalid_cutoff(valid_vec, "median", FALSE), c(1, 5.5, 10))
  expect_equal(check_invalid_cutoff(na_vec, "mEdIaN", TRUE), 4.5)
  expect_equal(check_invalid_cutoff(valid_vec, "MeAn", FALSE), c(1, 5.5, 10))
  expect_equal(check_invalid_cutoff(na_vec, "mean", TRUE), 4.875)
  expect_equal(check_invalid_cutoff(valid_vec, split_vec, FALSE), split_vec)
})


test_that("dtize_col() handles invalid cutoff types correctly", {
  
  # test if function throws an error when column is not a vector
  expect_error(
    dtize_col(valid_vec, cutoff = "mode"), 
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    dtize_col(valid_vec, cutoff = non_num_vec), 
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    dtize_col(valid_vec, cutoff = empty_vec), 
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    dtize_col(valid_vec, cutoff = test_df),  # Not a vector
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    dtize_col(valid_vec, cutoff = NULL),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    dtize_col(valid_vec, cutoff = na_vec),
    regexp = "`cutoff` cannot contain NA values."
  )
  
  # check that cutoff types produce the correct discretised vectors
  # median
  expect_equal(
    dtize_col(valid_vec, cutoff = "MEDIAN", include_right = TRUE, include_lowest = TRUE, infinity = FALSE), 
    factor(c("low", "low", "low", "low", "low", "high", "high", "high", "high", "high"), levels = c("low", "high"))
  )
  expect_equal(
    suppressWarnings(dtize_col(na_vec, cutoff = "medIAN", infinity = TRUE, na_fill = "none")), 
    factor(c("low", "low", "low", "low", "high", NA, "high", "high", "high", NA), levels = c("low", "high"))
  )
  
  # mean
  expect_equal(
    dtize_col(valid_vec, cutoff = "MeAN", include_right = TRUE, include_lowest = TRUE, infinity = FALSE), 
    factor(c("low", "low", "low", "low", "low", "high", "high", "high", "high", "high"), levels = c("low", "high"))
  )
  
  # custom
  expect_equal(
    dtize_col(valid_vec, cutoff = split_vec, labels = c("low", "med", "high"), include_right = TRUE, include_lowest = TRUE, infinity = FALSE), 
    factor(c("low", "low", "low", "low", "med", "med", "med", "med", "high", "high"), levels = c("low", "med", "high"))
  )
})


test_that("dtize_col checks for duplicate cutoff values", { 
  
  expect_error(
    dtize_col(valid_vec, cutoff = c(1, 1, 10), infinity = FALSE),
    regexp = "`cutoff` cannot contain duplicate values. Please ensure all values are unique."
  )
  expect_error(
    dtize_col(valid_vec, cutoff = c(5, 5, 5, 5), labels = c("one", "two", "three", "four", "five")),
    regexp = "`cutoff` cannot contain duplicate values. Please ensure all values are unique."
  )
  
  # if infinity is TRUE but user also has infinity in cutoff
  expect_error(
    dtize_col(valid_vec, cutoff = c(-Inf, Inf), labels = c("1", "2", "3")),
    regexp = "`cutoff` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `cutoff`."
  )
})


test_that("check_invalid_bounds() handles exceeded boundaries correctly", {
  
  # insufficient number of cutoff points
  expect_error(
    check_invalid_bounds(column = valid_vec, cutoffs = 5, include_right = TRUE, include_lowest = TRUE),
    regexp = "Please provide at least two cutoff points if infinity is FALSE."
  )
  
  # cutoff doesn't capture all values
  expect_error(
    check_invalid_bounds(column = na_vec, cutoffs = c(1, 5, 8), include_right = TRUE, include_lowest = TRUE),
    regexp = "Values in `column` exceed the maximum cutoff. Please ensure all values are within the defined range."
  )
  expect_error(
    check_invalid_bounds(column = valid_vec, cutoffs = c(1, 5, 10), include_right = FALSE, include_lowest = TRUE),
    regexp = "Values in `column` exceed the maximum cutoff. Please ensure all values are within the defined range."
  )
  expect_error(
    check_invalid_bounds(column = valid_vec, cutoffs = c(1, 5, 10), include_right = TRUE, include_lowest = FALSE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  expect_error(
    check_invalid_bounds(column = valid_vec, cutoffs = c(2, 5, 11), include_right = FALSE, include_lowest = FALSE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  expect_error(
    check_invalid_bounds(column = valid_vec, cutoffs = c(2, 5, 10), include_right = TRUE, include_lowest = TRUE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  
  # valid cutoffs
  expect_no_error(check_invalid_bounds(column = na_vec, cutoffs = c(1, 5, 10), include_right = TRUE, include_lowest = TRUE))
  expect_no_error(check_invalid_bounds(column = na_vec, cutoffs = c(1, 5, 11), include_right = FALSE, include_lowest = FALSE))
  expect_no_error(check_invalid_bounds(column = na_vec, cutoffs = c(0, 5, 10), include_right = TRUE, include_lowest = FALSE))
  expect_no_error(check_invalid_bounds(column = na_vec, cutoffs = c(1, 5, 11), include_right = FALSE, include_lowest = TRUE))
})


test_that("dtize_col handles boundaries correctly", {
  
  # right-closed finite upper boundary exceeded
  expect_error(
    dtize_col(valid_vec, cutoff = c(1, 5, 9), include_right = TRUE, infinity = FALSE),
    regexp = "Values in `column` exceed the maximum cutoff. Please ensure all values are within the defined range."
  )
  
  # left-closed finite upper boundary exceeded
  expect_error(
    dtize_col(valid_vec, cutoff = c(1, 5, 10), include_right = FALSE, infinity = FALSE),
    regexp = "Values in `column` exceed the maximum cutoff. Please ensure all values are within the defined range."
  )
  
  # right-closed finite lower boundary exceeded
  expect_error(
    dtize_col(valid_vec, cutoff = c(1, 5, 11), include_right = TRUE, infinity = FALSE, include_lowest = FALSE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  
  # left-closed finite lower boundary exceeded
  expect_error(
    dtize_col(valid_vec, cutoff = c(2, 5, 11), include_right = FALSE, infinity = FALSE, include_lowest = FALSE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  
  # right-closed + lowest included lower boundary exceeded
  expect_error(
    dtize_col(valid_vec, cutoff = c(2, 5, 11), include_right = TRUE, infinity = FALSE, include_lowest = TRUE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  
  # left-closed + lowest included lower boundary exceeded
  expect_error(
    dtize_col(valid_vec, cutoff = c(2, 5, 11), include_right = FALSE, infinity = FALSE, include_lowest = TRUE),
    regexp = "Values in `column` fall below the minimum cutoff. Please ensure all values are within the defined range."
  )
  
  # only one boundary, no infinity
  expect_error(
    dtize_col(valid_vec, cutoff = c(7), include_right = TRUE, infinity = FALSE, include_lowest = TRUE),
    regexp = "Please provide at least two cutoff points if infinity is FALSE."
  )
  
  # infinity vector with infinity = TRUE
  expect_error(
    dtize_col(valid_vec, cutoff = mixed_inf_vec, infinity = TRUE),
    regexp = "`cutoff` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `cutoff`."
  )
  
  expect_error(
    dtize_col(valid_vec, cutoff = mixed_inf_vec2, infinity = TRUE),
    regexp = "`cutoff` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `cutoff`."
  )
  
  expect_error(
    dtize_col(valid_vec, cutoff = inf_vec, labels = "only", infinity = TRUE),
    regexp = "`cutoff` cannot include -Inf or Inf when `infinity = TRUE`. Please remove infinite values from `cutoff`."
  )
})


test_that("check_invalid_labels() handles weird labelling correctly", {
  
  # non-existent labels
  expect_error(
    check_invalid_labels(NULL, cutoffs = split_vec),
    regexp = "`labels` cannot be NULL. Please provide valid labels for the intervals."
  )
  
  expect_error(
    check_invalid_labels(labels = c(NA, NA, NA), cutoffs = split_vec),
    regexp = "`labels` contains NA values. Please provide non-NA labels for the intervals."
  )
  
  # too few / too many labels
  expect_error(
    check_invalid_labels(labels = empty_vec, cutoffs = split_vec),
    regexp = "3 labels required for discretisation, but 0 given. Please provide one label for each interval."
  )
  
  expect_error(
    check_invalid_labels(labels = valid_vec, cutoffs = split_vec),
    regexp = "3 labels required for discretisation, but 10 given. Please provide one label for each interval."
  )
  
  # incorrect datatype for labels
  expect_error(
    check_invalid_labels(labels = test_df, cutoffs = split_vec),
    regexp = "`labels` must be a vector."
  )
  
  # valid labels
  expect_no_error(check_invalid_labels(labels = c("spongebob", "patrick", "squidward"), cutoffs = split_vec))
  expect_no_error(check_invalid_labels(labels = c("orion"), cutoffs = inf_vec))
  expect_no_error(check_invalid_labels(labels = c(TRUE, FALSE), cutoffs = c(1, 5, 10)))
  expect_no_error(check_invalid_labels(labels = c(1, 2, 3), cutoffs = split_vec))
})


test_that("dtize_col() handles mismatched labels correctly", {
  
  # too few labels
  expect_error(
    dtize_col(valid_vec, cutoff = split_vec, labels = c("high"), include_right = TRUE, infinity = FALSE),
    regexp = "3 labels required for discretisation, but 1 given. Please provide one label for each interval."
  )
  
  # too many labels
  expect_error(
    dtize_col(valid_vec, cutoff = split_vec, labels = c("low", "medium", "high", "extra high"), include_right = TRUE, infinity = FALSE),
    regexp = "3 labels required for discretisation, but 4 given. Please provide one label for each interval."
  )
  
  # no labels
  expect_error(
    dtize_col(valid_vec, cutoff = split_vec, labels = NULL, include_right = TRUE, infinity = FALSE),
    regexp = "`labels` cannot be NULL. Please provide valid labels for the intervals."
  )
  
  # NA labels
  expect_error(
    dtize_col(valid_vec, cutoff = split_vec, labels = NA, include_right = TRUE, infinity = FALSE),
    regexp = "`labels` contains NA values. Please provide non-NA labels for the intervals."
  )
  
  # valid labels
  expect_no_error(dtize_col(valid_vec, cutoff = split_vec, labels = c("low", "medium", "high"), include_right = TRUE, infinity = FALSE))
  expect_no_error(dtize_col(valid_vec, cutoff = split_vec, labels = c("one", "two", "three", "four", "five"), include_right = TRUE, infinity = TRUE))
})


test_that("impute_na() performs error checking and filling correctly", {
  
  # no NAs
  expect_equal(impute_na(valid_vec, na_fill = "medIAN"), valid_vec)
  
  # median
  expect_equal(
    impute_na(na_vec, na_fill = "meDian"), 
    c(1, 2, 3, 4, 5, 4.5, 7, 8, 9, 4.5)
  )
  
  # mean
  expect_equal(
    impute_na(na_vec, na_fill = "MEan"), 
    c(1, 2, 3, 4, 5, 4.875, 7, 8, 9, 4.875)
  )
  
  # infinite values
  expect_equal(
    impute_na(c(-Inf, 4, 4, 4, NA, Inf), na_fill = "MEan"), 
    c(-Inf, 4, 4, 4, 4, Inf)
  )
  
  # invalid imputation methods
  expect_error(
    impute_na(na_vec, na_fill = "dontfill"),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  expect_error(
    impute_na(na_vec, na_fill = NULL),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  expect_error(
    impute_na(na_vec, na_fill = NA),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  expect_error(
    impute_na(na_vec, na_fill = c("mean", "median", "pmm")),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
})


test_that("dtize_col() error checks na_fill inputs", {
  
  # invalid na_fill values
  expect_error(
    dtize_col(na_vec, na_fill = "dontfill"),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  expect_error(
    dtize_col(na_vec, na_fill = NULL),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  expect_error(
    dtize_col(na_vec, na_fill = NA),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  expect_error(
    dtize_col(na_vec, na_fill = c(1, 2, 3, 4)),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', or 'median'."
  )
  
  # valid na_fill values
  expect_identical(
    dtize_col(na_vec, na_fill = "MEAN"),
    factor(c("low", "low", "low", "low", "high", "high", "high", "high", "high", "high"), 
           levels = c("low", "high"))
  )
  
  expect_identical(
    dtize_col(na_vec, na_fill = "MeDiAn"),
    factor(c("low", "low", "low", "low", "high", "low", "high", "high", "high", "low"), 
           levels = c("low", "high"))
  )
  
  suppressWarnings(
    expect_identical(
      dtize_col(na_vec, na_fill = "NONE"),
      factor(c("low", "low", "low", "low", "high", NA, "high", "high", "high", NA), 
             levels = c("low", "high"))
    )
  )
})

