library(testthat)
library(mice)

source("./dtize_df.R")
source("./dtize_col.R")

test_df <- data.frame(
  column1 = c(1, 2, 3, 4, 5),
  column2 = c('A', 'B', 'C', 'D', 'E'),
  column3 = c(TRUE, FALSE, TRUE, FALSE, TRUE))

test_df2 <- data.frame(
  column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  column2 = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  column3 = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19),
  column4 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  column5 = c(5, 5, 6, 6, 7, 7, 8, 8, 9, 9))

empty_df <- data.frame()

char_df <- data.frame(
  SWIMMING = c("come back to earth", "hurt feelings", "what's the use", "perfecto", "self care"),
  CIRCLES = c("circles", "complicated", "blue world", "good news", "I can see"),
  GOOD_AM = c("doors", "brand new", "rush hour", "two matches", "100 grandkids"),
  KIDS = c("kids", "outside", "get em up", "nikes on my feet", "senior skip day"))

mixed_df <- data.frame(
  na = c(NA, NA, NA, NA),
  bool = c(TRUE, FALSE, TRUE, FALSE))

test_df3 <- data.frame(
  column1 = c(1, 1, 1, NA, 2, 2, 2),
  column2 = c(2, 2, 2, NA, 4, 4, 4))

test_df4 <- data.frame(
  column1 = c(1, 1, 1, 1.5, 1, 1, 1),
  column2 = c(2, 2, 2, 3, 4, 4, 4))

test_that("dtize_df() verifies data inputs correctly",{

  # non-df values
  expect_error(dtize_df(1),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df(TRUE),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df(list(c(1,2,3), FALSE, "gnx")),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df(NULL),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df(NA),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df(matrix(1:5)),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df("squabble up"),
               regexp=("`data` must be a dataframe."))
  expect_error(dtize_df(c(empty_df, test_df)),
               regexp=("`data` must be a dataframe."))

  expect_no_error(dtize_df(test_df))
  expect_no_error(dtize_df(test_df2))
  expect_no_error(dtize_df(empty_df))

})

test_that("dtize_df() catches na_fill correctly",{

  # non-df values
  expect_error(dtize_df(test_df, na_fill=1),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=c("reincarnated")),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=TRUE),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=NULL),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=NA),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=matrix(1:10)),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=c("pmm", "median")),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))
  expect_error(dtize_df(test_df, na_fill=list("a", 2)),
               regexp=("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."))

  expect_no_error(dtize_df(test_df, na_fill="pmm"))
  expect_no_error(dtize_df(test_df2, na_fill="pMm"))
  expect_no_error(dtize_df(test_df, na_fill="PMM"))

})

test_that("dtize_df() catches non-numeric df correctly",{

  # non-df values
  expect_warning(dtize_df(empty_df, na_fill="PMM"),
               regexp=("No numeric columns found for PMM imputation."))
  expect_warning(dtize_df(char_df, na_fill="pmm"),
               regexp=("No numeric columns found for PMM imputation."))
  expect_warning(dtize_df(mixed_df, na_fill="pmm"),
                 regexp=("No numeric columns found for PMM imputation."))

})

test_that("impute_pmm() allows non-na dataframes",{

  expect_no_error(impute_pmm(test_df, na_fill="pmm"))
  expect_no_error(impute_pmm(test_df2, na_fill="pmm"))

})

test_that("dtize_df() handles single-row dataframes correctly", {

  single_row_df <- data.frame(column1 = 1, column2 = NA)
  expect_no_error(dtize_df(single_row_df, na_fill = "pmm"))

})

test_that("impute_pmm() imputes missing values correctly", {

  df_with_na <- data.frame(col1 = c(1, 2, NA, 4, 5, 2, 3, 8, 1, 2, 6),
                           col2 = c(10, 12, 30, 40, NA, 10, 14, 15, 9, 23, 45))

  imputed_df <- impute_pmm("pmm", df_with_na)
  expect_true(!any(is.na(imputed_df$col1)))
  expect_true(!any(is.na(imputed_df$col2)))

})

test_that("impute_pmm() does not change data without missing values", {
  df_no_na <- data.frame(col1 = c(1, 2, 3, 4, 5),
                         col2 = c(10, 20, 30, 40, 50))

  result <- impute_pmm("pmm", df_no_na)
  expect_equal(result, df_no_na)

})

test_that("dtize_df() catches invalid splits", {

  expect_error(dtize_df(test_df2, splits = "invalid"),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
  expect_error(dtize_df(test_df2, splits = NA),
               regexp = "`splits` must be either `median`, `mean`, or a non-empty numeric vector.")
})

test_that("dtize_df() catches invalid labels", {

  expect_error(dtize_df(test_df2, labels = NULL),
               regexp = "`labels` cannot be NULL. Please provide valid labels for the intervals.")
  expect_error(dtize_df(test_df2, labels = c("low", NA)),
               regexp = "`labels` contains NA values. Please provide non-NA labels for the intervals.")

})

test_that("dtize_df() handles mean and median imputation correctly", {
  df_with_na <- data.frame(
    col1 = c(1, 2, NA, 4, 5))

  result_mean <- dtize_df(df_with_na, na_fill = "mean")
  expect_true(!any(is.na(result_mean$col1)))

  result_median <- dtize_df(df_with_na, na_fill = "median")
  expect_true(!any(is.na(result_median$col1)))

})

test_that("dtize_df() discretizes numeric columns correctly", {
  df <- data.frame(col1 = c(1, 2, 3, 4, 5),
                   col2 = c(10, 20, 30, 40, 50))

  result <- dtize_df(df, splits = c(2, 4), labels = c("low", "medium", "high"))
  expect_true(all(result$col1 %in% c("low", "medium", "high")))

})

test_that("impute_pmm() handles `m` and `maxit` correctly", {
  expect_error(impute_pmm("pmm", test_df3, m = -1),
               regexp = "Number of imputations (m) lower than 1.",
               fixed = TRUE)
  expect_error(impute_pmm("pmm", test_df3, m = 0),
               regexp = "Number of imputations (m) lower than 1.",
               fixed = TRUE)
  expect_error(impute_pmm("pmm", test_df2, maxit = 0),
               regexp = "`maxit` must be a positive integer.")
  expect_no_error(impute_pmm("pmm", test_df2, m = 2, maxit = 5))
})
