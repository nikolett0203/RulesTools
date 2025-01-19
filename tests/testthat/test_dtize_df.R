####### TEST INPUTS #######

test_df <- data.frame(
  column1 = c(1, 2, 3, 4, 5),
  column2 = c("A", "B", "C", "D", "E"),
  column3 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

test_df2 <- data.frame(
  column1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  column2 = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  column3 = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19),
  column4 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  column5 = c(5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
)

empty_df <- data.frame()

char_df <- data.frame(
  SWIMMING = c(
    "come back to earth", "hurt feelings", "what's the use",
    "perfecto", "self care"
  ),
  CIRCLES = c(
    "circles", "complicated", "blue world",
    "good news", "I can see"
  ),
  GOOD_AM = c(
    "doors", "brand new", "rush hour",
    "two matches", "100 grandkids"
  ),
  KIDS = c(
    "kids", "outside", "get em up",
    "nikes on my feet", "senior skip day"
  )
)

mixed_df <- data.frame(
  na   = c(NA, NA, NA, NA),
  bool = c(TRUE, FALSE, TRUE, FALSE)
)

test_df3 <- data.frame(
  column1 = c(1, 1, 1, NA, 2, 2, 2),
  column2 = c(2, 2, 2, NA, 4, 4, 4)
)

test_df4 <- data.frame(
  one = c(1, 5, 2, 7.6, 34, NA, 23, 22.3, NA),
  two = c(11, 5.3, 9, 4, NA, NA, 2, 15, NA)
)

####### TESTS #######

test_that("dtize_df() verifies data inputs correctly", {
  expect_error(
    dtize_df(1),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df(TRUE),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df(list(c(1, 2, 3), FALSE, "gnx")),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df(NULL),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df(NA),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df(matrix(1:5)),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df("squabble up"),
    regexp = "`data` must be a dataframe."
  )
  expect_error(
    dtize_df(c(empty_df, test_df)),
    regexp = "`data` must be a dataframe."
  )

  expect_no_error(dtize_df(test_df))
  expect_no_error(dtize_df(test_df2))
  expect_no_error(dtize_df(empty_df))
})


test_that("dtize_df() catches na_fill correctly", {
  expect_error(
    dtize_df(test_df, na_fill = 1),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = c("reincarnated")),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = TRUE),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = NULL),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = NA),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = matrix(1:10)),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = c("pmm", "median")),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )
  expect_error(
    dtize_df(test_df, na_fill = list("a", 2)),
    regexp = "Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'."
  )

  expect_no_error(dtize_df(test_df, na_fill = "pmm"))
  expect_no_error(dtize_df(test_df2, na_fill = "pMm"))
  expect_no_error(dtize_df(test_df, na_fill = "PMM"))
})

test_that("dtize_df() accepts na_fill for median, mean, and none", {
  expect_no_error(dtize_df(test_df, na_fill = "MEDIAN"))
  expect_no_error(dtize_df(test_df2, na_fill = "mean"))
  expect_no_error(dtize_df(test_df, na_fill = "noNe"))
})


test_that("dtize_df() catches incorrect `m` arguments", {
  # non-numeric
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = TRUE),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = list(matrix(1:5), 3, "kendrick")),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = NULL),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = NA),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = "oh yes you do"),
    regexp = "`m` must be a single positive integer."
  )

  # multiple values
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = c(1, 2, 3)),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = list(1, c(1, 2, 3), 3)),
    regexp = "`m` must be a single positive integer."
  )

  # less than 1
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = 0),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = -1),
    regexp = "`m` must be a single positive integer."
  )

  # infinite
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = Inf),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = -Inf),
    regexp = "`m` must be a single positive integer."
  )

  # decimal
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = 0.1),
    regexp = "`m` must be a single positive integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", m = 0.9999999),
    regexp = "`m` must be a single positive integer."
  )

  expect_no_error(dtize_df(test_df4, na_fill = "pmm", m = 1))
  expect_no_error(dtize_df(test_df4, na_fill = "pmm", m = 100))
})


test_that("dtize_df() catches incorrect `maxit` arguments", {
  # non-numeric
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = TRUE),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = list(matrix(1:5), 3, "kendrick")),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = NULL),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = NA),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = "oh yes you do"),
    regexp = "`maxit` must be a single non-negative integer."
  )

  # multiple values
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = c(1, 2, 3)),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = list(1, c(1, 2, 3), 3)),
    regexp = "`maxit` must be a single non-negative integer."
  )

  # less than 0
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = -1),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = -100),
    regexp = "`maxit` must be a single non-negative integer."
  )

  # infinite
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = Inf),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = -Inf),
    regexp = "`maxit` must be a single non-negative integer."
  )

  # decimal
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = 0.1),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", maxit = 0.9999999),
    regexp = "`maxit` must be a single non-negative integer."
  )

  expect_no_error(dtize_df(test_df4, na_fill = "pmm", maxit = 0))
  expect_no_error(dtize_df(test_df4, na_fill = "pmm", maxit = 100))
})


test_that("dtize_df() catches incorrect `seed` arguments", {
  # non-numeric
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = TRUE),
    regexp = "`seed` must be NULL or a single integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = list(matrix(1:5), 3, "kendrick")),
    regexp = "`seed` must be NULL or a single integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = NA),
    regexp = "`seed` must be NULL or a single integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = "oh yes you do"),
    regexp = "`seed` must be NULL or a single integer."
  )

  # multiple values
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = c(1, 2, 3)),
    regexp = "`seed` must be NULL or a single integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = list(1, c(1, 2, 3), 3)),
    regexp = "`seed` must be NULL or a single integer."
  )

  # infinite
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = Inf),
    regexp = "`seed` must be NULL or a single integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = -Inf),
    regexp = "`seed` must be NULL or a single integer."
  )

  # decimal
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = 0.1),
    regexp = "`seed` must be NULL or a single integer."
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", seed = 0.9999999),
    regexp = "`seed` must be NULL or a single integer."
  )

  expect_no_error(dtize_df(test_df4, na_fill = "pmm", seed = 0))
  expect_no_error(dtize_df(test_df4, na_fill = "pmm", seed = 1523675))
  expect_no_error(dtize_df(test_df4, na_fill = "pmm", seed = NULL))
})


test_that("dtize_df() catches incorrect `printFlag` arguments", {
  # non-logical values
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = 7),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = NA),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = NULL),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = Inf),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = list(1, c(TRUE, FALSE), "three")),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = matrix(1:9, nrow = 3, ncol = 3)),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = "truth"),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )

  # multiple values
  expect_error(
    dtize_df(test_df, na_fill = "pmm", printFlag = c(TRUE, FALSE, TRUE)),
    regexp = "`printFlag` must be a single logical value (TRUE or FALSE).",
    fixed = TRUE
  )

  expect_no_error(dtize_df(test_df4, na_fill = "pmm", printFlag = TRUE))
  expect_no_error(dtize_df(test_df4, na_fill = "pmm", printFlag = FALSE))
})


test_that("dtize_df() catches non-numeric df correctly", {
  expect_warning(
    dtize_df(empty_df, na_fill = "PMM"),
    regexp = "No numeric columns found for PMM imputation."
  )
  expect_warning(
    dtize_df(char_df, na_fill = "pmm"),
    regexp = "No numeric columns found for PMM imputation."
  )
  expect_warning(
    dtize_df(mixed_df, na_fill = "pmm"),
    regexp = "No numeric columns found for PMM imputation."
  )
})


test_that("impute_pmm() allows non-na dataframes", {
  expect_no_error(impute_pmm(test_df, na_fill = "pmm"))
  expect_no_error(impute_pmm(test_df2, na_fill = "pmm"))
})


test_that("dtize_df() handles single-row dataframes correctly", {
  single_row_df <- data.frame(column1 = 1, column2 = NA)
  expect_no_error(dtize_df(single_row_df, na_fill = "pmm"))
})


test_that("impute_pmm() imputes missing values correctly", {
  df_with_na <- data.frame(
    col1 = c(1, 2, NA, 4, 5, 2, 3, 8, 1, 2, 6),
    col2 = c(10, 12, 30, 40, NA, 10, 14, 15, 9, 23, 45)
  )

  imputed_df <- impute_pmm("pmm", df_with_na)
  expect_true(!any(is.na(imputed_df$col1)))
  expect_true(!any(is.na(imputed_df$col2)))
})


test_that("impute_pmm() does not change data without missing values", {
  df_no_na <- data.frame(
    col1 = c(1, 2, 3, 4, 5),
    col2 = c(10, 20, 30, 40, 50)
  )

  result <- impute_pmm("pmm", df_no_na)
  expect_equal(result, df_no_na)
})


test_that("dtize_df() catches invalid cutoff", {
  expect_error(
    dtize_df(test_df2, cutoff = "invalid"),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
  expect_error(
    dtize_df(test_df2, cutoff = NA),
    regexp = "`cutoff` must be either `median`, `mean`, or a non-empty numeric vector."
  )
})


test_that("dtize_df() catches invalid labels", {
  expect_error(
    dtize_df(test_df2, labels = NULL),
    regexp = "`labels` cannot be NULL. Please provide valid labels for the intervals."
  )
  expect_error(
    dtize_df(test_df2, labels = c("low", NA)),
    regexp = "`labels` contains NA values. Please provide non-NA labels for the intervals."
  )
})


test_that("dtize_df() handles mean and median imputation correctly", {
  df_with_na <- data.frame(col1 = c(1, 2, NA, 4, 5))

  result_mean <- dtize_df(df_with_na, na_fill = "mean")
  expect_true(!any(is.na(result_mean$col1)))

  result_median <- dtize_df(df_with_na, na_fill = "median")
  expect_true(!any(is.na(result_median$col1)))
})


test_that("dtize_df() discretizes numeric columns correctly", {
  df <- data.frame(
    col1 = c(1, 2, 3, 4, 5),
    col2 = c(10, 20, 30, 40, 50)
  )

  result <- dtize_df(df, cutoff = c(2, 4), labels = c("low", "medium", "high"))
  expect_true(all(result$col1 %in% c("low", "medium", "high")))
})


test_that("impute_pmm() handles `m` and `maxit` correctly", {
  expect_error(
    impute_pmm("pmm", test_df3, m = -1),
    regexp = "`m` must be a single positive integer.",
    fixed = TRUE
  )
  expect_error(
    impute_pmm("pmm", test_df3, m = 0),
    regexp = "`m` must be a single positive integer.",
    fixed = TRUE
  )
  expect_error(
    impute_pmm("pmm", test_df2, maxit = -1),
    regexp = "`maxit` must be a single non-negative integer."
  )
  expect_no_error(impute_pmm("pmm", test_df2, m = 2, maxit = 5))
})


test_that("validate_cuts() checks for valid cutoff inputs", {

  # character but not mean or median
  expect_error(
    validate_cuts("good news", test_df),
    regexp="`cutoff` must be either 'median', 'mean', or a named list of numeric vectors."
  )
  expect_error(
    validate_cuts(c("mean", "median"), test_df),
    regexp="`cutoff` must be either 'median', 'mean', or a named list of numeric vectors."
  )

  # non-character non-list
  expect_error(
    validate_cuts(TRUE, test_df),
    regexp="must be either a character string"
  )
  expect_error(
    validate_cuts(c(1, 2, 3), test_df2),
    regexp="must be either a character string"
  )
  expect_error(
    validate_cuts(NULL, test_df),
    regexp="must be either a character string"
  )
  expect_error(
    validate_cuts(NA, test_df2),
    regexp="must be either a character string"
  )
  expect_error(
    validate_cuts(matrix(1:7), test_df),
    regexp="must be either a character string"
  )
  expect_error(
    validate_cuts(Inf, test_df2),
    regexp="must be either a character string"
  )

  # vector list but not numeric
  expect_error(
    validate_cuts(list("1"=c("a", "b", "c"),
                       "2"=c("d", "e", "f"),
                       "3"=c("h", "i", "j")),
                  test_df2),
    regexp="must be numeric vectors."
  )

  expect_error(
    validate_cuts(list("one"=c(TRUE, FALSE),
                       "two"=c(FALSE, TRUE),
                       "three"=c(TRUE, TRUE, TRUE)),
                  test_df2),
    regexp="must be numeric vectors."
  )

  # numeric list but not vectors
  expect_error(
    validate_cuts(list("one"=matrix(1:3),
                       "two"=matrix(1:4),
                       "three"=matrix(1:3)),
                  test_df),
    regexp="must be numeric vectors."
  )

  # missing names
  expect_error(
    validate_cuts(list(3, 2, 1), test_df),
    regexp="must have names that match all numeric columns"
  )
  expect_error(
    validate_cuts(list("column1"=3, 2,
                       "column3"=1),
                  test_df),
    regexp="must have names that match all numeric columns"
  )

  # invalid names
  expect_error(
    validate_cuts(list("column1"=c(1, 3),
                       "COL2"=c(1, 2),
                       "column3"=c(1,3)),
                  test_df),
    regexp="must have names that match all numeric columns"
  )
  expect_error(
    validate_cuts(list("COL1"=c(1, 3),
                       "COL2"=c(1, 2),
                       "COL3"=c(1,3)),
                  test_df),
    regexp="must have names that match all numeric columns"
  )
  expect_error(
    validate_cuts(list("column2"=c(1, 2),
                       "column3"=c(1,3)),
                  test_df),
    regexp="must have names that match all numeric columns"
  )

  # missing vectors
  expect_error(
    validate_cuts(list("column1"=c(1, 3),
                       "column2"=c(1, 2),
                       "column3"=c(1,3)),
                  test_df2),
    regexp="must include entries for all numeric columns"
  )

  expect_no_error(validate_cuts("median", test_df))
  expect_no_error(validate_cuts("mEan", test_df))
  expect_no_error(validate_cuts(list("column1"=c(1, 2, 3)), test_df))
  expect_no_error(
    validate_cuts(list("column1"=c(1.1, 2.2, 3.3),
                       "column2"=c(40L,50L),
                       "column3"=c(10, 12, 18, 20),
                       "column4"=c(2, 4, 6),
                       "column5"=c(22, 34.3, 47.2, 59, 83.1)),
                  test_df2)
    )

  validate_cuts(
    list(
      column1 = c(1.1, 2.2, 3.3),
      column2 = c(40L, 50L),
      column3 = c(10, 12, 18, 20),
      column4 = c(2, 4, 6),
      column5 = c(22, 34.3, 47.2, 59, 83.1)
    ),
    test_df2
  )
})



