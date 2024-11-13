library(testthat)

source("./rule_by_rule.R")

test_that("rule_by_rule verifies number of arguments correctly (<2)",{
  
  # empty argument
  expect_error(rule_by_rule(),
               regexp=("At least two rule sets are required to find overlaps. Please provide at least two rule sets."))
  # single argument
  expect_error(rule_by_rule("one"),
               regexp=("At least two rule sets are required to find overlaps. Please provide at least two rule sets."))  
  
})

test_that("rule_by_rule checks that parameters are rules",{
  
  # strings
  expect_error(rule_by_rule("one", "two"),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  expect_error(rule_by_rule(NULL, NULL, NULL, NULL),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  expect_error(rule_by_rule(1, 2, 3),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  
})
