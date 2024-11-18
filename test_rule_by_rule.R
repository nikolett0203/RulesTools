library(testthat)

source("./rule_by_rule.R")

####### TEST INPUTS #######

data(Groceries)
rules1 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
rules2 <- apriori(Groceries, 
                  parameter = list(supp = 0.05, conf = 0.7, target = "rules"))
rules3 <- apriori(Groceries, 
                  parameter = list(supp = 0.1, conf = 0.9, target = "rules"))

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

test_that("rule_by_rule checks for named parameters",{
  
  expect_error(rule_by_rule(rules1, r2=rules2, r3=rules3),
               regexp=("Please provide names for all rule sets."))
  expect_error(rule_by_rule(rules1, rules2, rules3),
               regexp=("Please provide names for all rule sets."))  
  
})

test_that("rule_by_rule checks for validity of 'print' and 'filename'",{
  
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, TRUE, "file"),
               regexp=("Please provide names for all rule sets."))
  expect_error(rule_by_rule(rules1, rules2, rules3),
               regexp=("Please provide names for all rule sets."))  
  
})