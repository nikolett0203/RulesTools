library(testthat)
library(arules)

source("./rule_map.R")

####### TEST INPUTS #######

data(Groceries)

rules1 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
rules2 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.55, target = "rules"))

test_that("rule_map() validates rules objects",{
  
  expect_error(rule_map(1),
               regexp="Input must be of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(NULL),
               regexp="Input must be of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(NA),
               regexp="Input must be of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(c("no simulation", "cross that line", "pasadena")),
               regexp="Input must be of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(c(rules1, rules2)),
               regexp="Input must be a single 'rules' object. Please enter one rule set.")
  #expect_error(rule_map(),
   #            regexp="Input must be of class 'rules'. Please provide a valid rule set.")
  
})
