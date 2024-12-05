library(testthat)
library(arules)

source("./rule_map.R")

####### TEST INPUTS #######

data(Groceries)

rules1 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
rules2 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.55, target = "rules"))

test_that("rule_map() validates rules objects", {
  
  expect_error(rule_map(1),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(NULL),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(NA),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(c("no simulation", "cross that line", "pasadena")),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_map(matrix(1:3)),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")

  # why?
  expect_no_error(rule_map(c(rules1, rules2)))
  expect_no_error(rule_map(c(large_rules)))
  
})

test_that("rule_map validates `metric` arguments correctly", {
  
  expect_error(rule_map(rules1, metric=0),
               regexp="'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")
  expect_error(rule_map(rules1, metric=c("confidence, support, lift")),
               regexp="'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")
  expect_error(rule_map(rules1, metric=NULL),
               regexp="'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")
  expect_error(rule_map(rules1, metric=NA),
               regexp="'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")
  expect_error(rule_map(rules1, metric=rules1),
               regexp="'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")  
  expect_error(rule_map(rules1, metric=list(TRUE, "two", 3)),
               regexp="'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.") 
  
  expect_no_error(rule_map(rules1, metric="LIFT"))
  expect_no_error(rule_map(rules1, metric="cOnFiDeNcE"))
  expect_no_error(rule_map(rules1, metric="support"))
  
})

test_that("rule_map validates `graph_title` arguments correctly", {
  
  expect_error(rule_map(rules1, graph_title=1),
               regexp="The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_map(rules1, graph_title=matrix(1:10)),
               regexp="The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_map(rules1, graph_title=NA),
               regexp="The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_map(rules1, graph_title=c(1, 2, 3)),
               regexp="The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_map(rules1, graph_title=Inf),
               regexp="The graph title must be either NULL or a single non-NA character string.")  
  expect_error(rule_map(rules1, graph_title=rules1),
               regexp="The graph title must be either NULL or a single non-NA character string.") 
  
  expect_no_error(rule_map(rules1, graph_title="Heatmap"))
  expect_no_error(rule_map(rules1, graph_title=NULL))
  expect_no_error(rule_map(rules1, graph_title=""))
  
})

