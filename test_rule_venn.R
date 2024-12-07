library(testthat)
library(arules)

source("./rule_venn.R")

data(Groceries)

rules1 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
rules2 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.55, target = "rules"))
rules3 <- apriori(Groceries, 
                  parameter = list(supp = 0.02, conf = 0.5, target = "rules")) # empty 
large_rules <- apriori(Groceries, 
                       parameter = list(supp = 0.005, conf = 0.2, target = "rules"))

rule_list1 = list(r1 = rules1, r2 = rules2)

test_that("rule_by_rule() catches invalid rules arguments",{
  
  expect_error(rule_venn(1),
               regexp="'rules' objects must be provided as a list.")
  expect_error(rule_venn(rules1),
               regexp="'rules' objects must be provided as a list.")
  expect_error(rule_venn(c(rules1, rules2)),
               regexp="'rules' objects must be provided as a list.")
  expect_error(rule_venn(matrix(1:3)),
               regexp="'rules' objects must be provided as a list.")
  expect_error(rule_venn(matrix("khalid")),
               regexp="'rules' objects must be provided as a list.")
  expect_error(rule_venn(NULL),
               regexp="'rules' objects must be provided as a list.")
  
  expect_error(rule_venn(list(rules1)),
               regexp="You must provide between 2 and 4 'rules' objects.")
  expect_error(rule_venn(list(rules1, rules2, rules3, large_rules, rules1)),
               regexp="You must provide between 2 and 4 'rules' objects.")

  expect_error(rule_venn(list(rules1, rules2, rules3, NULL)),
               regexp="The list contains NULL values. Please provide valid 'rules' objects.")
  expect_error(rule_venn(list(NULL, NULL, NULL, NULL)),
               regexp="The list contains NULL values. Please provide valid 'rules' objects.")
  
  expect_error(rule_venn(list(rules1, NA, rules2)),
               regexp="All elements in the list must be 'rules' objects.")
  expect_error(rule_venn(list(1, 2, 3)),
               regexp="All elements in the list must be 'rules' objects.")
  expect_error(rule_venn(list("m.i.a.", "faded")),
               regexp="All elements in the list must be 'rules' objects.")
  expect_error(rule_venn(list(rules1, matrix(1:3), TRUE)),
               regexp="All elements in the list must be 'rules' objects.")
      
  expect_no_error(rule_venn(list(rules1, rules2)))
  expect_no_error(rule_venn(list(rules1, rules2, rules3)))
  expect_no_error(rule_venn(list(rules1, rules2, rules3, large_rules)))
  
    
})
