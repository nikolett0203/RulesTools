library(testthat)
library(arules)

source("./rule_by_rule.R")

####### TEST INPUTS #######

data(Groceries)

rules1 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
rules2 <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.55, target = "rules"))
rules3 <- apriori(Groceries, 
                  parameter = list(supp = 0.02, conf = 0.5, target = "rules"))


test_that("rule_by_rule() catches all unnamed arguments",{
  
  expect_error(rule_by_rule(rules1, rules2, rules3, TRUE, "name"),
               regexp="Please provide names for all arguments, including 'display', 'filename', and all rule sets.")

  expect_error(rule_by_rule(r1=rules1, rules2, r3=rules3, TRUE, filename="name"),
               regexp="Please provide names for all arguments, including 'display', 'filename', and all rule sets.")

  expect_error(rule_by_rule(rules1, r2=rules2, rules3, random=TRUE, "name"),
               regexp="Please provide names for all arguments, including 'display', 'filename', and all rule sets.")
  
  expect_no_error(rule_by_rule(r1=rules1, display=FALSE, r2=rules2))
  
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2, display=FALSE, filename="name.csv"))
  
})

test_that("rule_by_rule() validates optional parameters",{
  
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=c(TRUE, FALSE)),
               regexp="'display' must be either TRUE or FALSE.")
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=NULL),
               regexp="'display' must be either TRUE or FALSE.")
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=NA),
               regexp="'display' must be either TRUE or FALSE.")
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=" "),
               regexp="'display' must be either TRUE or FALSE.")
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=0),
               regexp="'display' must be either TRUE or FALSE.")
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=list(TRUE)),
               regexp="'display' must be either TRUE or FALSE.")
    
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=TRUE))
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, display=FALSE))
  
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename=TRUE),
               regexp="'filename' must be a non-empty string or NULL.") 
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename=c(1, 2, 3)),
               regexp="'filename' must be a non-empty string or NULL.") 
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename=NA),
               regexp="'filename' must be a non-empty string or NULL.") 
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename=""),
               regexp="'filename' must be a non-empty string or NULL.")
  expect_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename=c("harry", "ron", "hermione")),
               regexp="'filename' must be a non-empty string or NULL.")
  
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename=NULL))
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename="file.csv")) 
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2, r3=rules3, filename="You could have the world in the palm of your hand, you still might drop it.csv"))
  
})

test_that("rule_by_rule verifies number of arguments correctly (<2)",{
  
  # single argument
  expect_error(rule_by_rule(r1=rules1),
               regexp=("At least two rule sets are required to find overlaps. Please provide at least two rule sets."))  
  
  expect_no_error(rule_by_rule(r1=rules1, r2=rules2)) 
  expect_no_error(rule_by_rule(r1=rules1, r2=rules1, r3=rules1, r4=rules1, r5=rules1, r6=rules1, r7=rules1)) 
  
})

test_that("rule_by_rule checks that parameters are rules",{
  

  expect_error(rule_by_rule(r1="one", r2="two"),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  expect_error(rule_by_rule(rule=NULL, another_rule=NULL, one_more_rule=NULL, final_rule=NULL),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  expect_error(rule_by_rule(r=1, u=2, l=3, e=4),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  expect_error(rule_by_rule(r=rules1, u=NA, l=rules2, e=rules3),
               regexp=("All inputs must be of class 'rules'. Please provide valid rule sets."))
  expect_error(rule_by_rule(r1 = list("A", "B", "C"), r2 = rules1),
               regexp = "All inputs must be of class 'rules'. Please provide valid rule sets.")
  expect_error(rule_by_rule(r1 = data.frame(A = 1:3, B = 4:6), r2 = data.frame(C = 7:10, B = 11:14)),
               regexp = "All inputs must be of class 'rules'. Please provide valid rule sets.")
  
})




