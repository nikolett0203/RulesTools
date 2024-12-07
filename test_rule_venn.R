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
  
  expect_no_error(rule_venn(rule_list1))
  
})
