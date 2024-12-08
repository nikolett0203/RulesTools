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

test_that("rule_venn() catches invalid rules arguments",{
  
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

test_that("rule_venn() catches invalid numerical arguments",{
  
  expect_error(rule_venn(rule_list1, stroke_size=NULL),
               regexp="'stroke_size' must be a finite positive numeric value.")
  expect_error(rule_venn(rule_list1, stroke_size=NA),
               regexp="'stroke_size' must be a finite positive numeric value.") 
  expect_error(rule_venn(rule_list1, text_size=matrix(1:3)),
               regexp="'text_size' must be a finite positive numeric value.")
  expect_error(rule_venn(rule_list1, text_size=TRUE),
               regexp="'text_size' must be a finite positive numeric value.")  
  expect_error(rule_venn(rule_list1, name_size="names"),
               regexp="'name_size' must be a finite positive numeric value.")
  expect_error(rule_venn(rule_list1, name_size=Inf),
               regexp="'name_size' must be a finite positive numeric value.")
  
  expect_error(rule_venn(rule_list1, name_size=c(1, 2, 3)),
               regexp="'name_size' must be a finite positive numeric value.")
  expect_error(rule_venn(rule_list1, text_size=list(200)),
               regexp="'text_size' must be a finite positive numeric value.")
  
  expect_error(rule_venn(rule_list1, stroke_size=-0.000001))
  
  expect_no_error(rule_venn(rule_list1, stroke_size=2, text_size=8, name_size=10))
  expect_no_error(rule_venn(rule_list1, stroke_size=0, text_size=10.5, name_size=100))
      
})

test_that("rule_venn() catches invalid rules arguments",{

  expect_error(rule_venn(rule_list1, fill_alpha = 1.5),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = -1.5),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  
  expect_error(rule_venn(rule_list1, fill_alpha = c(0.5, 0.3)),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = list(0.1, 0.2, 0.3)),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  
  expect_error(rule_venn(rule_list1, fill_alpha = NULL),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = NA),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = matrix(1:3)),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = "alpha"),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = "TRUE"),
               regexp = "`alpha` must be a single numeric value between 0 and 1.")
    
  expect_no_error(rule_venn(rule_list1, fill_alpha = 0.75))
  expect_no_error(rule_venn(rule_list1, fill_alpha = 0))
  expect_no_error(rule_venn(rule_list1, fill_alpha = 0.785))

})

test_that("rule_venn() catches invalid fill_alpha arguments",{

  expect_error(rule_venn(rule_list1, fill_alpha = 1.5),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = -1.5),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  
  expect_error(rule_venn(rule_list1, fill_alpha = c(0.5, 0.3)),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = list(0.1, 0.2, 0.3)),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  
  expect_error(rule_venn(rule_list1, fill_alpha = NULL),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = NA),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = matrix(1:3)),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = "alpha"),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
  expect_error(rule_venn(rule_list1, fill_alpha = "TRUE"),
               regexp = "`fill_alpha` must be a single numeric value between 0 and 1.")
    
  expect_no_error(rule_venn(rule_list1, fill_alpha = 0.75))
  expect_no_error(rule_venn(rule_list1, fill_alpha = 0))
  expect_no_error(rule_venn(rule_list1, fill_alpha = 0.785))

})


test_that("rule_venn() catches invalid title arguments",{
  
  expect_error(rule_venn(rule_list1, title = 1.5),
               regexp = "The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_venn(rule_list1, title = c(0.5, 0.3)),
               regexp = "The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_venn(rule_list1, title = list(0.1, "two", FALSE)),
               regexp = "The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_venn(rule_list1, title = NA),
               regexp = "The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_venn(rule_list1, title = matrix(1:3)),
               regexp = "The graph title must be either NULL or a single non-NA character string.")
  expect_error(rule_venn(rule_list1, title = TRUE),
               regexp = "The graph title must be either NULL or a single non-NA character string.")
  
  expect_no_error(rule_venn(rule_list1, title="VENN"))
  expect_no_error(rule_venn(rule_list1, title="I switched the time zone, but what do I know? Spendin' nights hitchhikin', where will I go? I could fly home, with my eyes closed But it'd get kinda hard to see, that's no surprise though."))
  expect_no_error(rule_venn(rule_list1, title = ""))
  
})










