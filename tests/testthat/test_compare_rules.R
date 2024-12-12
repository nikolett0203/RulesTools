####### TEST INPUTS #######

data(Groceries)

rules1 <- apriori(
  Groceries,
  parameter = list(supp = 0.01, conf = 0.5, target = "rules")
)

rules2 <- apriori(
  Groceries,
  parameter = list(supp = 0.01, conf = 0.55, target = "rules")
)

rules3 <- apriori(
  Groceries,
  parameter = list(supp = 0.02, conf = 0.5, target = "rules")
)  # empty

large_rules <- apriori(
  Groceries,
  parameter = list(supp = 0.005, conf = 0.2, target = "rules")
)

####### TESTS #######

test_that("compare_rules() catches all unnamed arguments", {
  
  # all unnamed
  expect_error(
    compare_rules(rules1, rules2, rules3, TRUE, "name"),
    regexp = "Please provide names for all arguments, including 'display', 'filename', and all rule sets."
  )
  
  # partially unnamed
  expect_error(
    compare_rules(r1 = rules1, rules2, r3 = rules3, TRUE, filename = "name"),
    regexp = "Please provide names for all arguments, including 'display', 'filename', and all rule sets."
  )
  
  expect_error(
    compare_rules(rules1, r2 = rules2, rules3, random = TRUE, "name"),
    regexp = "Please provide names for all arguments, including 'display', 'filename', and all rule sets."
  )
  
  # duplicate names
  expect_error(
    compare_rules(r1 = rules1, r1 = rules2, r3 = rules3, display = TRUE, filename = "name"),
    regexp = "Duplicate names are not allowed. Please provide unique names for all rule sets."
  )
  
  # all named
  expect_no_error(compare_rules(r1 = rules1, display = FALSE, r2 = rules2))
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, display = FALSE, filename = "name.csv"))
})


test_that("compare_rules() validates optional parameters", {
  
  # invalid types for display
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = c(TRUE, FALSE)),
    regexp = "'display' must be either TRUE or FALSE."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = NULL),
    regexp = "'display' must be either TRUE or FALSE."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = NA),
    regexp = "'display' must be either TRUE or FALSE."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = " "),
    regexp = "'display' must be either TRUE or FALSE."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = 0),
    regexp = "'display' must be either TRUE or FALSE."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = list(TRUE)),
    regexp = "'display' must be either TRUE or FALSE."
  )
  
  # valid display arguments  
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = TRUE))
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, display = FALSE))
  
  # invalid types for filename
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = TRUE),
    regexp = "'filename' must be a non-empty string or NULL."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = c(1, 2, 3)),
    regexp = "'filename' must be a non-empty string or NULL."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = NA),
    regexp = "'filename' must be a non-empty string or NULL."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = ""),
    regexp = "'filename' must be a non-empty string or NULL."
  )
  
  expect_error(
    compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = c("harry", "ron", "hermione")),
    regexp = "'filename' must be a non-empty string or NULL."
  )
  
  # valid filenames
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = NULL))
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = "file.csv"))
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, r3 = rules3, filename = "You could have the world in the palm of your hand, you still might drop it.csv"))
})


test_that("compare_rules() verifies number of arguments correctly (<2)", {
  
  # single argument
  expect_error(
    compare_rules(r1 = rules1, display = FALSE),
    regexp = "At least two rule sets are required to find overlaps. Please provide at least two rule sets."
  )
  
  # valid number of arguments
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, display = FALSE))
  expect_no_error(compare_rules(r1 = rules1, r2 = rules1, r3 = rules1, r4 = rules1, r5 = rules1, r6 = rules1, r7 = rules1, display = FALSE))
})


test_that("compare_rules() checks that parameters are rules", {
  
  # input non-rules objects
  expect_error(
    compare_rules(r1 = "one", r2 = "two", display = FALSE),
    regexp = "All inputs must be of class 'rules'. Please provide valid rule sets."
  )
  
  expect_error(
    compare_rules(rule = NULL, another_rule = NULL, one_more_rule = NULL, final_rule = NULL, display = FALSE),
    regexp = "All inputs must be of class 'rules'. Please provide valid rule sets."
  )
  
  expect_error(
    compare_rules(r = 1, u = 2, l = 3, e = 4, display = FALSE),
    regexp = "All inputs must be of class 'rules'. Please provide valid rule sets."
  )
  
  expect_error(
    compare_rules(r = rules1, u = NA, l = rules2, e = rules3, display = FALSE),
    regexp = "All inputs must be of class 'rules'. Please provide valid rule sets."
  )
  
  expect_error(
    compare_rules(r1 = list("A", "B", "C"), r2 = rules1, display = FALSE),
    regexp = "All inputs must be of class 'rules'. Please provide valid rule sets."
  )
  
  expect_error(
    compare_rules(r1 = data.frame(A = 1:3, B = 4:6), r2 = data.frame(C = 7:10, B = 11:14), display = FALSE),
    regexp = "All inputs must be of class 'rules'. Please provide valid rule sets."
  )
})


test_that("compare_rules() handles complex names", {
  
  # odd characters and long names
  expect_no_error(
    compare_rules(
      `this_is_a_very_long_name_1234567890` = rules1,
      `another_very_long_name_with_special_chars!@#$%^&*` = rules2,
      display = FALSE
    )
  )
  
  expect_no_error(compare_rules(`if` = rules1, `else` = rules2, display = FALSE))
  expect_no_error(compare_rules(`rule!@#$` = rules1, `another_rule` = rules2, display = FALSE))
})


test_that("compare_rules() handles large rule sets", {
  expect_no_error(compare_rules(r1 = large_rules, r2 = rules1, r3 = rules2, filename = NULL, display = FALSE))
})


test_that("compare_rules() respects display suppression", {
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, display = FALSE))
  expect_output(compare_rules(r1 = rules1, r2 = rules2, display = TRUE), "r1 & r2")
})


