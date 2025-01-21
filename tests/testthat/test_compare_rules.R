####### TEST INPUTS #######

utils::data("Groceries", package = "arules")
library(arules)

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

test_that("validate_names() catches unnamed arguments or duplicated argument names", {

  # all unnamed
  expect_error(
    validate_names(names(list(1, "hello", NA))),
    regexp = "Please provide names for all arguments, including 'display', 'filename', and all rule sets."
  )

  # partially unnamed
  expect_error(
    validate_names(names(list(rules1=1, rules2=2, NULL))),
    regexp = "Please provide names for all arguments, including 'display', 'filename', and all rule sets."
  )

  # null list
  expect_error(
    validate_names(NULL),
    regexp = "Please provide names for all arguments, including 'display', 'filename', and all rule sets."
  )

  # duplicated names
  expect_error(
    validate_names(names(list(rules1=1, rules1=2, display=TRUE))),
    regexp = "Duplicate names are not allowed. Please provide unique names for all rule sets."
  )

  # all named
  expect_no_error(
    validate_names(names(list(rules1=1, filename="file", display=TRUE)))
  )
})


test_that("compare_rules() catches all unnamed arguments or duplicated argument names", {

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


test_that("validate_options() captures invalid filenames and logical parameters", {

  # non-logical display values
  expect_error(
    validate_options(display="hello", filename="file"),
    regexp = "'display' must be either TRUE or FALSE."
  )

  expect_error(
    validate_options(display=NA, filename="file"),
    regexp = "'display' must be either TRUE or FALSE."
  )

  expect_error(
    validate_options(display=123, filename="file"),
    regexp = "'display' must be either TRUE or FALSE."
  )

  # multiple logical values
  expect_error(
    validate_options(display=c(TRUE, FALSE), filename="file"),
    regexp = "'display' must be either TRUE or FALSE."
  )

  # non-character filenames
  expect_error(
    validate_options(display=TRUE, filename=NA),
    regexp = "'filename' must be a non-empty string or NULL."
  )

  expect_error(
    validate_options(display=FALSE, filename=123),
    regexp = "'filename' must be a non-empty string or NULL."
  )

  expect_error(
    validate_options(display=TRUE, filename=list(one=1, two="two", three=NULL)),
    regexp = "'filename' must be a non-empty string or NULL."
  )

  # multiple filenames
  expect_error(
    validate_options(display=FALSE, filename=c("name1", "name2", "name3")),
    regexp = "'filename' must be a non-empty string or NULL."
  )

  # valid inputs
  expect_no_error(validate_options(display=TRUE, filename="false"))
  expect_no_error(validate_options(display=FALSE, filename="eyes to the skies"))
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


test_that("validate_rules() verifies the number and types of arguments correctly", {

  # insufficient number of rules arguments for comparison
  expect_error(
    validate_rules(list(rules1)),
    regexp="At least two rule sets are required to find overlaps. Please provide at least two rule sets."
  )

  expect_error(
    validate_rules(list()),
    regexp="At least two rule sets are required to find overlaps. Please provide at least two rule sets."
  )

  # non-rules arguments
  expect_error(
    validate_rules(list(r1=1, r2="two", r3=TRUE, r4=NA)),
    regexp="All inputs must be of class 'rules'. Please provide valid rule sets."
  )

  # partial rules arguments
  expect_error(
    validate_rules(list(r1=rules1, r2="two", r3=NULL)),
    regexp="All inputs must be of class 'rules'. Please provide valid rule sets."
  )

  # valid rules
  expect_no_error(validate_rules(list(r1=rules1, r2=rules2, r3=rules3)))
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


test_that("get_intersection_key() produces the right keys", {

  # multiple indices
  expect_equal(
    get_intersection_key(c(1, 3), c("R1", "R2", "R3")),
    "R1 & R3"
  )

  expect_equal(
    get_intersection_key(c(1, 3, 4, 7), c("R1", "R2", "R3", "R4", "R5", "R6", "R7")),
    "R1 & R3 & R4 & R7"
  )

  # single index
  expect_equal(
    get_intersection_key(c(1), c("R1", "R2", "R3", "R4", "R5", "R6", "R7")),
    "R1"
  )

  # invalid index
  expect_equal(
    get_intersection_key(c(0), c("R1", "R2", "R3", "R4", "R5", "R6", "R7")),
    ""
  )
})


test_that("find_intersections() generates the right intersections", {

  # test 1: triple overlap
  triple <- list(c("A", "B", "C"), c("B", "C", "D"), c("C", "D", "E"))
  triple_names <- c("Set 1", "Set 2", "Set 3")
  expect_equal(
    find_intersections(triple, triple_names),
    list(
      "Set 1" = c("A", "B", "C"),
      "Set 2" = c("B", "C", "D"),
      "Set 3" = c("C", "D", "E"),
      "Set 1 & Set 2" = c("B", "C"),
      "Set 1 & Set 3" = "C",
      "Set 2 & Set 3" = c("C", "D"),
      "Set 1 & Set 2 & Set 3" = "C"
    )
  )

  # test 2: empty rules
  empty <- list(character(0), c("A", "B"), c("B", "C"))
  empty_names <- c("Set 1", "Set 2", "Set 3")
  expect_equal(
    find_intersections(empty, empty_names),
    list(
      "Set 1" = character(0),
      "Set 2" = c("A", "B"),
      "Set 3" = c("B", "C"),
      "Set 1 & Set 2" = "No common rules",
      "Set 1 & Set 3" = "No common rules",
      "Set 2 & Set 3" = "B",
      "Set 1 & Set 2 & Set 3" = "No common rules"
    )
  )

  # test 3: no overlap
  noverlap <- list(c("A", "B"), c("C", "D"), c("E", "F"), c("G", "H"))
  noverlap_names <- c("Set 1", "Set 2", "Set 3", "Set 4")
  expect_equal(
    find_intersections(noverlap, noverlap_names),
    list(
      "Set 1" = c("A", "B"),
      "Set 2" = c("C", "D"),
      "Set 3" = c("E", "F"),
      "Set 4" = c("G", "H"),
      "Set 1 & Set 2"= "No common rules",
      "Set 1 & Set 3"= "No common rules",
      "Set 1 & Set 4"= "No common rules",
      "Set 2 & Set 3"= "No common rules",
      "Set 2 & Set 4"= "No common rules",
      "Set 3 & Set 4"= "No common rules",
      "Set 1 & Set 2 & Set 3"= "No common rules",
      "Set 1 & Set 2 & Set 4"= "No common rules",
      "Set 1 & Set 3 & Set 4"= "No common rules",
      "Set 2 & Set 3 & Set 4"= "No common rules",
      "Set 1 & Set 2 & Set 3 & Set 4"= "No common rules"
    )
  )
})


test_that("pad_rules() correctly adds NAs to fill the vector", {

  # normal padding
  expect_equal(
    pad_rules(c("A", "B", "C"), 5),
    c("A", "B", "C", NA, NA)
  )

  # no padding needed
  expect_equal(
    pad_rules(c("A", "B", "C"), 3),
    c("A", "B", "C")
  )

  # vector already contains NA
  expect_equal(
    pad_rules(c("A", "B", NA), 5),
    c("A", "B", NA, NA, NA)
  )
})


test_that("compare_rules() handles large rule sets", {
  expect_no_error(compare_rules(r1 = large_rules, r2 = rules1, r3 = rules2, filename = NULL, display = FALSE))
})


test_that("compare_rules() respects display suppression", {
  expect_no_error(compare_rules(r1 = rules1, r2 = rules2, display = FALSE))
  expect_output(compare_rules(r1 = rules1, r2 = rules2, display = TRUE), "r1 & r2")
})
