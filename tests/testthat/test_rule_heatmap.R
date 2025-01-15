####### TEST INPUTS #######

data(Groceries)

rules1 <- apriori(Groceries,
                  parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
rules2 <- apriori(Groceries,
                  parameter = list(supp = 0.01, conf = 0.55, target = "rules"))
rules_empty <- apriori(Groceries,
                  parameter = list(supp = 1, conf = 1, target = "rules"))
large_rules <- apriori(Groceries,
                  parameter = list(supp = 0.001, conf = 0.3, target = "rules"))

####### TESTS #######

test_that("validate_rules_map() verifies the number and types of arguments correctly", {

  # multiple rules objects
  expect_error(
    validate_rules_map(list(r1=rules1, r2=rules2)),
    regexp="Input must be a single object of class 'rules'. Please provide a valid rule set."
  )

  # no rules objects
  expect_error(
    validate_rules_map(list()),
    regexp="Input must be a single object of class 'rules'. Please provide a valid rule set."
  )

  # non-rules arguments
  expect_error(
    validate_rules_map(NULL),
    regexp="Input must be a single object of class 'rules'. Please provide a valid rule set."
  )

  # empty ruleset
  expect_error(
    validate_rules_map(rules_empty),
    regexp="`rules` object is empty. Please provide a non-empty ruleset."
  )

  # valid rules
  expect_no_error(validate_rules_map(rules1))
})


test_that("rule_heatmap() validates rules objects", {

  expect_error(rule_heatmap(1),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_heatmap(NULL),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_heatmap(NA),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_heatmap(c("no simulation", "cross that line", "pasadena")),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_heatmap(matrix(1:3)),
               regexp="Input must be a single object of class 'rules'. Please provide a valid rule set.")
  expect_error(rule_heatmap(rules_empty),
               regexp="`rules` object is empty. Please provide a non-empty ruleset.")

  # why?
  expect_no_error(rule_heatmap(c(rules1, rules2)))
  expect_no_error(rule_heatmap(c(large_rules)))

})


test_that("validate_metric_map() validates `metric` arguments correctly", {

  # invalid metrics
  expect_error(
    validate_metric_map(NULL),
    regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric."
  )

  expect_error(
    validate_metric_map("mode"),
    regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric."
  )

  expect_error(
    validate_metric_map(123),
    regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric."
  )

  expect_error(
    validate_metric_map(list("confidence", "confidence")),
    regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric."
  )

  expect_error(
    validate_metric_map(c("confidence", "confidence")),
    regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric."
  )

  # valid metrics
  expect_no_error(validate_metric_map("ConFiDEnCe"))
  expect_no_error(validate_metric_map("LIFT"))
  expect_no_error(validate_metric_map("supPort"))
})


test_that("rule_heatmap() validates `metric` arguments correctly", {

  expect_error(rule_heatmap(rules1, metric=0),
               regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")
  expect_error(rule_heatmap(rules1, metric=c("confidence, support, lift")),
               regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")
  expect_error(rule_heatmap(rules1, metric=NULL),
               regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")
  expect_error(rule_heatmap(rules1, metric=NA),
               regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")
  expect_error(rule_heatmap(rules1, metric=rules1),
               regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")
  expect_error(rule_heatmap(rules1, metric=list(TRUE, "two", 3)),
               regexp="'metric' must be one of 'confidence', 'support', or 'lift'. Please provide a valid metric.")

  expect_no_error(rule_heatmap(rules1, metric="LIFT"))
  expect_no_error(rule_heatmap(rules1, metric="cOnFiDeNcE"))
  expect_no_error(rule_heatmap(rules1, metric="support"))

})


test_that("validate_title_map() validates title arguments correctly", {

  # non-character
  expect_error(
    validate_title_map(123),
    regexp="The graph and axis titles must be either NULL or a single non-NA character string."
  )

    expect_error(
    validate_title_map(list("hello")),
    regexp="The graph and axis titles must be either NULL or a single non-NA character string."
  )

  expect_error(
    validate_title_map(TRUE),
    regexp="The graph and axis titles must be either NULL or a single non-NA character string."
  )

    expect_error(
    validate_title_map(Inf),
    regexp="The graph and axis titles must be either NULL or a single non-NA character string."
  )

  # NA
  expect_error(
    validate_title_map(NA),
    regexp="The graph and axis titles must be either NULL or a single non-NA character string."
  )

  # mutliple titles
  expect_error(
    validate_title_map(c("title1", "title2", "title3")),
    regexp="The graph and axis titles must be either NULL or a single non-NA character string."
  )

  # valid title
  expect_no_error(validate_title_map("A good title"))
})


test_that("rule_heatmap() validates title arguments correctly", {

  # graph title
  expect_error(rule_heatmap(rules1, graph_title=1),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, graph_title=matrix(1:10)),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, graph_title=NA),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, graph_title=c(1, 2, 3)),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, graph_title=Inf),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, graph_title=rules1),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")

  # axis title
  expect_error(rule_heatmap(rules1, x_axis_title=1),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, y_axis_title=matrix(1:10)),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, x_axis_title=NA),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, y_axis_title=c(1, 2, 3)),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, x_axis_title=Inf),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")
  expect_error(rule_heatmap(rules1, y_axis_title=rules1),
               regexp="The graph and axis titles must be either NULL or a single non-NA character string.")

  expect_no_error(rule_heatmap(rules1, graph_title="Heatmap", x_axis_title="Before"))
  expect_no_error(rule_heatmap(rules1, graph_title=NULL))
  expect_no_error(rule_heatmap(rules1, graph_title="", x_axis_title="One", y_axis_title="Two"))

})


test_that("validate_color_map() handles valid hex color codes", {

  # valid codes
  expect_true(validate_color_map("#FFFFFF"))
  expect_true(validate_color_map("#000000"))
  expect_true(validate_color_map("#FF5733"))
  expect_true(validate_color_map("#123456"))

  # alpha channel
  expect_true(validate_color_map("#FFFFFFFF"))
  expect_true(validate_color_map("#12345678"))

  # R color names
  expect_true(validate_color_map("red"))
  expect_true(validate_color_map("blue"))
  expect_true(validate_color_map("green"))
  expect_true(validate_color_map("cyan"))

  # invalid color codes
  expect_error(validate_color_map("#FFF"))
  expect_error(validate_color_map("#GGGGGG"))
  expect_error(validate_color_map("#12345"))
  expect_error(validate_color_map("#123456789"))

  # invalid color names
  expect_error(validate_color_map("redd"))
  expect_error(validate_color_map("notacolor"))

  # non-character inputs
  expect_error(validate_color_map(12345))
  expect_error(validate_color_map(TRUE))
  expect_error(validate_color_map(NA))

  # empty or multiple inputs
  expect_error(validate_color_map(""))
  expect_error(validate_color_map(c("#FFFFFF", "red")))
})


test_that("rule_heatmap() validates `high_color` and `low_color` arguments correctly", {

  expect_error(rule_heatmap(rules1, high_color="#1"))
  expect_error(rule_heatmap(rules1, low_color=NULL))
  expect_error(rule_heatmap(rules1, high_color="#123"))
  expect_error(rule_heatmap(rules1, low_color=matrix(1:3)))
  expect_error(rule_heatmap(rules1, high_color=NA))
  expect_error(rule_heatmap(rules1, low_color=rules2))

  expect_no_error(rule_heatmap(rules1, high_color="#12345678"))
  expect_no_error(rule_heatmap(rules1, low_color="#77B777"))
  expect_no_error(rule_heatmap(rules1, high_color="#A7384F"))

})


test_that("validate_logical_map() accepts valid and rejects invalid logical inputs", {

  # valid inputs
  expect_no_error(validate_logical_map(TRUE))
  expect_no_error(validate_logical_map(FALSE))

  # invalid inputs
  expect_error(validate_logical_map(1))
  expect_error(validate_logical_map("TRUE"))
  expect_error(validate_logical_map(NA))
  expect_error(validate_logical_map(c(TRUE, FALSE)))
})


test_that("rule_heatmap() validates logical arguments correctly", {

  expect_error(rule_heatmap(rules1, include_zero="#1"),
               regexp="'include_zero' must be either 'TRUE' or 'FALSE'.")
  expect_error(rule_heatmap(rules1, include_zero=NULL),
               regexp="'include_zero' must be either 'TRUE' or 'FALSE'.")
  expect_error(rule_heatmap(rules1, include_zero="#123"))
  expect_error(rule_heatmap(rules1, include_zero=matrix(1:3)),
               regexp="'include_zero' must be either 'TRUE' or 'FALSE'.")
  expect_error(rule_heatmap(rules1, include_zero=NA),
               regexp="'include_zero' must be either 'TRUE' or 'FALSE'.")
  expect_error(rule_heatmap(rules1, include_zero=c(TRUE, FALSE)),
               regexp="'include_zero' must be either 'TRUE' or 'FALSE'.")

  expect_no_error(rule_heatmap(rules1, include_zero=TRUE))
  expect_no_error(rule_heatmap(rules1, include_zero=FALSE))

})


test_that("rule_heatmap() works with weird data", {

  # large datasets
  expect_no_error(rule_heatmap(large_rules, high_color="red", include_zero=TRUE))
  # weird characters
  expect_no_error(rule_heatmap(rules1, graph_title="Confidence Heatmap 🎨"))

})


test_that("validate_text_size accepts valid text size inputs", {

  # valid
  expect_silent(validate_text_size(10))
  expect_silent(validate_text_size(0.5))
  expect_silent(validate_text_size(1e-6))

  # non-numeric
  expect_error(validate_text_size("10"), "Text sizes must be single, non-infinite, positive, and numeric values.")
  expect_error(validate_text_size(TRUE), "Text sizes must be single, non-infinite, positive, and numeric values.")
  expect_error(validate_text_size(NULL), "Text sizes must be single, non-infinite, positive, and numeric values.")

  # non-single values
  expect_error(validate_text_size(c(10, 20)), "Text sizes must be single, non-infinite, positive, and numeric values.")
  expect_error(validate_text_size(numeric(0)), "Text sizes must be single, non-infinite, positive, and numeric values.") # Empty numeric vector

  # non-positive values
  expect_error(validate_text_size(0), "Text sizes must be single, non-infinite, positive, and numeric values.")
  expect_error(validate_text_size(-10), "Text sizes must be single, non-infinite, positive, and numeric values.")

  # infinite/NA
  expect_error(validate_text_size(NA), "Text sizes must be single, non-infinite, positive, and numeric values.")
  expect_error(validate_text_size(Inf), "Text sizes must be single, non-infinite, positive, and numeric values.")
  expect_error(validate_text_size(-Inf), "Text sizes must be single, non-infinite, positive, and numeric values.")
})


test_that("rule_heatmap() validates text sizes correctly", {

  # graph title size
  expect_error(
    rule_heatmap(rules1, graph_title_size="20"),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, graph_title_size=Inf),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, graph_title_size=NA),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, graph_title_size=0),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, graph_title_size=c(1, 2, 3, 4, 5)),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )

  # x_axis_title_size
  expect_error(
    rule_heatmap(rules1, x_axis_title_size=NULL),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_title_size=-Inf),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_title_size=NA),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_title_size=-1),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_title_size=list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5)),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )

  # y_axis_title_size
  expect_error(
    rule_heatmap(rules1, y_axis_title_size=TRUE),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_title_size=Inf),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_title_size=NA),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_title_size=-0.01),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_title_size=matrix(1:3)),
                 regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )

  # x_axis_text_size
  expect_error(
    rule_heatmap(rules1, x_axis_text_size="NULL"),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_text_size=Inf),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_text_size=NA),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_text_size=-100),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, x_axis_text_size=c(100, 200, 300)),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )

  # y_axis_text_size
  expect_error(
    rule_heatmap(rules1, y_axis_text_size="text"),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_text_size=Inf),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_text_size=NA),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_text_size=0),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, y_axis_text_size=matrix(1:3)),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )

  # legend_text_size
  expect_error(
    rule_heatmap(rules1, legend_text_size=FALSE),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, legend_text_size=-Inf),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, legend_text_size=NA),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, legend_text_size=-2),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
  expect_error(
    rule_heatmap(rules1, legend_text_size=matrix(1:5)),
    regexp = "Text sizes must be single, non-infinite, positive, and numeric values."
  )
})
