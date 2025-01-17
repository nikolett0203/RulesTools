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
)

large_rules <- apriori(
  Groceries,
  parameter = list(supp = 0.005, conf = 0.2, target = "rules")
)

rule_list1 <- list(r1 = rules1, r2 = rules2)

####### TESTS #######

test_that("validate_rules_euler() catches invalid rules arguments", {

  # non-list
  expect_error(
    validate_rules_euler(123),
    regexp="'rules' objects must be provided as a list."
  )
  expect_error(
    validate_rules_euler(NULL),
    regexp="'rules' objects must be provided as a list."
  )
  expect_error(
    validate_rules_euler(NA),
    regexp="'rules' objects must be provided as a list."
  )
  expect_error(
    validate_rules_euler(c(rules1, rules2)),
    regexp="'rules' objects must be provided as a list."
  )
  expect_error(
    validate_rules_euler("it ain't 2009 no more"),
    regexp="'rules' objects must be provided as a list."
  )

  # invalid number of rules
  expect_error(
    validate_rules_euler(list(rules1)),
    regexp="You must provide between 2 and 4 'rules' objects."
  )
  expect_error(
    validate_rules_euler(list(rules1, rules2, rules3, large_rules, rules1)),
    regexp="You must provide between 2 and 4 'rules' objects."
  )
  expect_error(
    validate_rules_euler(list(rules1, rules2, rules3, NULL)),
    regexp="The list contains NULL values. Please provide valid 'rules' objects."
  )

  # not rules objects
  expect_error(
    validate_rules_euler(list(rules1, rules2, rules3, 123)),
    regexp="All elements in the list must be 'rules' objects."
  )
  expect_error(
    validate_rules_euler(list(NA, "colours and shapes", 123)),
    regexp="All elements in the list must be 'rules' objects."
  )
  expect_error(
    validate_rules_euler(list(rules1, rules2, 123)),
    regexp="All elements in the list must be 'rules' objects."
  )

  # rules don't have to be named
  expect_no_error(validate_rules_euler(list(rules1, rules2)))
  expect_no_error(validate_rules_euler(list(rules1, rules2, rules3, large_rules)))
})


test_that("rule_euler() catches invalid rules arguments", {

  expect_error(
    rule_euler(1),
    regexp = "'rules' objects must be provided as a list."
  )
  expect_error(
    rule_euler(rules1),
    regexp = "'rules' objects must be provided as a list."
  )
  expect_error(
    rule_euler(c(rules1, rules2)),
    regexp = "'rules' objects must be provided as a list."
  )
  expect_error(
    rule_euler(matrix(1:3)),
    regexp = "'rules' objects must be provided as a list."
  )
  expect_error(
    rule_euler(matrix("khalid")),
    regexp = "'rules' objects must be provided as a list."
  )
  expect_error(
    rule_euler(NULL),
    regexp = "'rules' objects must be provided as a list."
  )
  expect_error(
    rule_euler(list(rules1)),
    regexp = "You must provide between 2 and 4 'rules' objects."
  )
  expect_error(
    rule_euler(list(rules1, rules2, rules3, large_rules, rules1)),
    regexp = "You must provide between 2 and 4 'rules' objects."
  )
  expect_error(
    rule_euler(list(rules1, rules2, rules3, NULL)),
    regexp = "The list contains NULL values. Please provide valid 'rules' objects."
  )
  expect_error(
    rule_euler(list(NULL, NULL, NULL, NULL)),
    regexp = "The list contains NULL values. Please provide valid 'rules' objects."
  )
  expect_error(
    rule_euler(list(rules1, NA, rules2)),
    regexp = "All elements in the list must be 'rules' objects."
  )
  expect_error(
    rule_euler(list(1, 2, 3)),
    regexp = "All elements in the list must be 'rules' objects."
  )
  expect_error(
    rule_euler(list("m.i.a.", "faded")),
    regexp = "All elements in the list must be 'rules' objects."
  )
  expect_error(
    rule_euler(list(rules1, matrix(1:3), TRUE)),
    regexp = "All elements in the list must be 'rules' objects."
  )

  expect_no_error(rule_euler(list(rules1, rules2)))
  expect_no_error(rule_euler(list(rules1, rules2, rules3)))
  expect_no_error(rule_euler(list(rules1, rules2, rules3, large_rules)))
})


test_that("validate_alpha_euler() catches invalid alpha arguments", {

  # non-numeric
  expect_error(
    validate_alpha_euler("1"),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )
  expect_error(
    validate_alpha_euler(NULL),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )
  expect_error(
    validate_alpha_euler(NA),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )
  expect_error(
    validate_alpha_euler(FALSE),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )

  # non-scalar
  expect_error(
    validate_alpha_euler(c(1, 2, 3)),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )
  expect_error(
    validate_alpha_euler(matrix(1:3)),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )

  # out-of-range
  expect_error(
    validate_alpha_euler(-0.01),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )
  expect_error(
    validate_alpha_euler(1.1),
    regexp="`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_no_error(validate_alpha_euler(0))
  expect_no_error(validate_alpha_euler(1))
  expect_no_error(validate_alpha_euler(0.7))
})


test_that("rule_euler() catches invalid fill_alpha arguments", {
  expect_error(
    rule_euler(rule_list1, fill_alpha = 1.5),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = -1.5),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = c(0.5, 0.3)),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = list(0.1, 0.2, 0.3)),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = NULL),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = NA),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = matrix(1:3)),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = "alpha"),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_error(
    rule_euler(rule_list1, fill_alpha = "TRUE"),
    regexp = "`fill_alpha` must be a single numeric value between 0 and 1."
  )

  expect_no_error(rule_euler(rule_list1, fill_alpha = 0.75))
  expect_no_error(rule_euler(rule_list1, fill_alpha = 0))
  expect_no_error(rule_euler(rule_list1, fill_alpha = 1.0))
  expect_no_error(rule_euler(rule_list1, fill_alpha = 0.785))
  expect_no_error(rule_euler(rule_list1, fill_alpha = 1e-10))
  expect_no_error(rule_euler(rule_list1))
})


test_that("validate_title_euler() rejects invalid titles", {

  # non-character
  expect_error(
    validate_title_euler(123),
    regexp="The graph title must be either NULL or a single non-NA character string."
  )
  expect_error(
    validate_title_euler(matrix(1:3)),
    regexp="The graph title must be either NULL or a single non-NA character string."
  )
  expect_error(
    validate_title_euler(NA),
    regexp="The graph title must be either NULL or a single non-NA character string."
  )
  expect_error(
    validate_title_euler(TRUE),
    regexp="The graph title must be either NULL or a single non-NA character string."
  )

  # multiple strings
  expect_error(
    validate_title_euler(c("title1", "title2")),
    regexp="The graph title must be either NULL or a single non-NA character string."
  )
  expect_error(
    validate_title_euler(character(0)),
    regexp="The graph title must be either NULL or a single non-NA character string."
  )

  expect_no_error(validate_title_euler("title"))
  expect_no_error(validate_title_euler("orosháza"))
})


test_that("rule_euler() catches invalid title arguments", {
  expect_error(
    rule_euler(rule_list1, title = 1.5),
    regexp = "The graph title must be either NULL or a single non-NA character string."
  )

  expect_error(
    rule_euler(rule_list1, title = c(0.5, 0.3)),
    regexp = "The graph title must be either NULL or a single non-NA character string."
  )

  expect_error(
    rule_euler(rule_list1, title = list(0.1, "two", FALSE)),
    regexp = "The graph title must be either NULL or a single non-NA character string."
  )

  expect_error(
    rule_euler(rule_list1, title = NA),
    regexp = "The graph title must be either NULL or a single non-NA character string."
  )

  expect_error(
    rule_euler(rule_list1, title = matrix(1:3)),
    regexp = "The graph title must be either NULL or a single non-NA character string."
  )

  expect_error(
    rule_euler(rule_list1, title = TRUE),
    regexp = "The graph title must be either NULL or a single non-NA character string."
  )

  expect_no_error(rule_euler(rule_list1, title = "euler"))
  expect_no_error(rule_euler(rule_list1, title = "I switched the time zone, but what do I know? Spendin' nights hitchhikin', where will I go? I could fly home, with my eyes closed But it'd get kinda hard to see, that's no surprise though."))
  expect_no_error(rule_euler(rule_list1, title = ""))
  expect_no_error(rule_euler(rule_list1, title = "测试标题"))
  expect_no_error(rule_euler(rule_list1, title = NULL))
})


test_that("validate_color_euler() rejects invalid colour inputs", {

  # invalid colors
  expect_error(
    validate_color_euler("notacolor", "color"),
    regexp="must be a valid 6-8 digit hex color code"
  )
  expect_error(
    validate_color_euler(TRUE, "color"),
    regexp="must be a valid 6-8 digit hex color code"
  )
  expect_error(
    validate_color_euler(matrix(1:7), "color"),
    regexp="must be a valid 6-8 digit hex color code"
  )
  expect_error(
    validate_color_euler(123, "color"),
    regexp="must be a valid 6-8 digit hex color code"
  )

  # multiple colours
  expect_error(
    validate_color_euler(c("blue", "purple"), "color"),
    regexp="must be a valid 6-8 digit hex color code"
  )
  expect_error(
    validate_color_euler(list("#238271", "red", "#948382"), "color"),
    regexp="must be a valid 6-8 digit hex color code"
  )

  # valid colours
  expect_no_error(validate_color_euler("#128738", "color"))
  expect_no_error(validate_color_euler("darkgreen", "color"))
})


test_that("rule_euler() catches invalid colors", {

  expect_error(
    rule_euler(rule_list1, stroke_color = "#12FG34"),
    regexp = "'stroke_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, name_color = "notacolor"),
    regexp = "'name_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, text_color = 12345),
    regexp = "'text_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, fill_color = list("blue", "#GGGGGG")),
    regexp = "'fill_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, fill_color = TRUE),
    regexp = "'fill_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, stroke_color = NULL),
    regexp = "'stroke_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, name_color = ""),
    regexp = "'name_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, name_color = matrix(1:3)),
    regexp = "'name_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, text_color = NA),
    regexp = "'text_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, text_color = c("blue", "pink")),
    regexp = "'text_color' must be a valid 6-8 digit hex color code"
  )
  expect_error(
    rule_euler(rule_list1, fill_color = c("#FF0000", "invalidcolor")),
    regexp = "'fill_color' must be a valid 6-8 digit hex color code"
  )

  expect_no_error(rule_euler(rule_list1, fill_color = c("#FF0000", "#00FF00", "#0000FFFF")))
  expect_no_error(rule_euler(rule_list1, stroke_color = "blue"))
  expect_no_error(rule_euler(rule_list1, stroke_color = "#FF573333"))
  expect_no_error(rule_euler(rule_list1, name_color = "green"))
  expect_no_error(rule_euler(rule_list1, text_color = "#1A2B3C"))
  expect_no_error(rule_euler(rule_list1, fill_color = c("cyan", "#ABCDEF")))
})


test_that("rule_euler() runs with valid arguments", {
  expect_no_error(
    rule_euler(
      rule_list1,
      fill_color  = c("blue", "purple", "green"),
      fill_alpha  = 0.3,
      stroke_color = "grey",
      stroke_size = 2,
      title       = "Euler Diagram of Association Rules",
      name_color  = "red",
      name_size   = 11,
      text_color  = "#BFE736",
      text_size   = 10
    )
  )

  expect_no_error(
    rule_euler(
      list(rules1, rules2, large_rules),
      fill_color  = c("#A318E3", "#B1EEF2", "#333FFF"),
      fill_alpha  = 0.888,
      stroke_color = "#222111",
      stroke_size = 5,
      title       = "",
      name_color  = "#000000",
      name_size   = 6,
      text_color  = "#ffffff",
      text_size   = 4
    )
  )
})


test_that("validate_numeric_euler() rejects invalid numerical arguments", {

  # non-numeric
  expect_error(
    validate_numeric_euler("text", "text size"),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    validate_numeric_euler(NA, "text size"),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    validate_numeric_euler(NULL, "text size"),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    validate_numeric_euler(FALSE, "text size"),
    regexp="must be a finite positive numeric value."
  )

  # multiple arguments
  expect_error(
    validate_numeric_euler(c(1, 3, 10), "text size"),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    validate_numeric_euler(list(1, 3, 10), "text size"),
    regexp="must be a finite positive numeric value."
  )

  # infinity
  expect_error(
    validate_numeric_euler(Inf, "text size"),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    validate_numeric_euler(-Inf, "text size"),
    regexp="must be a finite positive numeric value."
  )

  # negative values
  expect_error(
    validate_numeric_euler(-10, "text size"),
    regexp="must be a finite positive numeric value."
  )
  # negative values
  expect_error(
    validate_numeric_euler(-0.01, "text size"),
    regexp="must be a finite positive numeric value."
  )

  expect_no_error(validate_numeric_euler(10, "stroke size"))
  expect_no_error(validate_numeric_euler(200, "stroke size"))
  expect_no_error(validate_numeric_euler(1, "stroke size"))
})


test_that("rule_euler() rejects invalid numerical arguments", {

  # non-numeric
  expect_error(
    rule_euler(list(rules1, rules2), stroke_size=NA),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    rule_euler(list(rules1, rules2), name_size="123"),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    rule_euler(list(rules1, rules2), text_size=TRUE),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    rule_euler(list(rules1, rules2), stroke_size=Inf),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    rule_euler(list(rules1, rules2), name_size=c(1, 2, 3)),
    regexp="must be a finite positive numeric value."
  )
  expect_error(
    rule_euler(list(rules1, rules2), text_size=NULL),
    regexp="must be a finite positive numeric value."
  )

  expect_no_error(rule_euler(list(rules1, rules2), text_size=1))
  expect_no_error(rule_euler(list(rules1, rules2), name_size=100))
  expect_no_error(rule_euler(list(rules1, rules2), stroke_size=27))
  expect_no_error(rule_euler(list(rules1, rules2),
                             text_size=17,
                             name_size=50,
                             stroke_size=75))
})


test_that("validate_legend_pos_euler() rejects invalid positions", {

  # non-character
  expect_error(
    validate_legend_pos_euler(123),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    validate_legend_pos_euler(NA),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    validate_legend_pos_euler(NULL),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    validate_legend_pos_euler(matrix(1:7)),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    validate_legend_pos_euler(FALSE),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )

  # multiple inputs
  expect_error(
    validate_legend_pos_euler(c("top", "bottom")),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    validate_legend_pos_euler(c("top", "bottom", "left")),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )

  expect_no_error(validate_legend_pos_euler("top"))
  expect_no_error(validate_legend_pos_euler("bOTTOM"))
  expect_no_error(validate_legend_pos_euler("lEfT"))
  expect_no_error(validate_legend_pos_euler("rigHT"))
})


test_that("rule_euler() rejects invalid legend positions", {

  # non-character
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=300),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=NA),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=NULL),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=matrix(1:7)),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=FALSE),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )

  # multiple inputs
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=c("top", "right")),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )
  expect_error(
    rule_euler(list(rules1, rules2), legend_position=character(0)),
    regexp="Invalid legend position. Choose from 'right', 'left', 'top', or 'bottom'."
  )

  expect_no_error(rule_euler(list(rules1, rules2), legend_position="TOP"))
  expect_no_error(rule_euler(list(rules1, rules2), legend_position="bottom"))
  expect_no_error(rule_euler(list(rules1, rules2), legend_position="RiGhT"))
  expect_no_error(rule_euler(list(rules1, rules2), legend_position="leFT"))
})


test_that("validate_logical_euler() accepts valid and rejects invalid logical inputs", {

  # valid inputs
  expect_no_error(validate_logical_euler(TRUE))
  expect_no_error(validate_logical_euler(FALSE))

  # invalid inputs
  expect_error(validate_logical_euler(1))
  expect_error(validate_logical_euler("TRUE"))
  expect_error(validate_logical_euler(NA))
  expect_error(validate_logical_euler(c(TRUE, FALSE)))
})

test_that("rule_euler() accepts valid and rejects invalid logical inputs", {

  # valid inputs
  expect_no_error(rule_euler(list(rules1, rules2), show_legend=TRUE))
  expect_no_error(rule_euler(list(rules1, rules2), show_legend=FALSE))

  # invalid inputs
  expect_error(rule_euler(list(rules1, rules2), show_legend=1))
  expect_error(rule_euler(list(rules1, rules2), show_legend="true"))
  expect_error(rule_euler(list(rules1, rules2), show_legend=NA))
  expect_error(rule_euler(list(rules1, rules2), show_legend=c(TRUE, FALSE)))
})


test_that("validate_row_col() rejects invalid row/column arguments", {

  # non-numeric inputs
  expect_error(
    validate_row_col("one", "two", "three"),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(NULL, 1, 2),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(NA, NA, 20),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(matrix(1:3), 2, 4),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(4, TRUE, 4),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(list(1, 2, 3), 3, 4),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(-2, -3, 6),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    validate_row_col(0, 0, 6),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )

  # invalid combinations
  expect_error(
    validate_row_col(2, 3, 7),
    regexp="in the legend layout must match the number of sets"
  )
  expect_error(
    validate_row_col(10, 2, 11),
    regexp="in the legend layout must match the number of sets"
  )
  expect_error(
    validate_row_col(1, 1, 2),
    regexp="in the legend layout must match the number of sets"
  )

  # valid numbers
  expect_no_error(validate_row_col(2, 2, 4))
  expect_no_error(validate_row_col(1, 3, 3))
  expect_no_error(validate_row_col(4, 1, 4))
  expect_no_error(validate_row_col(NULL, NULL, 2))
})


test_that("rule_euler() rejects invalid row/column arguments", {

  # non-numeric inputs
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow="one", ncol="two"),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=NULL, ncol=3),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=NA, ncol=NA),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=3, ncol=matrix(1:7)),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=5, ncol=FALSE),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=list(1, 2, 3), ncol=4),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=-1, ncol=-2),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=1, ncol=0),
    regexp="'nrow' and 'ncol' must be single positive integers."
  )

  # invalid combinations
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=1, ncol=3),
    regexp="in the legend layout must match the number of sets"
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=10, ncol=1),
    regexp="in the legend layout must match the number of sets"
  )
  expect_error(
    rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=1, ncol=1),
    regexp="in the legend layout must match the number of sets"
  )

  # valid numbers
  expect_no_error(rule_euler(list(rules1, rules2), show_legend=TRUE, nrow=1, ncol=2))
  expect_no_error(rule_euler(list(rules1, rules2, rules3), show_legend=TRUE, nrow=3, ncol=1))
  expect_no_error(rule_euler(list(rules1, rules3), show_legend=TRUE, nrow=NULL, ncol=NULL))
})


test_that("rule_euler() works under general tests", {
  expect_no_error(
    rule_euler(
      list(rules1, rules2, rules3, large_rules),
      fill_color=c("#89CFF0", "#CCCCFF", "#9FE2BF", "#03045E"),
      fill_alpha=1,
      stroke_color="#ffffff",
      stroke_size=2,
      title="Rule Subsets",
      name_color="#001111",
      name_size=14,
      text_color="#ffffff",
      text_size=12,
      show_legend=TRUE,
      legend_position="TOP",
      nrow=1,
      ncol=4)
  )

  expect_no_error(
    rule_euler(
      list(rules1, rules2, rules3, large_rules),
      fill_color=c("blue", "red", "yellow", "green"),
      fill_alpha=0.7,
      stroke_color="purple",
      stroke_size=3,
      title="RULES",
      name_color="black",
      name_size=10,
      text_color="black",
      text_size=8,
      show_legend=TRUE,
      legend_position="riGHT",
      nrow=4,
      ncol=1)
  )

  expect_no_error(
    rule_euler(
      list(rules1, rules2, rules3, large_rules),
      fill_color=c("#ffba49", "#20a39e", "#ef5b5b", "#23001e"),
      fill_alpha=0.7,
      stroke_color="#152515",
      stroke_size=0.5,
      title="More rules",
      name_color="darkgrey",
      name_size=10.5,
      text_color="black",
      text_size=10.5,
      show_legend=TRUE,
      legend_position="left",
      nrow=2,
      ncol=2)
  )

  expect_no_error(
    rule_euler(
      list(rules1, rules2, rules3),
      fill_color=c("#d00000", "#ffba08", "#3f88c5"),
      fill_alpha=0.9,
      stroke_color="black",
      stroke_size=1,
      title="Romanian plot",
      text_color="white",
      text_size=12,
      show_legend=TRUE,
      legend_position="BOTTOM",
      nrow=3,
      ncol=1)
  )

  expect_no_error(
    rule_euler(
      list(rules2, rules3),
      fill_color=c("navy", "seagreen"),
      fill_alpha=0.7,
      stroke_color="#152515",
      stroke_size=0.5,
      title="No overlap",
      name_color="white",
      name_size=10,
      text_color="white",
      text_size=7,
      show_legend=FALSE,
      legend_position="left",
      nrow=2,
      ncol=1)
  )

  expect_no_error(
    rule_euler(
      list("Raw"=rules1, "Subset"=rules3),
      fill_color=c("#12b7bbff", "#48209287"),
      fill_alpha=0.7,
      stroke_color="green",
      stroke_size=0.5,
      title="No overlap",
      name_color="white",
      name_size=10,
      text_color="white",
      text_size=7,
      show_legend=FALSE,
      legend_position="left",
      nrow=2,
      ncol=1)
  )
})
