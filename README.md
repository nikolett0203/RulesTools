# RulesTools: Tools for Preparing, Analyzing, and Visualizing Association Rules
![](https://cranlogs.r-pkg.org/badges/RulesTools)](https://cran.rstudio.com/web/packages/RulesTools/index.html)
	

**RulesTools** is an R package designed to streamline association rule mining workflows. It provides functions for preparing datasets, analyzing generated rules, and visualizing results using heatmaps and Euler diagrams.


## Navigate To...
- [Key Features](#key-features)
  - [Discretization Functions](#discretization-functions)
    - [`dtize_col` Function: Discretize a Numeric Column](#dtize_col-function-discretize-a-numeric-column)
    - [`dtize_df` Function: Discretize Dataframe Columns](#dtize_df-function-discretize-dataframe-columns)
  - [Rule Analysis Functions](#rule-analysis-functions)
    - [`compare_rules` Function: Compare and Find Intersections of Association Rule Sets](#compare_rules-function-compare-and-find-intersections-of-association-rule-sets)
  - [Visualization Functions](#visualization-functions)
    - [`rule_euler` Function: Create an Euler Diagram for Association Rules](#rule_euler-function-create-an-euler-diagram-for-association-rules)
    - [`rule_heatmap` Function: Create a Heatmap for Association Rules](#rule_heatmap-function-create-a-heatmap-for-association-rules)
- [Brook Trout Dataset](#brook-trout-dataset)
- [Citations](#citations)


## Key Features

- **Discretization Tools:** Convert continuous data into discrete categories for rule mining.
- **Rule Comparison:** Identify and visualize intersections of multiple rule sets.
- **Visualization:** Create insightful heatmaps and customized Euler diagrams for rule interpretation.
- **BrookTrout Dataset:** Includes the `BrookTrout` dataset, which contains environmental metadata to explore how these variables influence high eDNA concentrations in aquatic samples. The dataset is derived from:

  **Nolan, K. P., et al. (2022).** *Detection of brook trout in spatiotemporally separate locations using validated eDNA technology.* Journal of Environmental Studies and Sciences, 13, 66–82. [https://doi.org/10.1007/s13412-022-00800-x](https://doi.org/10.1007/s13412-022-00800-x)


## Discretization Functions


### `dtize_col` Function: Discretize a Numeric Column

#### Purpose

The `dtize_col` function discretizes a numeric vector into categories based on specified cutoff points. It supports predefined cutoffs (such as the mean or median), handles missing values, and allows for infinite bounds. This is useful for transforming continuous data into categorical intervals for association rule mining.

#### Parameters

- **`column`** *(Numeric vector)*: The numeric vector to discretize.
- **`cutoff`** *(Numeric vector or string)*: Cutoff points for discretization, or a predefined string (`"mean"` or `"median"`). Default is `"median"`.
- **`labels`** *(Character vector)*: Labels for the resulting categories. Default is `c("low", "high")`.
- **`include_right`** *(Logical)*: If `TRUE`, intervals are closed on the right. Default is `TRUE`.
- **`infinity`** *(Logical)*: If `TRUE`, extends cutoffs to `-Inf` and `Inf`. Default is `TRUE`.
- **`include_lowest`** *(Logical)*: If `TRUE`, the lowest interval is closed on the left. Default is `TRUE`.
- **`na_fill`** *(String)*: Method to impute missing values: `"none"`, `"mean"`, or `"median"`. Default is `"none"`.

#### Return Value

A vector with the same length as `column`, where each value is categorized based on the specified cutoffs.

#### How It Works

1. **Validation**: Ensures inputs are valid, including logical parameters, cutoff points, and labels.
2. **Cutoff Handling**: Uses specified cutoffs or calculates cutoffs based on the mean or median.
3. **Interval Assignment**: Categorizes values based on the cutoffs and labels.
4. **Missing Value Imputation**: Optionally fills `NA` values with the mean or median before discretization.

#### Example Usage

```r
data(BrookTrout)

# Example with predefined cutoffs
discrete_conc <- dtize_col(
  BrookTrout$eDNAConc,
  cutoff = 13.3,
  labels = c("low", "high"),
  infinity = TRUE
)

# Example with median as cutoff
discrete_pH <- dtize_col(BrookTrout$pH, cutoff = "median")

# Example with missing value imputation
filled_col <- dtize_col(
  c(1, 2, NA, 4, 5),
  cutoff = "mean",
  include_right = FALSE,
  na_fill = "mean"
)
```


### `dtize_df` Function: Discretize Dataframe Columns

#### Purpose

The `dtize_df` function discretizes numeric columns in a dataframe based on specified splitting criteria. It also handles missing values using various imputation methods, making it useful for preparing data for association rule mining.

#### Parameters

- **`data`** *(Dataframe)*: The dataframe containing the data to be discretized.
- **`cutoff`** *(Character string or numeric vector)*: The method for splitting numeric columns. Options are `"median"` (default), `"mean"`, or a custom numeric vector of split points.
- **`labels`** *(Character vector)*: Labels for the discretized categories. Default is `c("low", "high")`.
- **`include_right`** *(Logical)*: If `TRUE`, intervals are closed on the right. Default is `TRUE`.
- **`infinity`** *(Logical)*: If `TRUE`, extends intervals to `-Inf` and `Inf`. Default is `TRUE`.
- **`include_lowest`** *(Logical)*: If `TRUE`, the lowest interval is closed on the left. Default is `TRUE`.
- **`na_fill`** *(Character string)*: Method to impute missing values. Options are `"none"` (default), `"mean"`, `"median"`, or `"pmm"` (predictive mean matching).
- **`m`** *(Integer)*: Number of multiple imputations if `na_fill = "pmm"`. Default is `5`.
- **`maxit`** *(Integer)*: Maximum number of iterations for the `mice` algorithm. Default is `5`.
- **`seed`** *(Integer)*: Seed for reproducibility of the imputation process. Default is `NULL`.
- **`printFlag`** *(Logical)*: If `TRUE`, prints logs during the `mice` imputation process. Default is `FALSE`.

#### Return Value

A dataframe with numeric columns discretized and missing values handled based on the specified imputation method.

#### How It Works

1. **Validation**: Checks that the input is a valid dataframe.
2. **Missing Value Imputation**: Handles missing values using the specified `na_fill` method, including predictive mean matching (`pmm`) via the `mice` package.
3. **Column Discretization**: Discretizes each numeric column based on the specified cutoff and labels.
4. **Non-Numeric Handling**: Non-numeric columns are converted to factors.

#### Example Usage

```r
data(BrookTrout)

# Example with median as cutoff
med_df <- dtize_df(
  BrookTrout, 
  cutoff = "median", 
  labels = c("below median", "above median")
)

# Example with mean as cutoff and left-closed intervals
mean_df <- dtize_df(
  BrookTrout, 
  cutoff = "mean", 
  include_right = FALSE
)

# Example with missing value imputation using predictive mean matching (pmm)
air <- dtize_df(
  airquality, 
  cutoff = "mean", 
  na_fill = "pmm", 
  m = 10, 
  maxit = 10, 
  seed = 42
)
```


## Rule Analysis Functions


### `compare_rules` Function: Compare and Find Intersections of Association Rule Sets

#### Purpose

The `compare_rules` function helps you compare multiple sets of association rules, identify their intersections, and optionally save the results to a CSV file. This function is particularly useful for exploring how rule sets generated under different parameters overlap or differ.

#### Parameters

- **`...`**: Named association rule sets (objects of class `rules`).
- **`display`** *(Logical)*: If `TRUE`, prints the intersection results. Default is `TRUE`.
- **`filename`** *(Character string)*: If provided, writes the results to a CSV file. Default is `NULL`.

#### Return Value

A list containing the intersections of the provided rule sets.

#### How It Works

1. **Input Rule Sets**: Pass multiple named rule sets to the function.
2. **Validation**: Ensures that inputs are valid rule sets and that parameters are correctly specified.
3. **Intersection Calculation**: Finds intersections between all combinations of the rule sets.
4. **Output**: Displays the results in the console and/or saves them to a CSV file.

#### Example Usage

```r
library(arules)
data(BrookTrout)

# Discretize the BrookTrout dataset
discrete_bt <- dtize_df(BrookTrout, cutoff = "mean")

# Generate the first set of rules with a confidence threshold of 0.5
rules1 <- apriori(
  discrete_bt,
  parameter = list(supp = 0.01, conf = 0.5, target = "rules")
)

# Generate the second set of rules with a higher confidence threshold of 0.6
rules2 <- apriori(
  discrete_bt,
  parameter = list(supp = 0.01, conf = 0.6, target = "rules")
)

# Compare the two sets of rules and display the intersections
compare_rules(
  r1 = rules1, 
  r2 = rules2, 
  display = TRUE, 
  filename = "intersections.csv"
)

# The intersections are saved in 'intersections.csv'
```


## Visualization Functions


### `rule_euler` Function: Create an Euler Diagram for Association Rules

#### Purpose

The `rule_euler` function generates an Euler diagram visualization for up to 4 sets of association rules. It helps display the relationships and overlaps between rule sets, with customizable options for colors, transparency, and labels.

#### Parameters

- **`rules`** *(List of `rules` objects)*: A list containing between 2 and 4 `rules` objects from the `arules` package.
- **`fill_color`** *(Character vector)*: Colors for filling the sets. If `NULL`, default colors `c("red", "blue", "green", "purple")` are used. Default is `NULL`.
- **`fill_alpha`** *(Numeric)*: Transparency of the fill colors (between 0 and 1). Default is `0.5`.
- **`stroke_color`** *(Character string)*: Color for the set borders. Default is `"black"`.
- **`stroke_size`** *(Numeric)*: Size of the set borders. Default is `1`.
- **`title`** *(Character string)*: Title of the Euler diagram. Default is `NULL`.
- **`name_color`** *(Character string)*: Color of the set names. Default is `"black"`.
- **`name_size`** *(Numeric)*: Font size of the set names. Default is `12`.
- **`text_color`** *(Character string)*: Color of the quantity labels (counts) in the diagram. Default is `"black"`.
- **`text_size`** *(Numeric)*: Font size of the quantity labels. Default is `11`.
- **`show_legend`** *(Logical)*: If `TRUE`, displays legend for the sets rather than labels. Defaults to `FALSE`.
- **`legend_position`** *(Character string)*: specifies the position of the legend. Must be one of `"top"`, `"bottom"`, `"left"`, or `"right"`. Defaults to `"bottom"`.
- **`nrow`** *(Numeric)*: Specifies the number of rows in the legend layout. If `NULL`, the number of rows is calculated automatically. Defaults to `NULL`.
- **`ncol`** *(Numeric)*: specifies the number of columns in the legend layout. If `NULL`, the number of columns is calculated automatically. Defaults to `NULL`.

#### Return Value

A `plot` object displaying the Euler diagram visualization.

#### How It Works

1. **Validation**: Checks that the input is a valid list of 2 to 4 `rules` objects.
2. **Customization**: Allows setting custom colors, transparency, and labels for the diagram.
3. **Plot Generation**: Uses the `eulerr` package to generate and display the Euler diagram.

#### Example Usage

```r
library(arules)
data(BrookTrout)

# Discretize the BrookTrout dataset
discrete_bt <- dtize_df(BrookTrout, cutoff = "median")

# Generate the first set of rules with a confidence threshold of 0.5
rules1 <- apriori(
  discrete_bt,
  parameter = list(supp = 0.01, conf = 0.5, target = "rules")
)

# Generate the second set of rules with a higher confidence threshold of 0.6
rules2 <- apriori(
  discrete_bt,
  parameter = list(supp = 0.01, conf = 0.6, target = "rules")
)

# Create an Euler diagram to visualize the intersections between the rule sets
rule_euler(
  rules = list(conf0.5 = rules1, conf0.6 = rules2),
  title = "Euler Diagram of BrookTrout Rule Sets",
  fill_color = c("#7832ff", "lightgreen"),
  stroke_color = "darkblue"
)
```


### `rule_heatmap` Function: Create a Heatmap for Association Rules

#### Purpose

The `rule_heatmap` function generates a heatmap visualization of association rules, showing the relationships between antecedents and consequents based on a specified metric. This visualization helps identify patterns and strengths of associations in the rule set.

#### Parameters

- **`rules`** *(`rules` object)*: An object of class `rules` from the `arules` package.
- **`metric`** *(Character string)*: The metric to use for coloring the heatmap. Options are `"confidence"` (default), `"support"`, or `"lift"`.
- **`graph_title`** *(Character string)*: Title of the heatmap. Default is an empty string (`""`).
- **`graph_title_size`** *(Numeric)*: Size of the graph title text. Default is `14`.
- **`x_axis_title`** *(Character string)*: Title for the x-axis. Default is `"Antecedents"`.
- **`x_axis_title_size`** *(Numeric)*: Size of the x-axis title text. Default is `12`.
- **`x_axis_text_size`** *(Numeric)*: Size of the x-axis text. Default is `11`.
- **`x_axis_text_angle`** *(Numeric)*: Angle of the x-axis text. Default is `45`.
- **`y_axis_title`** *(Character string)*: Title for the y-axis. Default is `"Consequents"`.
- **`y_axis_title_size`** *(Numeric)*: Size of the y-axis title text. Default is `12`.
- **`y_axis_text_size`** *(Numeric)*: Size of the y-axis text. Default is `11`.
- **`y_axis_text_angle`** *(Numeric)*: Angle of the y-axis text. Default is `0`.
- **`legend_title`** *(Character string)*: Title of the legend. Defaults to the value of `metric`.
- **`legend_text_size`** *(Numeric)*: Size of the legend text. Default is `8`.
- **`legend_position`** *(Character string)*: Position of the legend. Options are `"right"` (default), `"left"`, `"top"`, `"bottom"`, or `"none"`.
- **`low_color`** *(Character string)*: Color for the lower bound of the gradient. Default is `"lightblue"`.
- **`high_color`** *(Character string)*: Color for the upper bound of the gradient. Default is `"navy"`.
- **`include_zero`** *(Logical)*: If `TRUE`, includes zero values for missing antecedent-consequent combinations. Default is `FALSE`.


#### Return Value

A `ggplot` object representing the heatmap visualization of the association rules.

#### How It Works

1. **Validation**: Ensures the input is a valid `rules` object and parameters are correctly specified.
2. **Data Preparation**: Extracts antecedents, consequents, and the specified metric from the rule set.
3. **Optional Zero Inclusion**: Fills missing combinations with zeros if `include_zero = TRUE`.
4. **Plot Generation**: Uses `ggplot2` to create a heatmap with a gradient color scale based on the chosen metric.

#### Example Usage

```r
library(arules)
library(tidyr)
data(BrookTrout)

# Discretize the BrookTrout dataset
discrete_bt <- dtize_df(BrookTrout, cutoff = "median")

# Generate rules with a confidence threshold of 0.5
rules <- apriori(
  discrete_bt,
  parameter = list(supp = 0.01, conf = 0.5, target = "rules"),
  appearance = list(rhs = "eDNAConc=high")
)

# Subset ruleset to avoid redundancy and select significant rules
rules <- rules %>%
  subset(!is.redundant(., measure = "confidence")) %>%
  subset(is.significant(., alpha = 0.05)) %>%
  sort(by = c("confidence", "lift", "support"))

# Create a heatmap using confidence as the metric
rule_heatmap(
  rules,
  metric = "confidence",
  graph_title = "Confidence Heatmap"
)

# Create a heatmap using lift as the metric with custom colors
rule_heatmap(
  rules,
  metric = "lift",
  graph_title = "Lift Heatmap",
  low_color = "#D4A221",
  high_color = "darkgreen"
)
```


## Brook Trout Dataset

### Overview

The **`BrookTrout`** dataset included in the **RulesTools** package provides environmental metadata to explore factors influencing high eDNA concentrations in aquatic samples. This dataset is derived from a study conducted in **Hanlon Creek (Guelph, ON, Canada)** in **September 2019**.

### Dataset Details

- **Transactions**: 126  
- **Variables**: 10  

The dataset includes the following environmental and biological variables:

- **eDNA Concentrations** (measured via qPCR)
- **Brook Trout Counts** (measured via electrofishing)
- **Abiotic Characteristics**:  
  - Backpack (i.e. eDNA sampler type)
  - Site
  - Air Temperature  
  - Water Temperature  
  - Water pH  
  - Dissolved Oxygen  
  - Water conductivity  
  - Water Volume  

### Example Usage

```r
# Load the dataset
data(BrookTrout)

# View the first few rows
head(BrookTrout)

# Summary statistics
summary(BrookTrout)
```


### Citations

- **Bache, S. M., & Wickham, H. (2022).**  
  *magrittr: A Forward-Pipe Operator for R.* R package version 2.0.3.  
  [https://CRAN.R-project.org/package=magrittr](https://CRAN.R-project.org/package=magrittr)

- **Hahsler, M., Buchta, C., Gruen, B., & Hornik, K. (2023).**  
  *arules: Mining Association Rules and Frequent Itemsets.* R package version 1.7-7.  
  [https://CRAN.R-project.org/package=arules](https://CRAN.R-project.org/package=arules)

- **Larsson, J. (2023).**  
  *eulerr: Area-Proportional Euler and Venn Diagrams with Ellipses.* R package version 7.0.0.  
  [https://CRAN.R-project.org/package=eulerr](https://CRAN.R-project.org/package=eulerr)

- **Nolan, K. P., et al. (2022).**  
  *Detection of brook trout in spatiotemporally separate locations using validated eDNA technology.*  
  *Journal of Environmental Studies and Sciences*, 13, 66–82.  
  [https://doi.org/10.1007/s13412-022-00800-x](https://doi.org/10.1007/s13412-022-00800-x)

- **van Buuren, S., & Groothuis-Oudshoorn, K. (2011).**  
  *mice: Multivariate Imputation by Chained Equations in R.* Journal of Statistical Software, 45(3), 1–67.  
  [https://CRAN.R-project.org/package=mice](https://CRAN.R-project.org/package=mice)

- **Wickham, H. (2011).**  
  *testthat: Unit Testing for R.* Journal of Statistical Software, 40(2), 1–24.  
  [https://CRAN.R-project.org/package=testthat](https://CRAN.R-project.org/package=testthat)

- **Wickham, H. (2016).**  
  *ggplot2: Elegant Graphics for Data Analysis.* Springer-Verlag New York.  
  [https://CRAN.R-project.org/package=ggplot2](https://CRAN.R-project.org/package=ggplot2)

- **Wickham, H., & Girlich, M. (2023).**  
  *tidyr: Tidy Messy Data.* R package version 1.3.0.  
  [https://CRAN.R-project.org/package=tidyr](https://CRAN.R-project.org/package=tidyr)

- **Xie, Y. (2015).**  
  *Dynamic Documents with R and knitr.* Chapman and Hall/CRC.  
  [https://CRAN.R-project.org/package=knitr](https://CRAN.R-project.org/package=knitr)

- **Xie, Y., Allaire, J. J., & Grolemund, G. (2018).**  
  *R Markdown: The Definitive Guide.* Chapman and Hall/CRC.  
  [https://CRAN.R-project.org/package=rmarkdown](https://CRAN.R-project.org/package=rmarkdown)
