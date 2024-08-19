# Association Rule Mining of eDNA Datasets

This project applies association rule mining to eDNA datasets in order to determine which environmental metadata variables contribute most strongly to high eDNA concentrations in aquatic samples. 

## Usage

### plotting_funs.R

This file contains helper functions to create formatted plots of eDNA data prior to rule mining. It includes functions for barplots, histograms, and scatterplots.

#### `bar` Function

- **Purpose:** Generates a styled barplot for categorical data.
- **Parameters:**
  - `data`: The dataframe containing the column to be plotted.
  - `xvar`: The column name for the data you want to plot on the x-axis.
  - `xlab`: The label for the x-axis of the plot.
- **Example Usage:**
  ```r
  # Assuming 'df' is your dataframe and 'Site' is a column of categorical data in the dataframe:
  sites_plot <- bar(df, Site, "Site")
  print(sites_plot)

#### `his` Function

- **Purpose:** Generates a styled historgram for continuous data.
- **Parameters:**
  - `data`: The dataframe containing the column to be plotted.
  - `xvar`: The column name for the data you want to plot on the x-axis.
  - `xlab`: The label for the x-axis of the plot.
- **Example Usage:**
  ```r
  # Assuming 'df' is your dataframe and 'pH' is a column name in the dataframe:
  ph_plot <- his(df, pH, "pH")
  print(ph_plot)

#### `scatter` Function

- **Purpose:** Generates a styled scatterplot to visualize the relationship between two continuous variables, and annotates the plot with the Pearson correlation coefficient and p-value.
- **Parameters:**
  - `data`: The dataframe containing the columns to be plotted.
  - `xV`: The column name for the data you want to plot on the x-axis.
  - `yV`: The column name for the data you want to plot on the y-axis.
  - `labx`: The label (as a string) for the x-axis of the plot.
  - `laby`: The label (as a string) for the y-axis of the plot.
- **Example Usage:**
  ```r
  # Assuming 'df' is your dataframe, 'Temperature' is plotted on the x-axis, and 'eDNA_Concentration' on the y-axis:
  scatter_plot <- scatter(df, "Temperature", "eDNA_Concentration", "Water Temperature", "eDNA Concentration")
  print(scatter_plot)

### assoc_funs.R

This file contains helper functions to facilitate the discretization of data and the comparison of association rule sets. It includes functions for discretizing continuous variables, comparing two or more rulesets, and extracting rule labels for visualization.

#### `dtize` Function

- **Purpose:** Discretizes continuous data in a dataframe based on provided split thresholds.
- **Parameters:**
  - `data`: The dataframe containing the columns to be discretized.
  - `split`: A datafram containing the split thresholds for each column in the `data` dataframe. The splits must correspond to the order of columns in the dataframe.
  - `new_df`: An empty dataframe initialized with the number of columns needed to store the discretized data.
- **Example Usage:**
  ```r
  # Assuming 'df' is your original dataframe:
  ew_df <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  splits <- data.frame(AirTemp = 16, WaterTemp = 25, pH = 7.75, eDNAConc = 13.3)
  discretized_df <- dtize(df, splits, new_df)

#### `rule_by_rule` Function

- **Purpose:** Compares an indefinite number of rulesets by finding common rules and displaying their interestingness measures, such as support, confidence, and lift.
- **Parameters:**
  - `...`: An indefinite number of `rules` objects (each must be a named argument).
- **Example Usage:**
  ```r
  # Assuming 'rules1', 'rules2', and 'rules3' are three rules objects you want to compare:
  comparison_df <- rule_by_rule(R1 = rules1, R2 = rules2, R3 = rules3)
  print(comparison_df)