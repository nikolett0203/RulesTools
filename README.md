# Association Rule Mining of eDNA Datasets

This project applies association rule mining to eDNA datasets in order to determine which
environmental metadata variables contribute most strongly to high eDNA concentrations in 
aquatic samples. 

## Usage

### plotting_funs.R

This file contains helper functions to create formatted plots of eDNA data prior to rule mining.
It includes functions for barplots, histograms, and scatterplots.

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

### `scatter` Function

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
scatter_plot <- scatter(df, 
                        "Temperature", 
                        "eDNA_Concentration", 
                        "Water Temperature", 
                        "eDNA Concentration")
print(scatter_plot)