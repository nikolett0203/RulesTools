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
  - `xvar`: The column name (as a string) for the data you want to plot on the x-axis.
  - `xlab`: The label for the x-axis of the plot.
- **Example Usage:**
  ```r
  # Assuming 'df' is your dataframe and 'Species' is a categorical variable:
  bar_plot <- bar(df, Species, "Species Type")
  print(bar_plot)