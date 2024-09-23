# helper function to generate styled barplots for categorical data
# @param data: dataframe containing column to be plotted
# @param xvar: column name for plotting data
# @param xlab: label for x-axis of plot
bar <- function (data, xvar, xlab){
  plot <- data %>% ggplot(aes(x={{xvar}})) +
    geom_bar(fill = "cornflowerblue", colour = "black") +
    labs(x = xlab,
         y = "Count")
  return(plot)
}


# helper function to generate styled histograms for continuous data
# @param data: dataframe containing column to be plotted
# @param xvar: column name for plotting data
# @param xlab: label for x-axis of plot
his <- function(data, xvar, xlab){
  plot <- data %>% ggplot(aes(x={{xvar}})) +
    geom_histogram(fill="cornflowerblue", colour = "black") +
    labs(x = xlab,
         y = "Frequency")
  return(plot)
}

# helper function to generate styled scatterplots on a single grid (pearson correlation)
# @param data: dataframe containing column to be plotted
# @param xV: column name for x-axis plotting data
# @param yV: column name for y-axis plotting data
# @param labx: x-axis label
# @param laby: y-axis label
scatter <- function(data, xV, yV, labx, laby, method){
  
  corr_test <- cor.test(data[[xV]], data[[yV]], method = method)
  corr_coeff <- round(corr_test$estimate, 2)
  p_value <- round(corr_test$p.value, 3)
  
  ggplot(data, aes_string(x = xV, y= yV)) +
    geom_point(color = "#ebc349") +  # Set the points to white
    labs(x = labx,
         y = laby) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#4cbca6", color = NA),  # Background color
      panel.background = element_rect(fill = "#4cbca6", color = NA), # Panel background
      axis.title = element_text(color = "white"),  # Axis title color
      axis.text = element_text(color = "white"),   # Axis text color
      axis.line = element_line(color = "#14303f"),   # Axis line color
      axis.ticks = element_line(color = "white"),  # Axis ticks color
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    annotate("text", x = Inf, y = Inf, label = paste0("r = ", corr_coeff, "\np = ", p_value),
             hjust = 1.1, vjust = 1.1, color = "#000000", size = 4)
}
