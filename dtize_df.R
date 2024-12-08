library(mice)

dtize_df <- function(data, 
                     splits = "median", 
                     labels = c("low", "high"), 
                     right = TRUE, 
                     infinity = TRUE, 
                     lowest = TRUE, 
                     na_fill = "none") {
  
  if (!is.data.frame(data)) {
    stop("`data` must be a dataframe.")
  }
  
  data<-impute_pmm(na_fill, data)
  
  discretized_data <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
  
  for (col_name in names(data)) {
    column <- data[[col_name]]
    
    if (is.numeric(column)) {
      discretized_column <- dtize_col(column, 
                                      splits = splits, 
                                      labels = labels, 
                                      right = right, 
                                      infinity = infinity, 
                                      lowest = lowest, 
                                      na_fill = na_fill)
      discretized_data[[col_name]] <- discretized_column
    } else {
      discretized_data[[col_name]] <- column
    }
  }
  
  return(discretized_data)
}


impute_pmm <- function(na_fill, df) {
  # Check if na_fill is a character string
  if (!is.character(na_fill)) 
    return(df)
  
  na_fill <- tolower(na_fill)
  
  if (!na_fill %in% c("none", "mean", "median", "pmm")) {
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
  }
  
  if (na_fill != "pmm") {
    return(df)
  }
  
  numeric_cols <- sapply(df, is.numeric)
  
  if (!any(numeric_cols)) {
    warning("No numeric columns found for PMM imputation. Returning original dataframe.")
    return(df)
  }
  
  df_numeric <- df[, numeric_cols, drop = FALSE]
  
  if (!any(is.na(df_numeric))) {
    message("No missing values in numeric columns. Returning original dataframe.")
    return(df)
  }
  
  imp <- mice(df_numeric, method = "pmm", m = 1, maxit = 5, printFlag = FALSE)
  df_numeric_imputed <- complete(imp)
  
  df[, numeric_cols] <- df_numeric_imputed
  
  return(df)
}
