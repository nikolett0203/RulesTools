library(mice)

dtize_df <- function(data,
                     splits = "median",
                     labels = c("low", "high"),
                     right = TRUE,
                     infinity = TRUE,
                     lowest = TRUE,
                     na_fill = "none",
                     m = 5,
                     maxit = 5,
                     seed = NULL,
                     printFlag = FALSE) {
  
  if (!is.data.frame(data))
    stop("`data` must be a dataframe.")
  
  data <- impute_pmm(na_fill, data, m, maxit, seed, printFlag)
  
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


impute_pmm <- function(na_fill, df, m = 5, maxit = 5, seed = NULL, printFlag = FALSE) {
  
  if (!is.character(na_fill) || length(na_fill) != 1)
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
  
  na_fill <- tolower(na_fill)
  
  if (!na_fill %in% c("none", "mean", "median", "pmm"))
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
  
  if (na_fill != "pmm")
    return(df)
  
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m < 1 || !is.finite(m) || m %% 1 != 0)
    stop("`m` must be a single positive integer.")
  
  if (!is.numeric(maxit) || length(maxit) != 1 || is.na(maxit) || maxit < 0 || !is.finite(maxit) || maxit %% 1 != 0)
    stop("`maxit` must be a single non-negative integer.")
  
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed) || !is.finite(seed) || seed %% 1 != 0))
    stop("`seed` must be NULL or a single integer.")
  
  if (!is.logical(printFlag) || length(printFlag) != 1 || is.na(printFlag))
    stop("`printFlag` must be a single logical value (TRUE or FALSE).")
  
  numeric_cols <- sapply(df, is.numeric)
  
  if (!any(numeric_cols)) {
    warning("No numeric columns found for PMM imputation.")
    return(df)
  }
  
  df_numeric <- df[, numeric_cols, drop = FALSE]
  
  if (!any(is.na(df_numeric))) {
    message("No missing values in numeric columns.")
    return(df)
  }
  
  if (!is.null(seed))
    set.seed(seed)
  
  imp <- mice(df_numeric, method = "pmm", m = m, maxit = maxit, printFlag = printFlag)
  df_numeric_imputed <- complete(imp)
  
  df[, numeric_cols] <- df_numeric_imputed
  
  return(df)
  
}
