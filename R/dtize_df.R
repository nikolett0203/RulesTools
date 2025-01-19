#' Discretize Dataframe Columns
#'
#' Discretizes numeric columns of a dataframe based on specified splitting criteria,
#' and handles missing values using specified imputation methods.
#'
#' @param data A dataframe containing the data to be discretized.
#' @param cutoff A character string specifying the splitting method for numeric columns.
#'   Options are `"median"` (default) or a custom numeric vector of split points.
#' @param labels A character vector of labels for the discretized categories. Default is `c("low", "high")`.
#' @param include_right A logical value indicating if the intervals should be closed on the right. Default is `TRUE`.
#' @param infinity A logical value indicating if the split intervals should extend to infinity. Default is `TRUE`.
#' @param include_lowest A logical value indicating if the lowest value should be included in the first interval. Default is `TRUE`.
#' @param na_fill A character string specifying the imputation method for handling missing values.
#'   Options are `"none"` (default), `"mean"`, `"median"`, or `"pmm"` (predictive mean matching).
#' @param m An integer specifying the number of multiple imputations if `na_fill = "pmm"`. Default is `5`.
#' @param maxit An integer specifying the maximum number of iterations for the `mice` algorithm. Default is `5`.
#' @param seed An integer seed for reproducibility of the imputation process. Default is `NULL`.
#' @param printFlag A logical value indicating if `mice` should print logs during imputation. Default is `FALSE`.
#'
#' @return A dataframe with numeric columns discretized and missing values handled based on the specified imputation method.
#'
#' @importFrom mice mice complete
#'
#' @examples
#' data(BrookTrout)
#'
#' # Example with median as cutoff
#' med_df <- dtize_df(
#'   BrookTrout,
#'   cutoff="median",
#'   labels=c("below median", "above median")
#' )
#'
#' # Example with mean as cutoff
#' mean_df <- dtize_df(
#'   BrookTrout,
#'   cutoff="mean",
#'   include_right=FALSE
#' )
#'
#' # Example with missing value imputation
#' air <- dtize_df(
#'   airquality,
#'   cutoff="mean",
#'   na_fill="pmm",
#'   m=10,
#'   maxit=10,
#'   seed=42
#' )
#'
#'
#' @export

dtize_df <- function(data,
                     cutoff = "median",
                     labels = c("low", "high"),
                     include_right = TRUE,
                     infinity = TRUE,
                     include_lowest = TRUE,
                     na_fill = "none",
                     m = 5,
                     maxit = 5,
                     seed = NULL,
                     printFlag = FALSE) {

  if (!is.data.frame(data)) {
    stop("`data` must be a dataframe.")
  }

  data <- impute_pmm(
    na_fill  = na_fill,
    df       = data,
    m        = m,
    maxit    = maxit,
    seed     = seed,
    printFlag = printFlag
  )

  discretized_data <- data.frame(matrix(ncol = 0, nrow = nrow(data)))

  for (col_name in names(data)) {
    column <- data[[col_name]]

    if (is.numeric(column)) {
      discretized_column <- dtize_col(
        column,
        cutoff           = cutoff,
        labels           = labels,
        include_right    = include_right,
        infinity         = infinity,
        include_lowest   = include_lowest,
        na_fill          = na_fill
      )

      discretized_data[[col_name]] <- discretized_column
    } else {
      discretized_data[[col_name]] <- as.factor(column)
    }
  }

  return(discretized_data)
}


#' @noRd
#' @title Impute Missing Values Using Predictive Mean Matching
#' @description Imputes missing values in numeric columns of a dataframe using predictive mean matching (PMM).
#'   This function is used internally by `dtize_df` to handle missing data.
#'
#' @param na_fill A character string specifying the imputation method. Must be `"none"`, `"mean"`, `"median"`, or `"pmm"`.
#' @param df A dataframe with numeric columns to be imputed.
#' @param m An integer specifying the number of multiple imputations. Default is `5`.
#' @param maxit An integer specifying the maximum number of iterations for the `mice` algorithm. Default is `5`.
#' @param seed An integer seed for reproducibility. Default is `NULL`.
#' @param printFlag A logical value indicating if `mice` should print logs during imputation. Default is `FALSE`.
#'
#' @return A dataframe with missing values in numeric columns imputed using the specified method.
#' @examples

impute_pmm <- function(na_fill,
                       df,
                       m = 5,
                       maxit = 5,
                       seed = NULL,
                       printFlag = FALSE) {

  if (!is.character(na_fill) || length(na_fill) != 1) {
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
  }

  na_fill <- tolower(na_fill)

  if (!na_fill %in% c("none", "mean", "median", "pmm")) {
    stop("Invalid imputation method. `na_fill` must be 'none', 'mean', 'median', or 'pmm'.")
  }

  if (na_fill != "pmm") {
    return(df)
  }

  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m < 1 || !is.finite(m) || m %% 1 != 0) {
    stop("`m` must be a single positive integer.")
  }

  if (!is.numeric(maxit) || length(maxit) != 1 || is.na(maxit) || maxit < 0 || !is.finite(maxit) || maxit %% 1 != 0) {
    stop("`maxit` must be a single non-negative integer.")
  }

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed) || !is.finite(seed) || seed %% 1 != 0)) {
    stop("`seed` must be NULL or a single integer.")
  }

  if (!is.logical(printFlag) || length(printFlag) != 1 || is.na(printFlag)) {
    stop("`printFlag` must be a single logical value (TRUE or FALSE).")
  }

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

  if (!is.null(seed)) {
    set.seed(seed)
  }

  imp <- mice(
    df_numeric,
    method    = "pmm",
    m         = m,
    maxit     = maxit,
    printFlag = printFlag
  )

  df_numeric_imputed <- complete(imp)
  df[, numeric_cols] <- df_numeric_imputed

  return(df)
}


# enter numeric cols



validate_cuts <- function(cutoff, data) {

  # if cutoff is character but not "mean" or "median
  if (is.character(cutoff)){
    if (length(cutoff) != 1 || !tolower(cutoff) %in% c("median", "mean"))
      stop("`cutoff` must be either 'median', 'mean', or a named list of numeric vectors.")
    return(TRUE)
  }

  numeric_cols <- names(data)[sapply(data, is.numeric)]

  if (is.list(cutoff)) {
    # all elements in the list must be numeric vectors
    if (!all(sapply(cutoff, function(x) is.numeric(x) && is.vector(x))))
      stop("All elements of `cutoff` (if a list) must be numeric vectors.")

    # list must be named, matching columns
    if (is.null(names(cutoff)) || !all(names(cutoff) %in% numeric_cols))
      stop("`cutoff` (if a list) must have names that match all numeric columns in the dataframe.")

    # all columns need vectors
    if (!all(numeric_cols %in% names(cutoff)))
      stop("`cutoff` (if a list) must include entries for all numeric columns in the dataframe.")

    return(TRUE)
  }

  # If `cutoff` is neither a character string nor a list
  stop("`cutoff` must be either a character string ('median', 'mean') or a named list of numeric vectors.")
}
