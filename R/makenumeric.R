#' Convert Column to Numeric with Optional Custom Mapping
#'
#' This function converts a specified column in a dataset to numeric values using custom mappings.
#' It is particularly useful for converting categorical or ordinal data to numeric format, with
#' flexible handling of true/false conditions and missing values.
#'
#' @param data A data frame containing the dataset
#' @param column A character string specifying the column name to be converted
#' @param true_values A character vector of values to be mapped to `true_number` (default: NULL)
#' @param true_number The numeric value for `true_values` (default: 1)
#' @param false_value The numeric value for non-true values (default: NULL)
#' @param na_value The numeric value for NA values (default: NA_real_)
#'
#' @return A data frame with the specified column converted to numeric values
#' @export
#'
#' @examples
#' # Example 1: Basic true/false conversion
#' data <- data.frame(
#'   response = c("Yes", "No", "Yes", NA)
#' )
#' makenumeric(data, "response", 
#'             true_values = "Yes",
#'             false_value = 0)
#'
#' # Example 2: Multiple true values
#' survey <- data.frame(
#'   satisfaction = c("Very Satisfied", "Satisfied", "Neutral", NA)
#' )
#' makenumeric(survey, "satisfaction",
#'             true_values = c("Very Satisfied", "Satisfied"),
#'             true_number = 1,
#'             false_value = 0,
#'             na_value = -99)
#'
#' # Example 3: Converting living situation
#' household <- data.frame(
#'   spouse = c("Living with spouse", "Not living with spouse", NA)
#' )
#' makenumeric(household, "spouse",
#'             true_values = "Living with spouse",
#'             false_value = 0,
#'             na_value = -1)

makenumeric <- function(data, column, true_values = NULL, true_number = 1,
                       false_value = NULL, na_value = NA_real_) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  
  if (!column %in% colnames(data)) {
    stop("Column '", column, "' not found in the dataset.")
  }
  
  if (!is.character(data[[column]]) && !is.factor(data[[column]])) {
    stop("Column '", column, "' must be character or factor.")
  }
  
  if (!is.null(true_values) && !is.character(true_values)) {
    stop("'true_values' must be a character vector or NULL.")
  }
  
  # Extract and convert column to character
  variable <- as.character(data[[column]])
  
  # Create numeric vector for results
  numeric_variable <- rep(NA_real_, length(variable))
  
  # Apply conversions in specific order
  if (!is.null(true_values)) {
    # Convert true values first
    numeric_variable[variable %in% true_values] <- true_number
    
    # Apply false value to non-NA, non-true values if specified
    if (!is.null(false_value)) {
      numeric_variable[!is.na(variable) & !(variable %in% true_values)] <- false_value
    }
  } else {
    # If no true_values specified, attempt direct numeric conversion
    numeric_variable <- suppressWarnings(as.numeric(variable))
  }
  
  # Handle NA values last
  if (!is.na(na_value)) {
    numeric_variable[is.na(variable)] <- na_value
  }
  
  # Update the column and return
  data[[column]] <- numeric_variable
  return(data)
}
