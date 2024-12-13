#' Convert Column to Numeric with Optional Custom Mapping
#'
#' This function converts a specified column in a dataset to numeric, with optional mappings for "true," "false," and `NA` values.
#'
#' @param data A data frame or tibble containing the dataset.
#' @param column A character string specifying the column name to be converted.
#' @param true_values A vector of character values to be replaced with `true_number`. Default is `NULL`.
#' @param true_number The numeric value corresponding to `true_values`. Default is `1`.
#' @param false_value The numeric value for values not in `true_values`. Default is `NULL` (no change).
#' @param na_value The numeric value to replace `NA` values. Default is `NA`.
#'
#' @return A data frame with the specified column converted to numeric.
#' @export
#'
#' @examples
#' data <- data.frame(spouse = c("Living with spouse", "Not living with spouse", "Living with spouse"))
#' data <- makenumeric(data, column = "spouse", true_values = "Living with spouse", true_number = 1, false_value = 0)
#' print(data)
#'
#' data2 <- data.frame(spouse = c("Living with spouse", NA, "Not living with spouse"))
#' data2 <- makenumeric(data2, column = "spouse", true_values = "Living with spouse", true_number = 1, false_value = 0, na_value = -1)
#' print(data2)

makenumeric <- function(data, column, true_values = NULL, true_number = 1, false_value = NULL, na_value = NA) {
  # Check if the column exists
  if (!column %in% colnames(data)) {
    stop("Column not found in the dataset.")
  }

  # Extract the column as a character vector
  variable <- as.character(data[[column]])

  # Apply true value conversion if specified
  if (!is.null(true_values)) {
    variable[variable %in% true_values] <- true_number
  }

  # Convert to numeric
  numeric_variable <- suppressWarnings(as.numeric(variable))

  # Handle NA replacements
  if (!is.na(na_value)) {
    numeric_variable[is.na(variable)] <- na_value
  }

  # Apply false value replacement if specified
  if (!is.null(false_value)) {
    numeric_variable[is.na(numeric_variable)] <- false_value
  }

  # Update and return the dataset
  data[[column]] <- numeric_variable
  return(data)
}
