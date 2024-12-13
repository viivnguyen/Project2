#' Convert Specific Character Values to Numeric in a Dataset
#'
#' This function converts specified character values in a given column of a dataset to numeric values, while allowing
#' the user to define what the numeric values for "true", "false", and "NA" should be.
#'
#' @param data A data frame or tibble containing the dataset. This dataset should have the column you want to convert.
#' @param column A character string specifying the column name to be converted. The column should be of class `character` or `factor`.
#' @param true_values A vector of character values that should be converted to `true_number`. These are the "true" categories.
#' @param true_number The numeric value to replace the `true_values`. Default is `1`.
#' @param false_value The numeric value to replace values that are not in `true_values`. Default is `NULL`, meaning no change.
#' @param na_value The numeric value to replace `NA` values. Default is `NA`.
#'
#' @return A data frame or tibble with the specified column converted to numeric values. The modified column is updated with numeric values based on the provided logic.
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


makenumeric <- function(data, column, true_values, true_number = 1, false_value = NULL, na_value = NA) {
  # Check if the column exists
  if (!column %in% colnames(data)) {
    stop("Column not found in the dataset.")
  }

  # Extract the column as character
  variable <- as.character(data[[column]])

  # Replace true values with true_number
  variable[variable %in% true_values] <- true_number

  # Convert remaining values to numeric
  numeric_variable <- suppressWarnings(as.numeric(variable))

  # Handle NAs
  numeric_variable[is.na(variable)] <- na_value

  # Apply false_value only if provided
  if (!is.null(false_value)) {
    numeric_variable[is.na(numeric_variable)] <- false_value
  }

  # Update the dataset
  data[[column]] <- numeric_variable
  return(data)
}




