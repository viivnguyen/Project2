#' Title
#'
#' @param data
#' @param x
#'
#' @return
#' @export
#'
#' @examples

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




