#' Title
#'
#' @param data
#' @param x
#'
#' @return
#' @export
#'
#' @examples

surveylevels3 <- function(data, column_name, ordered_levels) {
  # Check if the column exists in the data
  if (!column_name %in% colnames(data)) {
    stop(paste("Column", column_name, "not found in the dataframe."))
  }

  # Drop rows with NA values in the specified column
  data <- data[!is.na(data[[column_name]]), ]

  # Extract the column
  column_values <- data[[column_name]]

  # Validate that all unique values in the column match the provided levels
  unique_values <- unique(column_values)
  if (!all(unique_values %in% ordered_levels)) {
    stop("Some values in the column do not match the provided ordered levels.")
  }

  # Map the levels to numeric values based on the provided order
  level_map <- setNames(seq_along(ordered_levels), ordered_levels)

  # Replace values in the column with their numeric equivalents
  data[[column_name]] <- as.numeric(factor(column_values, levels = ordered_levels, labels = seq_along(ordered_levels)))

  # Replace the column in the dataframe with the numeric version
  #data[[column_name]] <- numeric_column

  # Print the mapping for transparency
  cat("Level mapping:\n")
  print(level_map)

  invisible(NULL)
}

# Example usage:
# Assuming the column "English Speaking" has values like "Not at all", "Not well", "Very well", etc.
# data <- convert_to_numeric_levels(data, "English Speaking",
#                                   c("Not at all", "Not well", "Very well", "Well"))
