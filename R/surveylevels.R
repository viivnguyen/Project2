#' Converts Survey Character Levels to Numeric
#'
#' This function converts specified character levels in a given column of a data frame to numeric values.
#' The conversion is based on provided order of levels, which ensures that the numeric values
#' correspond to the intended ranking or hierarchy. The function replaces the original column with the
#' numeric equivalents while validating that all levels in the column are part of the provided order.
#'
#' @param data A data frame containing the column to be converted.
#' @param column A string specifying the name of the column to convert. The column should contain
#'        character values that match the provided ordered levels.
#' @param ordered_levels A character vector containing the desired order of levels. The first level in the
#'        vector will be assigned the value 1, the second level will be assigned the value 2, and so on.
#'
#' @return The modified data frame with the specified column replaced by its numeric equivalents.
#' @export
#'
#' @examples
#' data <- data.frame(proficiency = c("Well", "Not at all", NA, "Not well", "Very well"))
#' response_scale <- c("Not at all", "Not well", "Well", "Very well")
#' data <- surveylevels(data, column = "proficiency", ordered_levels = response_scale)
#' print(data)

surveylevels <- function(data, column, ordered_levels) {

  # Check if the column exists in the data
  if (!base::column %in% base::colnames(data)) {
    base::stop(base::paste("Column", column, "not found in the dataframe."))
  }

  # Extract the column
  col_values <- data[[column]]

  # Check if all unique values in the column match the provided levels, excluding NA
  unique_values <- base::unique(stats::na.omit(col_values))
  if (!base::all(unique_values %in% ordered_levels)) {
    base::stop("Some values in the column do not match the provided ordered levels.")
  }

  # Convert the column to numeric without altering row order
  data[[column]] <- base::as.numeric(base::factor(col_values, levels = ordered_levels))

  # Print the mapping for transparency
  base::cat("Level mapping:\n")
  base::print(stats::setNames(base::seq_along(ordered_levels), ordered_levels))

  return(data)
}
