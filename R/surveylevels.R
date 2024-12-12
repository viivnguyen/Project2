#' Title
#'
#' @param data
#' @param x
#'
#' @return
#' @export
#'
#' @examples

convert_to_ordered_numeric <- function(data, column, level_order) {
  # Check if the column exists
  if (!column %in% colnames(data)) {
    stop("Column not found in the dataset.")
  }

  # Ensure the column is a factor or character
  if (!is.character(data[[column]]) && !is.factor(data[[column]])) {
    stop("Column must be a factor or character type.")
  }

  # Determine the order of levels
  if (is.null(level_order)) {
    # If no custom order provided, use the natural order of unique levels
    level_order <- sort(unique(data[[column]]))
  }

  # Convert to factor with levels ordered as specified
  data[[column]] <- factor(data[[column]], levels = level_order, ordered = TRUE)

  # Convert the ordered factor to numeric
  data[[column]] <- as.numeric(data[[column]])

  # Return the modified dataset
  return(data)
}

