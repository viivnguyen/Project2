#' Scatter Plot Function
#'
#' This function generates a scatter plot using ggplot2, with customizable aesthetics and validation of input data types.
#' It checks that the x and y variables are numeric and that the color variable is a factor.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x The name of the variable to be used on the x-axis (should be numeric).
#' @param y The name of the variable to be used on the y-axis (should be numeric).
#' @param color The name of the categorical variable to be used for color (should be a factor).
#' @param ... Additional arguments to be passed to `ggplot2::geom_point()`.
#'
#' @return A `ggplot` object representing the scatter plot.
#' @export
#'
#' @examples
#' # Example usage of the scatter function
#' data <- data.frame(x = rnorm(100), y = rnorm(100), color = factor(sample(c("A", "B"), 100, replace = TRUE)))
#' scatter(data, x = "x", y = "y", color = "color")
#'
#' @import ggplot2
#' @import dplyr
#'

# Function
scatter <- function(data, x, y, color, ...) {

  # Ensure x and y are numeric using base R
  stopifnot(base::is.numeric(data[[x]]))   # Check if x is numeric
  stopifnot(base::is.numeric(data[[y]]))   # Check if y is numeric

  # Convert color to a factor if it is not already a factor
  if (!base::is.factor(data[[color]])) {
    data[[color]] <- base::factor(data[[color]])  # Convert to factor
  }

  stopifnot(base::is.factor(data[[color]])) # Check if color is a factor

  # Check if the data has the expected number of rows
  if (nrow(data) == 0) {
    stop("The data contains no rows.")
  }

  # Plot the scatter plot
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color]]) +  # Correctly map variables to aesthetics
    ggplot2::geom_point(...)
}

