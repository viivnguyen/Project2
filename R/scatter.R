#' Scatter Plot Function
#'
#' This function generates customizable scatter plots for various types of data,
#' including mouse trial data and survey data.
#'
#' @param data A data frame containing the measurements
#' @param x The name of the x-axis variable
#' @param y The name of the y-axis variable
#' @param color The name of the grouping variable for color
#' @param title Plot title (optional)
#' @param xlab X-axis label (optional)
#' @param ylab Y-axis label (optional)
#' @param point_size Size of points (default = 2)
#' @param point_alpha Point transparency (default = 0.6)
#' @param add_trendline Logical, whether to add trend lines (default = FALSE)
#' @param add_threshold Logical, whether to add threshold line (default = FALSE)
#' @param threshold_value Numeric value for threshold line
#' @param threshold_label Character string for threshold label
#' @param facet_by Character, name of grouping variable for faceting
#' @param theme_style Character, either "light" or "dark" (default = "light")
#' @param ... Additional arguments to be passed to `ggplot2::geom_point()`
#'
#' @return A `ggplot` object representing the scatter plot
#' @export
#'
#' @import ggplot2

scatter <- function(data, x, y, color = NULL, title = NULL, xlab = NULL, ylab = NULL,
                    point_size = 2, point_alpha = 0.6, add_trendline = FALSE,
                    add_threshold = FALSE, threshold_value = NULL,
                    threshold_label = "Threshold",
                    facet_by = NULL, theme_style = "light", ...) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  # Check for missing values
  if (any(is.na(data[[x]])) || any(is.na(data[[y]]))) {
    warning("Data contains missing values which will be removed")
    data <- data[complete.cases(data[c(x, y)]), ]
  }

  # Check if columns exist
  if (!x %in% names(data)) {
    stop("Column '", x, "' not found in data")
  }
  if (!y %in% names(data)) {
    stop("Column '", y, "' not found in data")
  }

  # Create base plot
  mapping <- ggplot2::aes(
    x = .data[[x]],
    y = .data[[y]]
  )

  if (!is.null(color)) {
    mapping <- ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]],
      color = .data[[color]]
    )
  }

  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha, ...) +
    ggplot2::labs(
      title = title,
      x = ifelse(is.null(xlab), x, xlab),
      y = ifelse(is.null(ylab), y, ylab),
      color = color
    )

  # Add trend line if requested
  if (add_trendline) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.2)
  }

  # Add threshold line if requested
  if (add_threshold && !is.null(threshold_value)) {
    p <- p +
      ggplot2::geom_hline(yintercept = threshold_value, linetype = "dashed", color = "red") +
      ggplot2::annotate("text",
                        x = min(data[[x]], na.rm = TRUE),
                        y = threshold_value,
                        label = threshold_label,
                        vjust = -0.5,
                        color = "red")
  }

  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_by)))
  }

  # Apply theme
  if (theme_style == "dark") {
    p <- p + ggplot2::theme_dark()
  } else {
    p <- p + ggplot2::theme_light()
  }

  # Custom theme elements
  p <- p + ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.minor = ggplot2::element_line(color = "gray90"),
    strip.background = ggplot2::element_rect(fill = "gray95"),
    strip.text = ggplot2::element_text(face = "bold")
  )

  return(p)
}
