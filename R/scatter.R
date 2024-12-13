#' Scatter Plot Function for Trial Data Analysis
#'
#' This function generates a customizable scatter plot specifically designed for analyzing trial data,
#' with features for tracking measurements over time and highlighting important thresholds.
#'
#' @param data A data frame containing the trial data.
#' @param x The name of the variable to be used on the x-axis (should be numeric).
#' @param y The name of the variable to be used on the y-axis (should be numeric).
#' @param color The name of the categorical variable to be used for color (should be a factor).
#' @param title Plot title (optional).
#' @param xlab X-axis label (optional).
#' @param ylab Y-axis label (optional).
#' @param point_size Size of points (default = 2).
#' @param point_alpha Point transparency (default = 0.6).
#' @param add_trendline Logical, whether to add trend lines (default = FALSE).
#' @param add_threshold Logical, whether to add threshold line (default = FALSE).
#' @param threshold_value Numeric value for threshold line (default = NULL).
#' @param threshold_label Character string for threshold label (default = "Threshold").
#' @param highlight_outliers Logical, whether to highlight potential outliers (default = FALSE).
#' @param xlim Vector of length 2 for x-axis limits.
#' @param ylim Vector of length 2 for y-axis limits.
#' @param theme_style Character, either "light" or "dark" (default = "light").
#' @param facet_by Character, name of grouping variable for faceting (optional).
#' @param ... Additional arguments to be passed to `ggplot2::geom_point()`.
#'
#' @return A `ggplot` object representing the scatter plot.
#' @export
#'
#' @examples
#' # Basic usage with trial data
#' trial_data <- data.frame(
#'   day = rep(1:10, each = 20),
#'   weight = rnorm(200, mean = 25, sd = 2),
#'   group = factor(rep(c("Vaccine", "Placebo"), each = 100)),
#'   mouse_id = factor(rep(1:20, 10))
#' )
#' scatter(trial_data, 
#'         x = "day", 
#'         y = "weight", 
#'         color = "group", 
#'         add_threshold = TRUE,
#'         threshold_value = 20,
#'         threshold_label = "20% Weight Loss Threshold",
#'         facet_by = "mouse_id")
#'
#' @import ggplot2
#' @import dplyr

scatter <- function(data, x, y, color, title = NULL, xlab = NULL, ylab = NULL,
                   point_size = 2, point_alpha = 0.6, add_trendline = FALSE,
                   add_threshold = FALSE, threshold_value = NULL, 
                   threshold_label = "Threshold", highlight_outliers = FALSE,
                   xlim = NULL, ylim = NULL, theme_style = "light", 
                   facet_by = NULL, ...) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  # Check for missing values
  if (any(is.na(data[[x]])) || any(is.na(data[[y]]))) {
    warning("Data contains missing values which will be removed")
    data <- data[complete.cases(data[c(x, y)]), ]
  }
  
  # Ensure x and y are numeric
  stopifnot(base::is.numeric(data[[x]]))
  stopifnot(base::is.numeric(data[[y]]))
  
  # Convert color to factor
  if (!base::is.factor(data[[color]])) {
    data[[color]] <- base::factor(data[[color]])
  }
  
  # Create base plot
  p <- ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color]]) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha, ...)
  
  # Add trend line if requested
  if (add_trendline) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.2)
  }
  
  # Add threshold line if requested
  if (add_threshold && !is.null(threshold_value)) {
    p <- p + 
      ggplot2::geom_hline(yintercept = threshold_value, 
                         linetype = "dashed", 
                         color = "red") +
      ggplot2::annotate("text", 
                       x = min(data[[x]]), 
                       y = threshold_value,
                       label = threshold_label, 
                       vjust = -0.5, 
                       color = "red")
  }
  
  # Highlight outliers if requested
  if (highlight_outliers) {
    outliers <- boxplot(data[[y]], plot = FALSE)$out
    if (length(outliers) > 0) {
      data$is_outlier <- data[[y]] %in% outliers
      p <- p + 
        ggplot2::geom_point(data = subset(data, is_outlier), 
                           shape = 21, 
                           size = 4, 
                           color = "black")
    }
  }
  
  # Add faceting if requested
  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)))
  }
  
  # Add labels
  p <- p +
    ggplot2::labs(
      title = title,
      x = ifelse(is.null(xlab), x, xlab),
      y = ifelse(is.null(ylab), y, ylab),
      color = color
    )
  
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
  
  # Set axis limits if provided
  if (!is.null(xlim)) p <- p + ggplot2::xlim(xlim)
  if (!is.null(ylim)) p <- p + ggplot2::ylim(ylim)
  
  return(p)
}

