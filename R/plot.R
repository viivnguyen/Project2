#' Title
#'
#' @param data
#' @param x
#'
#' @return
#' @export
#'
#' @examples

# Function
scatter <- function(data, x, y, color, alpha, ...){
  #Determine if x is numeric
  x_class <- data %>%
    pull({{ x }}) %>%
    is.numeric()
  stopifnot(x_class)
  y_class <- data %>%
    pull({{ x }}) %>%
    is.numeric()
  stopifnot(y_class)
  color_class <- data %>%
    pull({{ color }}) %>%
    is.factor()
  stopifnot(color_class)
  stopifnot(alpha >= 0 & alpha <= 1)

  ggplot(data = data, mapping = aes(x = {{ x }}, y = {{ y }}, color = {{ color }})) +
    geom_point(alpha = alpha)
}
