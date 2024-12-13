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
proptable <- function(data, x, y) {
  data %>%
    count({{ x }}, {{ y }}) %>%
    group_by({{ x }}) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup()
}
