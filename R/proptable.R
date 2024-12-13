#' Create Proportion Tables
#'
#' This function creates tables of proportions or counts for categorical variables.
#' It can handle both survey data and experimental data.
#'
#' @param data A data frame containing the variables
#' @param x The grouping variable (e.g., "Treatment" or "education")
#' @param y The variable to calculate proportions for (e.g., "Status" or "english_speaking")
#' @param type Type of summary ("proportion" or "count", default = "proportion")
#' @param digits Number of decimal places for proportions (default = 2)
#' @param include_total Logical, whether to include row totals (default = FALSE)
#'
#' @return A df containing the summary statistics (counts and/or proportions)
#' @export
#'
#' @examples
#' # Example 1: Mouse trial data - Treatment outcomes
#' proptable(mouse_birth, 
#'          x = Treatment, 
#'          y = Sex)
#'
#' # Example 2: Survey data - Education by English proficiency
#' proptable(asian_american,
#'          x = education,
#'          y = english_speaking,
#'          include_total = TRUE)
#'
#' # Example 3: Get counts instead of proportions
#' proptable(asian_american,
#'          x = marital_status,
#'          y = english_difficulties,
#'          type = "count")
#'
#' @importFrom dplyr count group_by mutate ungroup select
#' @importFrom rlang enquo
#' @importFrom tidyr pivot_wider

proptable <- function(data, x, y, 
                     type = "proportion",
                     digits = 2,
                     include_total = FALSE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (!type %in% c("proportion", "count")) {
    stop("'type' must be either 'proportion' or 'count'")
  }
  
  # calculate base counts
  result <- data %>%
    count({{ x }}, {{ y }}) %>%
    group_by({{ x }}) %>%
    mutate(prop = round(n/sum(n), digits)) %>%
    ungroup()
  
  # add totals if requested
  if (include_total) {
    result <- result %>%
      group_by({{ x }}) %>%
      mutate(total = sum(n)) %>%
      ungroup()
  }
  
  # return either proportions or counts
  if (type == "proportion") {
    result <- result %>%
      select(-n)
  } else {
    result <- result %>%
      select(-prop)
  }
  
  return(result)
}
