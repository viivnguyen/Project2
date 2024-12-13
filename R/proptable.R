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
#' @return A data frame containing the summary statistics (counts and/or proportions)
#' @export
#'
#' @examples
#' # Example 1: Mouse trial data - Treatment outcomes
#' proptable(mouse_birth,
#'          x = Treatment,
#'          y = Sex,
#'          include_total = TRUE)
#'
#' # Example 2: Survey data - Education by English proficiency
#' proptable(asian_american,
#'          x = education,
#'          y = english_speaking,
#'          digits = 3)
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
  if (!base::is.data.frame(data)) {
    base::stop("Input 'data' must be a data frame")
  }

  if (!type %in% c("proportion", "count")) {
    base::stop("'type' must be either 'proportion' or 'count'")
  }

  if (digits < 0) {
    base::stop("'digits' must be a non-negative number")
  }

  # Calculate base counts
  result <- dplyr::count(data, {{ x }}, {{ y }}) %>%
    dplyr::group_by({{ x }}) %>%
    dplyr::mutate(prop = base::round(n/base::sum(n), digits)) %>%
    dplyr::ungroup()

  # Add totals if requested
  if (include_total) {
    result <- result %>%
      dplyr::group_by({{ x }}) %>%
      dplyr::mutate(total = base::sum(n)) %>%
      dplyr::ungroup()
  }

  # Return either proportions or counts
  if (type == "proportion") {
    result <- dplyr::select(result, -n)
  } else {
    result <- dplyr::select(result, -prop)
  }

  return(result)
}
