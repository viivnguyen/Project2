test_that("surveylevels handles various scenarios correctly", {
  # Example dataset
  data <- data.frame(
    proficiency = c("Well", "Not at all", NA, "Not well", "Very well")
  )
  response_scale <- c("Not at all", "Not well", "Well", "Very well")

  # Test basic level conversion
  result <- surveylevels(data, column = "proficiency", ordered_levels = response_scale)
  expect_equal(result$proficiency, c(3, 1, NA_real_, 2, 4))

  # Test missing column error
  expect_error(surveylevels(data, column = "nonexistent", ordered_levels = response_scale), "Column .* not found in the dataframe.")

  # Test unmatched levels error
  data_unmatched <- data.frame(proficiency = c("Well", "Unknown"))
  expect_error(surveylevels(data_unmatched, column = "proficiency", ordered_levels = response_scale), "Some values in the column do not match the provided ordered levels.")

  # Test with all levels present
  data_complete <- data.frame(
    proficiency = c("Not at all", "Not well", "Well", "Very well", "Very well")
  )
  result_complete <- surveylevels(data_complete, column = "proficiency", ordered_levels = response_scale)
  expect_equal(result_complete$proficiency, c(1, 2, 3, 4, 4))

  # Test empty column
  data_empty <- data.frame(proficiency = character(0))
  result_empty <- surveylevels(data_empty, column = "proficiency", ordered_levels = response_scale)
  expect_equal(result_empty$proficiency, numeric(0))

  # Test NA handling
  data_na <- data.frame(proficiency = c(NA, NA, "Very well"))
  result_na <- surveylevels(data_na, column = "proficiency", ordered_levels = response_scale)
  expect_equal(result_na$proficiency, c(NA_real_, NA_real_, 4))
})
