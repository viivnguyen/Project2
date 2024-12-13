test_that("makenumeric handles various scenarios correctly", {
  # Example dataset
  data <- data.frame(
    spouse = c("Living with spouse", "Not living with spouse", "Living with spouse", NA)
  )

  # Test basic conversion with true and false values
  result <- makenumeric(data, column = "spouse", true_values = c("Living with spouse"), true_number = 1, false_value = 0)
  expect_equal(result$spouse, c(1, 0, 1, NA_real_))

  # Test NA replacement
  result_na <- makenumeric(data, column = "spouse", true_values = c("Living with spouse"), true_number = 1, false_value = 0, na_value = -1)
  expect_equal(result_na$spouse, c(1, 0, 1, -1))

  # Test missing column error
  expect_error(makenumeric(data, column = "nonexistent"), "Column not found in the dataset.")

  # Test invalid column type error
  expect_error(makenumeric(data.frame(spouse = 1:3), column = "spouse"), "The specified column must be character or factor.")

  # Test with no true_values provided
  data_no_true <- data.frame(
    spouse = c("Yes", "No", "No", NA)
  )
  result_no_true <- makenumeric(data_no_true, column = "spouse", false_value = 0, na_value = -1)
  expect_equal(result_no_true$spouse, c(NA_real_, 0, 0, -1))

  # Test with edge cases in true_values
  data_edge <- data.frame(
    status = c("TRUE", "true", "FALSE", "True", NA)
  )
  result_edge <- makenumeric(data_edge, column = "status", true_values = c("TRUE", "true"), true_number = 1, false_value = 0, na_value = -1)
  expect_equal(result_edge$status, c(1, 1, 0, NA_real_, -1))
})
