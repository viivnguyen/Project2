test_that("makenumeric handles various scenarios correctly", {
  # Example dataset
  data <- data.frame(
    spouse = c("Living with spouse", "Not living with spouse", "Living with spouse", NA),
    stringsAsFactors = FALSE
  )

  # Test basic conversion with true and false values
  result <- makenumeric(data, column = "spouse", true_values = c("Living with spouse"), true_number = 1, false_value = 0)
  expect_equal(result$spouse, c(1, 0, 1, 0)) # `NA` is replaced by `false_value` in the function

  # Test NA replacement
  result_na <- makenumeric(data, column = "spouse", true_values = c("Living with spouse"), true_number = 1, false_value = 0, na_value = -1)
  expect_equal(result_na$spouse, c(1, 0, 1, -1))

  # Test missing column error
  expect_error(makenumeric(data, column = "nonexistent"), "Column not found in the dataset.")

  # Test invalid column type error
  test_data_invalid <- data.frame(spouse = 1:3)
  expect_error(makenumeric(test_data_invalid, column = "spouse"), "The specified column must be character or factor")

  # Test with no true_values provided
  data_no_true <- data.frame(
    spouse = c("Yes", "No", "No", NA),
    stringsAsFactors = FALSE
  )
  result_no_true <- makenumeric(data_no_true, column = "spouse", false_value = 0, na_value = -1)
  expect_equal(result_no_true$spouse, c(NA, 0, 0, -1)) # True values are `NULL`, so no conversion

  # Test with edge cases in true_values
  data_edge <- data.frame(
    status = c("TRUE", "true", "FALSE", "True", NA),
    stringsAsFactors = FALSE
  )
  result_edge <- makenumeric(data_edge, column = "status", true_values = c("TRUE", "true"), true_number = 1, false_value = 0, na_value = -1)
  expect_equal(result_edge$status, c(1, 1, 0, 0, -1))
})

test_that("makenumeric converts values correctly", {
  # Create test data
  test_data <- data.frame(
    status = c("Living with spouse", "Not living with spouse", NA),
    stringsAsFactors = FALSE
  )

  # Test basic conversion
  result <- makenumeric(test_data,
                        column = "status",
                        true_values = "Living with spouse",
                        true_number = 1,
                        false_value = 0)

  expect_equal(result$status, c(1, 0, 0)) # `NA` is replaced by `false_value` in the function

  # Test with NA value replacement
  result_na <- makenumeric(test_data,
                           column = "status",
                           true_values = "Living with spouse",
                           true_number = 1,
                           false_value = 0,
                           na_value = -1)

  expect_equal(result_na$status, c(1, 0, -1))
})

test_that("makenumeric handles errors appropriately", {
  test_data <- data.frame(status = c("A", "B"), stringsAsFactors = FALSE)

  expect_error(makenumeric(test_data, "nonexistent"),
               "Column not found in the dataset.")

  test_numeric <- data.frame(value = c(1, 2, 3), stringsAsFactors = FALSE)
  expect_error(makenumeric(test_numeric, "value"),
               "The specified column must be character or factor")
})
