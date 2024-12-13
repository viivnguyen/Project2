usethis::use_test("surveylevels")test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("makenumeric converts values correctly", {
  # Create test data
  test_data <- data.frame(
    status = c("Living with spouse", "Not living with spouse", NA)
  )
  
  # Test basic conversion
  result <- makenumeric(test_data, 
                       column = "status",
                       true_values = "Living with spouse",
                       true_number = 1,
                       false_value = 0)
  
  expect_equal(result$status[1], 1)
  expect_equal(result$status[2], 0)
  expect_true(is.na(result$status[3]))
  
  # Test with NA value replacement
  result_na <- makenumeric(test_data,
                          column = "status",
                          true_values = "Living with spouse",
                          true_number = 1,
                          false_value = 0,
                          na_value = -1)
  
  expect_equal(result_na$status[3], -1)
})

test_that("makenumeric handles errors appropriately", {
  test_data <- data.frame(status = c("A", "B"))
  
  expect_error(makenumeric(test_data, "nonexistent"),
              "Column not found in the dataset")
  
  test_numeric <- data.frame(value = c(1, 2, 3))
  expect_error(makenumeric(test_numeric, "value"),
              "The specified column must be character or factor")
})
