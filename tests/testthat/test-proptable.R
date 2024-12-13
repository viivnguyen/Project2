test_that("proptable calculates proportions correctly", {
  # Create test data
  test_data <- data.frame(
    group = c("A", "A", "B", "B", "B"),
    category = c("X", "Y", "X", "X", "Y")
  )
  
  # Test proportion calculation
  result <- proptable(test_data, x = group, y = category)
  
  # Check group A proportions
  expect_equal(result$prop[result$group == "A" & result$category == "X"], 0.5)
  expect_equal(result$prop[result$group == "A" & result$category == "Y"], 0.5)
  
  # Check group B proportions
  expect_equal(result$prop[result$group == "B" & result$category == "X"], 0.67, tolerance = 0.01)
  expect_equal(result$prop[result$group == "B" & result$category == "Y"], 0.33, tolerance = 0.01)
})

test_that("proptable handles optional parameters correctly", {
  test_data <- data.frame(
    group = c("A", "A", "B", "B", "B"),
    category = c("X", "Y", "X", "X", "Y")
  )
  
  # Test count option
  count_result <- proptable(test_data, x = group, y = category, type = "count")
  expect_true("n" %in% names(count_result))
  expect_false("prop" %in% names(count_result))
  
  # Test include_total option
  total_result <- proptable(test_data, x = group, y = category, include_total = TRUE)
  expect_true("total" %in% names(total_result))
})
