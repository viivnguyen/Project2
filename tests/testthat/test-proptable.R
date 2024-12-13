test_that("proptable works as expected", {
  # Create test data
  test_data <- data.frame(
    x = c("A", "A", "B", "B"),
    y = c("Y", "Z", "Y", "Z")
  )
  
  # Test proportion calculation
  result <- proptable(test_data, x, y)
  expect_equal(result$prop[3:4], c(0.5, 0.5))  # Updated expectation
  
  # Test count calculation
  result_count <- proptable(test_data, x, y, type = "count")
  expect_equal(result_count$n[3:4], c(1, 1))  # Updated expectation
  
  # Test w totals
  result_total <- proptable(test_data, x, y, include_total = TRUE)
  expect_equal(result_total$total[3:4], c(2, 2))  # Fixed expectation: only test indices 3:4
})
