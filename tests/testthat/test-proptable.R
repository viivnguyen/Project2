test_that("proptable works as expected", {
  # Example dataset
  data <- data.frame(
    group = c("A", "A", "B", "B", "B", "C", "C", "C", "C"),
    outcome = c("Success", "Failure", "Success", "Failure", "Failure", "Success", "Failure", "Success", "Failure")
  )

  # Test proportion output
  result <- proptable(data, x = group, y = outcome, type = "proportion", digits = 2)
  expect_equal(result$prop[1:2], c(0.5, 0.5))  # Group A proportions
  expect_equal(result$prop[3:4], c(0.33, 0.67), tolerance = 0.01)  # Group B proportions

  # Test count output
  result_count <- proptable(data, x = group, y = outcome, type = "count")
  expect_equal(result_count$n[1:2], c(1, 1))  # Group A counts
  expect_equal(result_count$n[3:4], c(1, 2))  # Group B counts

  # Test including totals
  result_total <- proptable(data, x = group, y = outcome, type = "count", include_total = TRUE)
  expect_true("total" %in% names(result_total))
  expect_equal(result_total$total[1:2], c(2, 2))  # Group A totals
  expect_equal(result_total$total[3:5], c(3, 3, 3))  # Group B totals

  # Test invalid type
  expect_error(proptable(data, x = group, y = outcome, type = "invalid_type"))

  # Test invalid data input
  expect_error(proptable("not_a_dataframe", x = group, y = outcome))
})
