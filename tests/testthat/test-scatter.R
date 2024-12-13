test_that("scatter plot creation works", {
  # Create test data
  test_data <- data.frame(
    x = 1:5,
    y = 1:5,
    group = c("A", "A", "B", "B", "B")
  )
  
  # Test basic plot creation
  p <- scatter(test_data, x = "x", y = "y", color = "group")
  expect_s3_class(p, "ggplot")
  
  # Test with optional parameters
  p_with_options <- scatter(test_data, 
                          x = "x", 
                          y = "y", 
                          color = "group",
                          title = "Test Plot",
                          add_trendline = TRUE)
  expect_s3_class(p_with_options, "ggplot")
})

test_that("scatter plot handles errors appropriately", {
  test_data <- data.frame(x = 1:5, y = 1:5)
  
  # Test invalid data frame
  expect_error(scatter("not_a_dataframe", x = "x", y = "y"),
              "'data' must be a data frame")
  
  # Test missing columns
  expect_error(scatter(test_data, x = "nonexistent", y = "y"),
              "object 'nonexistent' not found")
})
