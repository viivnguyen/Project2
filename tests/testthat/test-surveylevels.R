test_that("surveylevels correctly converts ordered levels", {
  # Create test data
  test_data <- data.frame(
    proficiency = c("Well", "Not at all", NA, "Not well", "Very well")
  )
  response_scale <- c("Not at all", "Not well", "Well", "Very well")
  
  # Run function
  result <- surveylevels(test_data, "proficiency", response_scale)
  
  # Test expectations
  expect_equal(result$proficiency[1], 3)  # "Well" should be 3
  expect_equal(result$proficiency[2], 1)  # "Not at all" should be 1
  expect_true(is.na(result$proficiency[3]))  # NA should remain NA
  expect_equal(result$proficiency[4], 2)  # "Not well" should be 2
  expect_equal(result$proficiency[5], 4)  # "Very well" should be 4
})

test_that("surveylevels handles errors appropriately", {
  test_data <- data.frame(proficiency = c("Well", "Invalid"))
  response_scale <- c("Not at all", "Not well", "Well", "Very well")
  
  expect_error(surveylevels(test_data, "nonexistent", response_scale),
              "Column nonexistent not found in the dataframe")
  
  expect_error(surveylevels(test_data, "proficiency", response_scale),
              "Some values in the column do not match the provided ordered levels")
})
