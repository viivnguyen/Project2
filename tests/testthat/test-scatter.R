test_that("scatter generates ggplot objects", {
  data <- data.frame(age = c(20, 30, 40), income = c(20000, 30000, 40000), education = c("High School", "Bachelor's", "Master's"))

  # Basic plot
  plot <- scatter(data, x = "age", y = "income", color = "education")
  expect_s3_class(plot, "ggplot")

  # Missing column
  expect_error(scatter(data, x = "missing", y = "income"))

  # Faceting
  expect_s3_class(scatter(data, x = "age", y = "income", facet_by = "education"), "ggplot")
})
