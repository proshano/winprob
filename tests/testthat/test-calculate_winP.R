library(testthat)
library(winprob)

# Create some sample data
  sample_data <- data.frame(
    id = 1:61,
    group = c(rep(0, 27), rep(1, 34)),
    pre = c(18, 27, 16, 17, 15, 20, 16, 28, 28, 25, 24, 16, 26, 21, 21, 22, 26, 19, 22, 16, 21, 20, 17, 22, 19, 21, 18, 21, 27, 15, 24, 15, 17, 20, 18, 28, 21, 18, 27.46, 19, 20, 16, 21, 23, 23, 24, 25, 22, 20, 20, 25, 18, 26, 20, 17, 22, 22, 23, 17, 22, 26),
    post = c(17, 26, 17, 14, 12, 19, 13, 26, 26, 9, 14, 19, 13, 7, 18, 18, 19, 19, 20, 7, 19, 16, 15, 20, 16, 7, 19, 13, 8, 8, 14, 15, 9, 7, 8, 11, 7, 8, 22, 14, 13, 17, 19, 11, 16, 16, 20, 15, 7, 12.13, 15, 17, 1, 27, 20, 12, 15.38, 11, 15, 7, 24)
  )

test_that("calculate_winP works with valid inputs", {
  result <- calculate_winP(sample_data, "group", "post")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("WinP" %in% names(result))
  expect_true(result$WinP >= 0 && result$WinP <= 1)
})

test_that("calculate_winP works with pre_var", {
  result <- calculate_winP(sample_data, "group", "post", "pre")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("WinP" %in% names(result))
  expect_true(result$WinP >= 0 && result$WinP <= 1)
})

test_that("calculate_winP throws error with invalid group_var", {
  expect_error(calculate_winP(sample_data, "non_existent", "post"))
})

test_that("calculate_winP throws error with invalid post_var", {
  expect_error(calculate_winP(sample_data, "group", "non_existent"))
})

test_that("calculate_winP results are consistent", {
  result1 <- calculate_winP(sample_data, "group", "post")
  result2 <- calculate_winP(sample_data, "group", "post")

  expect_equal(result1, result2)
})

