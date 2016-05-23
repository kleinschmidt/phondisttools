context("Utility functions")


x <- log(runif(100) + 1)

test_that("log_sum_exp is numerically stable for large and small numbers", {
  expect_equal(log_sum_exp(x), log(sum(exp(x))))
  expect_equal(log_sum_exp(x - 1e9) + 1e9, log_sum_exp(x))
  expect_equal(log_sum_exp(x + 1e9) - 1e9, log_sum_exp(x))
})

test_that("subtracting log_sum_exp normalizes to sum to 1", {
  x_norm <- x - log_sum_exp(x)
  expect_equal(sum(exp(x_norm)), 1)
})
