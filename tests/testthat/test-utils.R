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

test_that("log_sum_exp of Inf and anything is Inf", {
  expect_equal(log_sum_exp(Inf), Inf)
  expect_equal(log_sum_exp(c(0, Inf)), Inf)
})

test_that("log_sum_exp of -Inf and anything is log_sum_exp of non-infinite", {
  expect_equal(log_sum_exp(-Inf), -Inf)
  expect_equal(log_sum_exp(c(0, -Inf)), 0)
  expect_equal(log_sum_exp(c(-Inf, Inf)), Inf)
})
