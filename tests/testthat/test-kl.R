context("KL Divergence")

test_that("Lower triangular to square matrix is symmetric and reversible", {
  for (p in 1:10) {
    x <- runif(p*(p+1)/2)
    X <- phondisttools:::lower_tri_to_full(x)
    expect_equal(X[lower.tri(X, diag=TRUE)], x)
    # symmetric
    expect_equal(X[lower.tri(X)], t(X)[lower.tri(X)])
  }
})

test_that("KL is >= 0, and 0 only for identical distributions", {
  expect_true(KL_mvnorm(c(0,0), diag(c(1,1)), c(0,0), diag(c(1,1))) == 0)
  expect_true(KL_mvnorm(c(1,0), diag(c(1,1)), c(0,0), diag(c(1,1))) > 0)
  expect_true(KL_mvnorm(c(0,0), diag(c(2,1)), c(0,0), diag(c(1,1))) > 0)
  expect_true(KL_mvnorm(c(0,0), diag(c(1,1)), c(1,0), diag(c(1,1))) > 0)
  expect_true(KL_mvnorm(c(0,0), diag(c(1,1)), c(0,0), diag(c(1,2))) > 0)
  expect_true(KL_mvnorm(c(0,0), diag(c(1,1)), c(1,0), matrix(c(1,0.1,0.1,1), 2)) > 0)
})

test_that("KL_mods unpacks mu and Sigma from model lists", {
  expect_equal(KL_mods(list(mu=c(0,0), Sigma=diag(2)), list(mu=c(1,2), Sigma=diag(2)*1.3)),
               KL_mvnorm(c(0,0), diag(2), c(1,2), diag(2)*1.3))
})
