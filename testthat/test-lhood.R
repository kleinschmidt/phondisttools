context("Model likelihood")

test_that("Log likelihood is log of non-log likelihood", {

  model <- list(mu = c(0,0), Sigma = diag(c(1,1)))

  expect_equal(log(model_lhood(model, c(0,0))),
               model_lhood(model, c(0,0), log=TRUE))

})



models <- map(-5:5, ~ list(mu = c(., .), Sigma = diag(c(1,1))))
dat <- c(0,0)

test_that("Marginal likelihood is mean of individual likelihoods", {

  expect_equal(marginal_model_lhood(models, dat, log=FALSE),
               mean(map_dbl(models, ~ model_lhood(., dat))))
  expect_equal(marginal_model_lhood(models, dat, log=TRUE),
               log(mean(exp(map_dbl(models, ~ model_lhood(., dat, log=TRUE))))))

})

test_that("Marginal likelihood defaults to log=TRUE", {

  expect_equal(marginal_model_lhood(models, dat),
               log(mean(exp(map_dbl(models, ~ model_lhood(., dat, log=TRUE))))))

})
