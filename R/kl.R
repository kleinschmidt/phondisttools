NULL

## Functions for calculating KL divergence between distributions

#' Expand lower-triangular vector to full matrix
#'
#' @param lower_tri_vec Lower triangular vector of length p*(p+1)/2
#' @return Symmetric, square p×p matrix
lower_tri_to_full <- function(lower_tri_vec) {
  assert_that(is.vector(lower_tri_vec))
  # lower tri has p*(p+1)/2 = n elements.  solve for n:
  # p^2+p = 2n
  # p^2 + p - 2n = 0
  # p = (-1 \pm \sqrt(1 + 8n))/2
  n <- length(lower_tri_vec)
  p <- (sqrt(1+8*n)-1) / 2
  assert_that(min(abs(c(p%%1, p%%1-1))) < .Machine$double.eps^0.5,
              msg=paste("Vector of length", n, "can't be lower triangular part!"))
  X <- diag(p)
  X[lower.tri(X, diag=TRUE)] <- lower_tri_vec
  X + t(X) - diag(diag(X), p)
}

#' Calculate the KL divergence between two multivariate Gaussians
#'
#' @param mu1,mu2 mean vector of two distributions
#' @param sigma1,sigma2 covariance matrix (or lower-triangular vectors)
#'
#' @export
KL_mvnorm <- function(mu1, sigma1, mu2, sigma2) {
  assert_that(is.vector(mu1))
  assert_that(is.vector(mu2))
  assert_that(length(mu1) == length(mu2))

  # variance-covariance matrix for Gaussian 1
  
  if (! is.matrix(sigma1) )
    Sigma1 <- lower_tri_to_full(sigma1)
  else
    Sigma1 <- sigma1

  # variance-covariance matrix for Gaussian 2
  if (! is.matrix(sigma2) )
    Sigma2 <- lower_tri_to_full(sigma2)
  else
    Sigma2 <- sigma2

  assert_that(is.matrix(Sigma1))
  assert_that(is.matrix(Sigma2))

  ## ------------------
  ## (Dividing by log(2) gives KL in bits)
  kl <- .5 * (log(det(Sigma2) / det(Sigma1)) - 
          dim(Sigma1)[1] + 
          sum(diag(solve(Sigma2)%*%Sigma1)) +
          t(mu2-mu1) %*% solve(Sigma2) %*% (mu2-mu1)
        ) / log(2)
        
  return(kl)
}

#' Compute KL divergence of one model from a second
#'
#' The KL divergence of mod2 from mod1 is the cost (in bits) of encoding data
#' drawn from the distribution of the true model (\code{mod1}) using a code that
#' is optimized for another model's distribution (\code{mod2}). That is, it
#' measures how much it hurts to think that data is coming from \code{mod2} when
#' it's actually generated from \code{mod1}.
#'
#' @param mod1 true model (list with fields mu and Sigma)
#' @param mod2 other model
#' @return KL divergence of mod2 (candidate) from mod1 (true model), in bits.
#'
#' @export
KL_mods <- function(mod1, mod2) {
  KL_mvnorm(mod1$mu, mod1$Sigma, mod2$mu, mod2$Sigma)
}
