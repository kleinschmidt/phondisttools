#' @importFrom psych tr
NULL

## Functions for calculating KL divergence between vowel distributions

#' make a 2D variance-covariance matrix from a vector of vars and covar
#'
#' @param dim1 variance of dimension 1
#' @param dim2 variance of dimension 2
#' @param cov12 covariance
#'
#' @export
covar2D = function(dim1, dim2, cov12) {
  matrix(c(dim1, cov12,
           cov12, dim2),
         ncol = 2)
}

#' Calculate the KL divergence between two bivariate Gaussians
#'
#' @param mu1,mu2 mean vector of two distributions
#' @param sigma1,sigma2 covariance matrix in lower triangular vector (see
#'   \code{\link{covar2D}}) or matrix form
#'
#' @export
KL.bvnorm <- function(
  # distributional parameters for bivariate Gaussian 1
  mu1, 
  sigma1,
  # distributional parameters for bivariate Gaussian 2
  mu2, 
  sigma2
  ) 
{

  # variance-covariance matrix for Gaussian 1
  if (! is.matrix(sigma1) )
    Sigma1 <- covar2D(dim1 = sigma1[1], 
                      dim2 = sigma1[2], 
                      cov12 = sigma1[3])
  else
    Sigma1 <- sigma1

  # variance-covariance matrix for Gaussian 2
  if (! is.matrix(sigma2) )
    Sigma2 <- covar2D(dim1 = sigma2[1], 
                      dim2 = sigma2[2], 
                      cov12 = sigma2[3])
  else
    Sigma2 <- sigma2

  ## ------------------
  ## (Dividing by log(2) gives KL in bits)
  kl <- .5 * (log(det(Sigma2) / det(Sigma1)) - 
          dim(Sigma1)[1] + 
          psych::tr(solve(Sigma2)%*%Sigma1) +
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
  KL.bvnorm(mod1$mu, mod1$Sigma, mod2$mu, mod2$Sigma)
}
