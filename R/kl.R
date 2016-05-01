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
#' @param mu1 mean vector of first distribution
#' @param sigma1 covariance matrix in vector (see covar2D) or matrix form
#' @param mu2
#' @param sigma2
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
  # SHOULD THE LOG BE BASE 2 ???
  kl <- .5 * (log2(det(Sigma2) / det(Sigma1)) - 
          dim(Sigma1)[1] + 
          tr(solve(Sigma2)%*%Sigma1) +
          t(mu2-mu1) %*% solve(Sigma2) %*% (mu2-mu1)
        )
        
  return(kl)
}

#' Compute KL divergence between two models
#'
#' @param mod1 Model, a list with fields mu and Sigma
#' @param mod2
#'
#' @export
KL_mods <- function(mod1, mod2) {
  KL.bvnorm(mod1$mu, mod1$Sigma, mod2$mu, mod2$Sigma)
}
