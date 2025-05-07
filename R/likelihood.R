
# unregularized log-likelihood
lnm_logl = function(beta1, beta2, sigma1, sigma2, gamma, Y, Z, X) {


  eta = X %*% gamma
  p = ilogit(eta)

  mu1 = Z %*% beta1
  mu2 = Z %*% beta2

  f1 = suppressWarnings(dnorm(Y, mu1, sigma1))
  f2 = suppressWarnings(dnorm(Y, mu2, sigma2))

  ll = sum(log(p * f1 + (1 - p) * f2))

  return(ll)
}
