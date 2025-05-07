rlnm = function(n, beta1, beta2, gamma, sigma1, sigma2) {
  q1 = length(beta1)
  q2 = length(gamma)

  # randomize to treatment arms
  # order doesn't matter
  # equal allocation for now
  trt = rep(c(0, 1), n/2)

  # design matrices
  # 1 + trt + cov for outcome level
  # 1 + cov for subgroup level
  Z = cbind(rep(1, n), trt, matrix(rnorm(n * (q1 - 2)), nrow = n, ncol = q1-2))
  X = cbind(rep(1, n), matrix(rnorm(n * (q2 - 1)), nrow = n, ncol = q2-1))

  # mean structures
  mu1 = Z %*% beta1
  mu2 = Z %*% beta2
  eta = X %*% gamma

  # generate classes
  class = rbinom(n, 1, exp(eta) / (1 + exp(eta)))

  # generate mixture outcome
  Y = numeric(n)
  for (i in 1:n) {
    if (class[i] == 1) {
      Y[i] = rnorm(1, mu1[i], sigma1)
    }
    else if (class[i] == 0) {
      Y[i] = rnorm(1, mu2[i], sigma2)
    }
  }

  return(list(
    Y = Y,
    Z = Z,
    X = X,
    class = class
  ))
}
