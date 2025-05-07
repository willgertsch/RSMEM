
#' Fit (regularized) subgroup mixture of experts using metaheuristics
#'
#' @param Y Outcome vector.
#' @param Z Design matrix for outcome-level regression.
#' @param X Design matrix for subgroup-level regression.
#' @param iter Maximum number of iterations for algorithm.
#' @param swarm Swarm size.
#' @param algorithm Algorithm to use. Supports algorithms from metaheuristicOpt.
#' @param bounds Parameter bounds to search within. Needs to be a named list with
#' entries beta1_lb, beta1_ub, beta2_lb, beta2_ub, gamma_lb, gamma_ub, sigma_lb,
#' sigma_ub.
#' @param lam Value of regularization parameter.
#' @param show_progress Whether to show progress bar for algorithm.
#'
#' @return
#' A named list of parameter values and objective function value.
#' @export
#'
#' @examples
#' # fit to Hitters data with no regularization
#' Hitters = na.omit(ISLR2::Hitters)[c(-173, -241),]
#' y = Hitters$Salary
#' z = model.matrix(Salary ~ RBI, Hitters)
#' x = model.matrix(Salary~ ., Hitters)[,c(-1,-6)]
#' bounds = list(
#'   beta1_lb = 0,
#'   beta1_ub = max(Hitters$Salary),
#'   beta2_lb = 0,
#'   beta2_ub = max(Hitters$Salary),
#'   gamma_lb = -5,
#'   gamma_ub = 5,
#'   sigma_lb = 1,
#'   sigma_ub = sd(Hitters$Salary)
#' )
#' set.seed(312)
#' res = rsmem(y,z,x, iter = 5000, swarm = 64, 'HS', bounds)
#' res
#'
#' table(predict_class(res, x))
#'
#' plot_class(res, y, z, x, 'slr') +
#' ggplot2::labs(
#'   y = "Salary",
#'   x = 'RBI'
#' )
#'
#' # Setting lambda
#' set.seed(312)
#' res1 = rsmem(y,z,x, iter = 5000, swarm = 64, 'HS', bounds, lam = 1)
#' res1
#' cbind(res$gamma, res1$gamma) # compare subgroup predictors
rsmem = function(Y, Z, X, iter, swarm, algorithm, bounds, lam=0, show_progress = T, seed = 1234) {

  # get dimensions
  n = length(Y)
  q1 = ncol(Z)
  q2 = ncol(X)

  # set up objective function
  obj_fun = function(param) {

    beta1 = param[1:q1]
    beta2 = param[(q1 + 1):(2*q1)]
    gamma = param[(2*q1+1):(2*q1 + q2)]
    sigma1 = param[2*q1 + q2 + 1]
    sigma2 = param[2*q1 + q2 + 2]
    LL = lnm_logl(beta1, beta2, sigma1, sigma2, gamma, Y, Z, X)

    penalty = lam*sum(abs(gamma))
    obj = -LL + penalty

    # deal with missing
    if (is.na(obj))
      return(Inf)
    else
      return(obj)
  }

  # set bounds for problem
  beta1_bounds = matrix(c(
    rep(c(bounds$beta1_lb, bounds$beta1_ub), q1)
  ), nrow = 2)
  beta2_bounds = matrix(c(
    rep(c(bounds$beta2_lb, bounds$beta2_ub), q1)
  ), nrow = 2)
  gamma_bounds = matrix(c(
    rep(c(bounds$gamma_lb, bounds$gamma_ub), q2)
  ), nrow = 2)
  sigma_bounds = matrix(c(
    rep(c(bounds$sigma_lb, bounds$sigma_ub), 2)
  ), nrow = 2)

  bounds = cbind(beta1_bounds, beta2_bounds, gamma_bounds, sigma_bounds)

  # call metaheuristics library to maximize likelihood
  control = list()
  control$maxIter = iter
  control$numPopulation = swarm
  if (show_progress) {
    out = metaheuristicOpt::metaOpt(
      FUN = obj_fun,
      optimType = "MIN",
      algorithm = algorithm,
      numVar = 2*q1 + q2 + 2,
      rangeVar = bounds,
      control = control,
      seed = seed
    )
  }
  else {
    invisible(capture.output({out = metaheuristicOpt::metaOpt(
      FUN = obj_fun,
      optimType = "MIN",
      algorithm = algorithm,
      numVar = 2*q1 + q2 + 2,
      rangeVar = bounds,
      control = control,
      seed = seed
    )}))
  }



  # extract and return parameters
  beta1 = out$result[1:q1]
  beta2 = out$result[(q1 + 1):(2*q1)]
  gamma = out$result[(2*q1+1):(2*q1 + q2)]
  sigma = out$result[(2*q1 + q2 + 1):(2*q1 + q2 + 2)]
  return(list(
    ll = out$optimumValue[1],
    beta1 = beta1,
    beta2 = beta2,
    gamma = gamma,
    sigma1 = sigma[1],
    sigma2 = sigma[2]
  ))

}
