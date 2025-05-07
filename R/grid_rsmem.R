#' Fit RSMEM over a grid of regularization parameters
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
#' @param lam_grid Vector of regularization parameter values.
#' @param show_progress Whether to show progress bar for algorithm.
#'
#' @return
#' Matrix where the first column is the lambda value, then parameter values.
#' @export
#'
#' @examples
#' # fit to Hitters data with regularization
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
#'
#' grid = 10^seq(10, -2, length = 20)
#' set.seed(245)
#' grid_res = grid_rsmem(y,z,x, iter = 5000, swarm = 64, 'DE', bounds, lam_grid = grid)
#' grid_res
#'
#' ggplot2::ggplot(grid_res, ggplot2::aes(x = log(lambda), y = MSE)) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_line() +
#'   ggplot2::theme_bw()
#'
grid_rsmem = function(Y, Z, X, iter, swarm, algorithm, bounds, lam_grid, show_progress = F, seed=1234) {

  #browser()
  q1 = ncol(Z)
  q2 = ncol(X)
  K = length(lam_grid)
  param_mat = matrix(NA, nrow = K, ncol = 2*q1 + q2 + 2)
  BIC = numeric(K)
  l1 = numeric(K)
  num_par = numeric(K)
  for (i in 1:K) {
    cat("Fitting model: ", i, "/", K, "\n", sep = "")
    # fit model
    res_i = rsmem(Y, Z, X, iter = iter, swarm = swarm, algorithm, bounds,
                  lam = lam_grid[i], show_progress, seed = seed)

    # predict
    #yhat_i = predict_y(res_i, Z, X, 0.5)
    l1[i] = sum(abs(res_i$gamma))

    num_par[i] = sum(abs(res_i$gamma)!=0)

    # compute BIC
    BIC[i] = -2*lnm_logl(res_i$beta1, res_i$beta2, res_i$sigma1, res_i$sigma2,
                         res_i$gamma, Y, Z, X) + sum(abs(res_i$gamma)!=0)*log(length(Y))

    # save parameters
    param_mat[i, ] = unlist(res_i)[-1]
  }

  return(data.frame(lambda = lam_grid, BIC = BIC, l1 = l1, num_par=num_par, param_mat))
}
