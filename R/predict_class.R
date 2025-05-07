#' Predict class assignments in subgroup model.
#'
#' @param model Result from calling rsmem function.
#' @param X Design matrix for subgroup membership model
#' @param threshold Observations with p > threshold are assigned to A, B otherwise.
#'
#' @return A vector of class assignments.
#' @export
#'
#' @examples
predict_class = function(model, X, threshold = 0.5) {

  gamma = model$gamma
  p = ilogit(X %*% gamma)
  class = ifelse(p > threshold, 'A', 'B')
  return(class)

}
