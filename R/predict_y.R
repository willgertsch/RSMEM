predict_y = function(model, Z, X, threshold=0.5) {

  class = predict_class(model, X, threshold)
  # could improve code here
  yhat_A = Z %*% model$beta1
  yhat_B = Z %*% model$beta2
  y = ifelse(class == 'A', yhat_A, yhat_B)
  return(y)
}
