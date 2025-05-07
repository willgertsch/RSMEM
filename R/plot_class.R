plot_class = function(model, y, Z, X, type) {

  if (type == 'slr') {
    # plot for when outcome model is a simple linear regression
    if (length(model$beta1) != 2) {
      stop('Outcome model does not have two predictors.')
    }

    class = predict_class(model, X, threshold = 0.5)
    plot_dat = data.frame(
      y = y,
      class = class,
      Z1 = Z[, 2]
    )

    p = ggplot2::ggplot(plot_dat, ggplot2::aes(x = Z1, y = y, color = class)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = 'lm', se = F) +
      ggplot2::theme_bw()
    return(p)
  }
  else {
    warning('z_type not supported.')
    return(NULL)
  }
}
