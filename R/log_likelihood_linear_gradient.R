#'
log_likelihood_gradient_linear <-
  function(coef, x, y, noise_var = 1) {
    as.numeric((1 / noise_var) * t(x) %*% (y - x %*% coef))

  }
