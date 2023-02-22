log_likelihood_gradient_linear <- function(coef, x, y, noise_var = 1) {

  as.numeric(t(x) %*% (y - x %*% coef))

}
