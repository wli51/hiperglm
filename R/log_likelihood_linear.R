log_likelihood_linear <- function(coef, x, y, noise_var = 1) {

  n = length(y)

  -(n/2)*log(2*pi)-n*log(sqrt(noise_var))-(1/(2*noise_var))*sum((y - x %*% coef)^2)

}
