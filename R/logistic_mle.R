#'
logistic.log_likelihood <- function(coef, x, y) {
  n <- length(y)
  p_x_coef <- logistic.fun(coef, x)
  y <- matrix(y, ncol = 1)
  sum(y * log(p_x_coef) + (1 - y) * log(1 - p_x_coef))
}

#'
logistic.log_likelihood.gradient <- function(coef, x, y) {
  n <- length(y)
  m <- length(coef)
  gradient <- NULL
  for (j in 1:m) {
    g <- sum(x[,j]*(y - logistic.fun(coef, x)))
    gradient <- c(gradient, g)
  }
  gradient
}

#'
logistic.fun <- function(coef, x) {
  1/(1+exp(-1 * x %*% coef))
}
