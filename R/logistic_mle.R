#'
logistic.log_likelihood <- function(coef, x, y) {
  n <- length(y)
  p_x_coef <- logistic.fun(coef, x)
  y <- matrix(y, ncol = 1)
  sum(y * log(p_x_coef) + (1 - y) * log(1 - p_x_coef))
}

#'
logistic.log_likelihood.gradient <- function(coef, x, y) {
  p <- logistic.fun(coef, x)
  t(x) %*% (y - p)
}

#'
logistic.fun <- function(coef, x) {
  1/(1+exp(-1 * x %*% coef))
}

#'
logistic.mle.BFGS <- function(design, outcome) {
  op <- stats::optim(
    par = rep(1, dim(design)[2]),
    fn = function(par)
      logistic.log_likelihood(par, design, outcome),
    gr = function(par)
      logistic.log_likelihood.gradient(par, design, outcome),
    control = list(fnscale = -1)
  )

  matrix(op$par, ncol = 1)
}

#'
logistic.log_likelihood.hessian <- function(coef, x, y) {
  -1*t(x) %*% weight_matrix(coef, x) %*% x
}

#'
weight_matrix <- function(coef, x) {
  n <- dim(x)[1]
  weight <- matrix(0, nrow=n, ncol=n)

  for (i in 1:n) {
    p = logistic.fun(coef, x[i,])
    weight[i,i] = p * (1-p)
  }
  weight
}

logistic.mle.newton <- function(design, outcome, it=1000, start_coef = NULL, qr_tol = 1e-5) {

  if (is.null(start_coef)) {
    coef <- rep(0, dim(design)[2])
  } else {
    coef <- start_coef
  }

  for (i in 1:it) {
    hessian <- logistic.log_likelihood.hessian(coef, design, outcome)
    gradient <- logistic.log_likelihood.gradient(coef, design, outcome)

    coef <- coef - matlib::inv(hessian) %*% gradient
  }

  matrix(coef, ncol=1)
}
