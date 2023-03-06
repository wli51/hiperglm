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
  1 / (1 + exp(-1 * x %*% coef))
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
  -1 * t(x) %*% weight_matrix(coef, x) %*% x
}

#'
weight_matrix <- function(coef, x) {
  n <- dim(x)[1]
  weight <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    p = logistic.fun(coef, x[i, ])
    weight[i, i] = p * (1 - p)
  }
  weight
}

logistic.mle.newton <-
  function(design,
           outcome,
           max_it = 1000,
           start_coef = NULL) {
    if (is.null(start_coef)) {
      coef <- rep(0, dim(design)[2])
    } else {
      coef <- start_coef
    }
    # 2 times difference in log likelihood under null model follows chi-sq
    # distribution, using mean of chi-sq with df1 here as proxy for negligible
    # increase in log likelihood
    negligible_log_lik_increment <- 1
    conservative_multiplier <- 0.05
    convergence_log_lik_tolerance <- negligible_log_lik_increment *
      conservative_multiplier

    for (i in 1:max_it) {
      log_lik_prev <- logistic.log_likelihood(coef, design, outcome)
      hessian <-
        logistic.log_likelihood.hessian(coef, design, outcome)
      gradient <-
        logistic.log_likelihood.gradient(coef, design, outcome)
      coef <- coef - matlib::inv(hessian) %*% gradient
      log_lik_curr <- logistic.log_likelihood(coef, design, outcome)
      if (log_lik_curr - log_lik_prev < convergence_log_lik_tolerance/2) {
        break
      }
      log_lik_prev <- log_lik_curr
    }

    matrix(coef, ncol = 1)
  }

