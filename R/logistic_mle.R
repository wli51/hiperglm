#'
logistic.log.likelihood <- function(coef, x, y) {
  n <- length(y)
  p_x_coef <- logistic(coef, x)
  y <- matrix(y, ncol = 1)
  sum(y * log(p_x_coef) + (1 - y) * log(1 - p_x_coef))
}

#'
logistic.log.likelihood.gradient <- function(coef, x, y) {
  p <- logistic(coef, x)
  t(x) %*% (y - p)
}

#'
logistic <- function(coef, x) {
  1 / (1 + exp(-1 * x %*% coef))
}

#'
logistic.log.likelihood.hessian <- function(coef, x, y) {
  -t(x) %*% weight_matrix(coef, x) %*% x
}

#'
weight_matrix <- function(coef, x) {
  n <- dim(x)[1]
  p <- logistic(coef, x)
  weights <- p * (1 - p)
  matrix(diag(as.numeric(weights)), ncol = n, nrow = n)
}

#'
logistic.mle.newton <-
  function(design,
           outcome,
           max_it = 1000,
           start_coef = NULL,
           conservative_multiplier = 0.00025) {
    if (is.null(start_coef)) {
      coef <- rep(0, dim(design)[2])
    } else {
      coef <- start_coef
    }
    # 2 times difference in log likelihood under null model follows chi-sq
    # distribution, using mean of chi-sq with df1 here as proxy for negligible
    # increase in log likelihood
    negligible_log_lik_increment <- 1
    convergence_log_lik_tolerance <- negligible_log_lik_increment *
      conservative_multiplier

    for (i in 1:max_it) {
      log_lik_prev <- logistic.log.likelihood(coef, design, outcome)
      hessian <-
        logistic.log.likelihood.hessian(coef, design, outcome)
      gradient <-
        logistic.log.likelihood.gradient(coef, design, outcome)
      coef <- coef - solve(hessian) %*% gradient
      log_lik_curr <- logistic.log.likelihood(coef, design, outcome)
      if (log_lik_curr - log_lik_prev < convergence_log_lik_tolerance) {
        break
      }
      log_lik_prev <- log_lik_curr
    }

    matrix(coef, ncol = 1)
  }
