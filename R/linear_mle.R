#'
linear.log.likelihood <- function(coef, x, y, noise_var = 1) {
  n <- length(y)
  - (n / 2) * log(2 * pi) - n * log(sqrt(noise_var)) - (1 / (2 * noise_var)) *
    sum((y - x %*% coef) ^ 2)

}

#'
linear.log.likelihood.gradient <-
  function(coef, x, y, noise_var = 1) {
    as.numeric((1 / noise_var) * t(x) %*% (y - x %*% coef))

  }

#'
linear.mle.pseudo_inverse <- function(design, outcome) {
  matrix(chol_solve_linear_system(
    A = t(design) %*% design,
    b = t(design) %*% outcome
  ),
  ncol = 1)

}

#'
chol_solve_linear_system <- function(A, b) {
  L <- t(chol(A))
  as.numeric(backsolve(t(L), forwardsolve(L, b)))

}

