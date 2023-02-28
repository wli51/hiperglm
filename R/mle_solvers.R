linear.mle.pseudo_inverse <- function(design, outcome) {
  chol_solve_linear_system(A = t(design) %*% design,
                           b = t(design) %*% outcome)

}

linear.mle.BFGS <- function(design, outcome) {
  op <- stats::optim(
    par = rep(1, dim(design)[2]),
    fn = function(par)
      log_likelihood_linear(par, design, outcome),
    gr = function(par)
      log_likelihood_gradient_linear(par, design, outcome),
    control = list(fnscale = -1)
  )

  matrix(op$par, ncol = 1)
}
