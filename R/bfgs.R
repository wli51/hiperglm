#'
bfgs <- function(design, outcome, obj_fn, obj_grad) {
  op <- stats::optim(
    par = rep(1, dim(design)[2]),
    fn = function(par)
      obj_fn(par, design, outcome),
    gr = function(par)
      obj_grad(par, design, outcome),
    control = list(fnscale = -1)
  )

  matrix(op$par, ncol = 1)
}
