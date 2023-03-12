#'
bfgs <- function(design, outcome, model = c("logit", "linear")) {
  match.arg(model, c("logit", "linear"))

  if (model == "linear") {
    op <- stats::optim(
      par = rep(1, dim(design)[2]),
      fn = function(par)
        linear.log.likelihood(par, design, outcome),
      gr = function(par)
        linear.log.likelihood.gradient(par, design, outcome),
      control = list(fnscale = -1)
    )
  } else if (model == "logit") {
    op <- stats::optim(
      par = rep(1, dim(design)[2]),
      fn = function(par)
        logistic.log.likelihood(par, design, outcome),
      gr = function(par)
        logistic.log.likelihood.gradient(par, design, outcome),
      control = list(fnscale = -1)
    )
  }

  matrix(op$par, ncol = 1)
}
