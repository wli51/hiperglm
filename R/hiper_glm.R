#' @export
hiper_glm <- function(design, outcome, model="linear", option = list()) {

  supported_model <- c("linear", "logit")

  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  coef_estimate <- NULL

  if (!is.null(option[["mle_solver"]])) {

    if (option[["mle_solver"]] == "BFGS") {
      # message("Using BFGS\n")

      op <- stats::optim(par = rnorm(dim(design)[2]),
                         fn = function(par) log_likelihood_linear(par, design, outcome),
                         gr = function(par) log_likelihood_gradient_linear(par, design, outcome),
                         control = list(fnscale=-1))

      coef_estimate <- op$par

    } else {
      warning("No current plans for adding solver options other than pseudo-inverse and BFGS.")
      stop()
    }


  } else {
    # message("No mle_solver option specified, using pseudo inverse by default\n")
    # coef_estimate <- as.numeric(
    #   chol2inv(chol(t(design) %*% design))%*%t(design)%*%outcome)
    coef_estimate <- as.numeric(
      chol_solve_linear_system(A = t(design) %*% design, b=t(design) %*% outcome)
    )
  }

  # warning("`hiper_glm` is yet to be implemented.")

  # TODO: implement find MLE.
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  hglm_out$coef <- coef_estimate
  return(hglm_out)

}
