#' @export
hiper_glm <- function(design, outcome, model="linear", option = list(mle_solver=c("pseudo inverse", "BFGS"))) {

  supported_model <- c("linear", "logit")

  if (!(model %in% supported_model)) {

    stop(sprintf("The model %s is not supported.", model))

  }

  option$mle_solver <- match.arg(option$mle_solver, c("pseudo inverse", "BFGS"))

  coef_estimate <- switch (option$mle_solver,
    "pseudo inverse" = linear.mle.pseudo_inverse(design, outcome),
    "BFGS" = coef_estimate <- linear.mle.BFGS(design, outcome),
    NULL
  )

  hglm_out <- list()
  class(hglm_out) <- "hglm"
  hglm_out$coef <- coef_estimate
  return(hglm_out)

}
