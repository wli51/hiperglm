#' @export
hiper_glm <- function(design, outcome, model="linear", option = list()) {

  supported_model <- c("linear", "logit")

  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  coef_estimate <- NULL

  if (!is.null(option[["mle_solver"]])) {

    if (option[["mle_solver"]] == "BFGS") {
      message("Using BFGS\n")

    } else {
      warning("No current plans for adding solver options other than pseudo-inverse and BFGS.")
      stop()
    }


  } else {
    message("No mle_solver option specified, using pseudo inverse by default\n")
    coef_estimate <- as.numeric(
      chol2inv(chol(t(design) %*% design))%*%t(design)%*%outcome)
  }

  # warning("`hiper_glm` is yet to be implemented.")

  # TODO: implement find MLE.
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  hglm_out$coef <- coef_estimate
  return(hglm_out)

}
