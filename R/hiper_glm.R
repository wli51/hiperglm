#' @export
hiper_glm <-
  function(design,
           outcome,
           model = "linear",
           option = list(mle_solver = NULL)) {
    supported_model <- c("linear", "logit")

    if (!(model %in% supported_model)) {
      stop(sprintf("The model %s is not supported.", model))

    }

    coef_estimate <- NULL

    if (model == "linear") {
      option$mle_solver <-
        match.arg(option$mle_solver, c("pseudo inverse", "BFGS"))
      if (option$mle_solver == "pseudo inverse") {
        coef_estimate <- linear.mle.pseudo_inverse(design, outcome)
      } else if (option$mle_solver == "BFGS") {
        coef_estimate <- bfgs(design, outcome, model="linear")
      } else {
        stop("No current plans for adding solver options other than pseudo inverse and BFGS.")
      }
    } else if (model == "logit") {
      option$mle_solver <-
        match.arg(option$mle_solver, c("newton", "BFGS"))
      if (option$mle_solver == "newton") {
        coef_estimate <- logistic.mle.newton(design, outcome)
      } else if (option$mle_solver == "BFGS") {
        coef_estimate <- bfgs(design, outcome, model="logit")
      }
    }

    hglm_out <- list()
    class(hglm_out) <- "hglm"
    hglm_out$coef <- coef_estimate
    return(hglm_out)

  }
