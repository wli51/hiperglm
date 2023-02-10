hiper_glm <- function(design, outcome) {

  upported_model <- c("linear", "logit")

  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  warning("`hiper_glm` is yet to be implemented.")

  # TODO: implement find MLE.
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)

}
