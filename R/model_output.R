#' @export
coef.hglm <- function(hglm_out) {

  # warning("To be implemented.")
  return(hglm_out$coef)

}

#' @export
vcov.hglm <- function(hglm_out) {

  warning("To be implemented.")

}

#' @export
print.hglm <- function(hglm_out, ...) {

  # TODO:
  cat("Output of hiperglm")

}
