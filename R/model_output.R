#' @export
coef.hglm <- function(hglm_out) {
  return(hglm_out$coef)

}

#' @export
vcov.hglm <- function(hglm_out) {
  warning("To be implemented.")

}

#' @export
print.hglm <- function(hglm_out, ...) {
  cat("Output of hiperglm")

}
