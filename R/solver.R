#'
solve.linear.system <- function(A, b, solver=c("LU", "Chol", "QR", "RcppQR")) {
  x <- NULL
  if (solver == "LU") {
    x <- as.numeric(solve(A, b))
  } else if (solver == "Chol") {
    L <- t(chol(A))
    x <- backsolve(t(L), forwardsolve(L, b))
  } else if (solver == "QR") {
    x <- solve.qr(qr(A), b)
  } else if (solver == "RcppQR") {
    x <- rcpp_qr(A, b)
  }
  return(matrix(x, ncol = 1))
}
