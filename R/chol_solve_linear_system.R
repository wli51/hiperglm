chol_solve_linear_system <- function(A, b) {

  L = t(chol(A))
  as.numeric(backsolve(t(L), forwardsolve(L, b)))

}
