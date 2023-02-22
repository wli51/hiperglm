# solves Ax = b for x
chol_solve_linear_system <- function(A, b) {

  L = t(chol(A))
  backsolve(t(L), forwardsolve(L, b))

}
