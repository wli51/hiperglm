#include "hiperglm_types.h"

//' @export
//' @useDynLib hiperglm
//' @importFrom Rcpp sourceCpp
// [[Rcpp::export]]
VectorXd rcpp_qr(Map<MatrixXd> A, Map<VectorXd> b) {
  if (A.rows() != b.size()) {
    Rcpp::stop("Incompatible matrix-vector dimensions.");
  }
  Eigen::HouseholderQR<Eigen::MatrixXd> qr(A);
  return qr.solve(b);
}
