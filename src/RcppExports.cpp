// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "hiperglm_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_qr
VectorXd rcpp_qr(Map<MatrixXd> A, Map<VectorXd> b);
RcppExport SEXP _hiperglm_rcpp_qr(SEXP ASEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Map<MatrixXd> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Map<VectorXd> >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_qr(A, b));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_hiperglm_rcpp_qr", (DL_FUNC) &_hiperglm_rcpp_qr, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_hiperglm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}