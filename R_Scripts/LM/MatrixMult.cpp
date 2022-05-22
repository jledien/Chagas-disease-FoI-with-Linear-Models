// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]

#include <RcppArmadillo.h>
#include <RcppEigen.h>
#include <Rcpp.h>

// [[Rcpp::export]]
SEXP armaMatMult(arma::mat A, arma::mat B){
  arma::mat C = A * B;
  
  return Rcpp::wrap(C);
}

// [[Rcpp::export]]
SEXP eigenMatMult(Eigen::MatrixXd A, Eigen::MatrixXd B){
  Eigen::MatrixXd C = A * B;
  
  return Rcpp::wrap(C);
}

// [[Rcpp::export]]
SEXP eigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B){
  Eigen::MatrixXd C = A * B;
  
  return Rcpp::wrap(C);
}

// [[Rcpp::export]]
SEXP EigenMapMatMult2(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B, int nrows, int ncols){
  Eigen::MatrixXd C(nrows,ncols);
  for (int i = 0; i < nrows; i++){
    C.row(i) = A.row(i)*B;
    
  }
  
  
  return Rcpp::wrap(C);
}

// [[Rcpp::export]]
SEXP MedEigenMapMatMult(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B, int nrows, int ncols){
  Rcpp::NumericVector v(nrows);
  Rcpp::NumericVector x(ncols);
  for (int i = 0; i < nrows; i++){
    x = A.row(i)*B;
    v(i) = Rcpp::median(x);
  }
  
  return Rcpp::wrap(v);
}

// [[Rcpp::export]]
SEXP mv_mult(arma::mat lhs, arma::vec rhs){
  arma::vec C = lhs * rhs;
  
  return Rcpp::wrap(C);
}
