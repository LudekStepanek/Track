// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
// downloaded from: https://github.com/platypus1989/PathMatch/blob/master/src/utility_functions.cpp
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
//' Columnwise standard deviations.
//'
//' @param X a matrix.
//' @param norm_type normalization type, integer input, default 0.
//' @return columnwise standard deviations.
//' @examples
//'
//'colSds(matrix(1:4,nrow=2))
//' @export
// [[Rcpp::export]]
arma::rowvec colSds(const arma::mat X, const int norm_type = 0) {
  return stddev(X, norm_type, 0);
}

//' Rowwise standard deviations.
//'
//' @param X a matrix.
//' @param norm_type normalization type, integer input, default 0.
//' @return rowwise standard deviations.
//' @examples
//'
//'rowSds(matrix(1:4,nrow=2))
//' @export
// [[Rcpp::export]]
arma::colvec rowSds(const arma::mat X, const int norm_type = 0) {
  return stddev(X, norm_type, 1);
}

//' Distance from a point to a line linked by two other points C++ version.
//'
//' @param a a numeric vetor with length 2.
//' @param b a numeric vetor with length 2.
//' @param c a numeric vetor with length 2.
//' @return real-valued Eucleadian distance from point a to the line linking b and c.
//' @examples
//'
//'dist2dCPP(c(1,1),c(0,0),c(0,1))
//' @export
// [[Rcpp::export]]
double dist2dCPP(arma::rowvec a,
                 arma::rowvec b,
                 arma::rowvec c) {
  arma::rowvec v1 = b-c;
  arma::rowvec v2 = a-b;
  if (sum(abs(v1)) == 0){
    double d = sqrt(sum(v2%v2));
    return(d);
  } else {
    double detm = v1(0)*v2(1)-v1(1)*v2(0);
    double d = std::abs(detm)/sqrt(sum(v1%v1));
    return(d);
  }
}

//' Ramer–Douglas–Peucker algorithm C++ version.
//'
//' @param points point matrix with 2 columns (x and y coordinate like).
//' @param epsilon cut-off distance for the algorithm.
//' @return RDP points.
//' @examples
//'
//'RDPCPP(cbind(c(1:4),c(1:4)),0.5)
//'
//'# pick a trip from driver Alexander
//'sample_trip <- as.matrix(subset(tele_data,{trip_id==tele_data$trip_id[1]})[,c("lat","long")])
//'RDPCPP(sample_trip,0.001)
//'
//'
//' @export
// [[Rcpp::export]]
arma::mat RDPCPP(arma::mat points,
                 double epsilon){
  double dmax = 0;
  int index = 0;
  int n = points.n_rows;
  for (int i = 1; i < n-1; i++){
    double d = dist2dCPP(points.row(i),points.row(0),points.row(n-1));
    if (d > dmax){
      index = i;
      dmax = d;
    }
  }
  if (dmax > epsilon){
    arma::mat result1 = RDPCPP(points.rows(0,index),epsilon);
    arma::mat result2 = RDPCPP(points.rows(index,n-1),epsilon);
    arma::mat results(result1.n_rows+result2.n_rows-1,2);
    results = join_cols(result1.rows(0,result1.n_rows-2),result2);
    return(results);
  } else {
    arma::mat results(2,2);
    results = join_cols(points.row(0),points.row(n-1));
    return(results);
  }
  
}
