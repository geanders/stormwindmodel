#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' Convert from degrees to radians
//'
//' Convert an angle from degrees to radians (C++ version)
//'
//' @param degrees A numeric vector with measurements in degrees.
//'
//' @return A numeric vector with measurement in radians.
// [[Rcpp::export]]
Rcpp::NumericVector degrees_to_radians_Cpp(Rcpp::NumericVector degrees){
  Rcpp::NumericVector radians(degrees.size());
  for (int i = 0; i < degrees.size(); i++) {
    radians[i] = degrees[i] * M_PI / 180;
  }
  return radians;
}
