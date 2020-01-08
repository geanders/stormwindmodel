#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector degrees_to_radiansC(Rcpp::NumericVector degrees){
  int n = degrees.size();
  Rcpp::NumericVector radians(n);

  for(int i = 1; i < n; ++i) {
    radians[i] = degrees[i] * 3.14159265358979 / 180.;
  }

  return radians;
}

// [[Rcpp::export]]
Rcpp::NumericVector radians_to_degreesC(Rcpp::NumericVector radians){
  int n = radians.size();
  Rcpp::NumericVector degrees(n);

  for(int i = 1; i < n; ++i) {
    degrees[i] = radians[i] * 180. / 3.14159265358979;
  }

  return degrees;
}


// Test degrees_to_radians and radians_to_degrees against original R versions
// and benchmark both.
//

/*** R
degrees_to_radiansR <- function(degrees){
  degrees * pi / 180
}

radians_to_degreesR <- function(radians){
  radians * 180 / pi
}

x <- c(0, 5, 10, 15, 90, 180, 360, 400, -90)
y <- c(0, pi, 1.2 * pi, -0.9 * pi, 0.3, 0.001)

all.equal(degrees_to_radiansR(x), degrees_to_radiansC(x))
all.equal(radians_to_degreesR(y), radians_to_degreesC(y))

bench::mark(
  degrees_to_radiansR(x),
  degrees_to_radiansC(x)
)

bench::mark(
  radians_to_degreesR(x),
  radians_to_degreesC(x)
)
*/
