#include <Rcpp.h>
#include <math.h>
#include "degrees_to_radians.h"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
Rcpp::NumericVector radians_to_degrees_Cpp1(Rcpp::NumericVector radians){
  Rcpp::NumericVector degrees(radians.size());
  for (int i = 0; i < radians.size(); i++){
    degrees[i] = radians[i] * 180 / M_PI;
  }
  return degrees;
}

//' Calculate bearing from one location to another (C++ version)
//'
//' Calculates the bearing of a second location, as seen from
//' the first location, based on latitude and longitude coordinates for both
//' locations.
//'
//' @inheritParams latlon_to_km
//'
//' @return A numeric vector giving the direction of the second location from the first location,
//'    in degrees. A direction of 0 degrees indicates the second location is
//'    due east of the first, 90 degrees indicates the second location is due
//'    north of the first, etc.
//'
//' @details This function uses the following equations to calculate the bearing
//'    from one latitude-longitude pair to another:
//'
//'    \deqn{S = cos(\phi_2) * sin(L_1 - L_2)}{
//'    S = cos(\phi2) * sin(L1 - L1)}
//'
//'    \deqn{C = cos(\phi_1) * sin(\phi_2) - sin(\phi_1) * cos(\phi_2) * cos(L_1 - L_2)}{
//'    C = cos(\phi1) * sin(\phi2) - sin(\phi1) * cos(\phi2) * cos(L1 - L2)}
//'
//'    \deqn{\theta = atan2(S, C) * \frac{180}{\pi} + 90}
//'
//'    where:
//'    \itemize{
//'      \item{\eqn{\phi_1}{\phi1}: Latitude of first location, in radians}
//'      \item{\eqn{L_1}{L1}: Longitude of first location, in radians}
//'      \item{\eqn{\phi_2}{\phi2}: Latitude of second location, in radians}
//'      \item{\eqn{L_2}{L2}: Longitude of second location, in radians}
//'      \item{\eqn{S, C}: Intermediary results}
//'      \item{\eqn{\theta}: Direction of the storm movement, in degrees}
//'    }
//'
//'    In cases where this equation results in values below 0 degrees or above
//'    360 degrees, the function applies modular arithmetic to bring the value
//'    back within the 0--360 degree range.
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector calc_bearing_Cpp(Rcpp::NumericVector tclat_1,
                                     Rcpp::NumericVector tclon_1,
                                     Rcpp::NumericVector tclat_2,
                                     Rcpp::NumericVector tclon_2){

  tclat_1 = degrees_to_radians_Cpp(tclat_1);
  tclon_1 = degrees_to_radians_Cpp(-tclon_1);
  tclat_2 = degrees_to_radians_Cpp(tclat_2);
  tclon_2 = degrees_to_radians_Cpp(-tclon_2);

  Rcpp::NumericVector S(tclat_1.size()),
                      C(tclat_1.size()),
                      theta_rad(tclat_1.size()),
                      theta(tclat_1.size());

  for (int i = 0; i < tclat_1.size(); i++ ){

    S[i] = cos(tclat_2[i]) * sin(tclon_1[i] - tclon_2[i]);
    C[i] = cos(tclat_1[i]) * sin(tclat_2[i]) - sin(tclat_1[i]) *
      cos(tclat_2[i]) * cos(tclon_1[i] - tclon_2[i]);

    theta_rad[i] = atan2(S[i], C[i]);
  }
  theta = radians_to_degrees_Cpp1(theta_rad) + 90;

  for (int i = 0; i < tclat_1.size(); i++ ){
    theta[i] = fmod(theta[i],360); // restrict to be between 0 and 360 degrees
  }

  return theta;
}

/*** R
calc_bearing(7, 4, 3, 15)
calc_bearing_Cpp(7, 4, 3, 15)

microbenchmark::microbenchmark(calc_bearing(7, 4, 3, 15),
                               calc_bearing_Cpp(7, 4, 3, 15))
# C++ function around 25 times faster
*/
