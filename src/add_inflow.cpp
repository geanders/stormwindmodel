#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' Add inflow angle (C++ version)
//'
//' This function adds an inflow angle to the angle of the wind direction.
//' It calculates an inflow angle as a function of the distance from the
//' storm center to a location (Phadke et al. 2003), and then adds 20 degrees to
//' this inflow angle to account for the location being over land rather than
//' over water.
//'
//' @param gwd A numeric vector giving direction of gradient wind at a location,
//'    in degrees. Due east is 0 degrees, due north 90 degrees, etc.
//' @param cdist A numeric vector giving radius (in kilometers) from the storm
//'    center to a location.
//' @inheritParams will3_right
//'
//' @return Numeric vector with the gradient wind direction (in degrees),
//'    adjusted with an inflow angle appropriate for being over land and for the
//'    location's distance from the storm's center.
//'
//' @details
//'
//' This function uses equations 11a-c from Phadke et al. (2003).
//'
//' @references
//'
//' Phadke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
//'    tropical cyclone winds and waves for emergency management. Ocean
//'    Engineering 30(4):553-578.
//'
//' @examples
//' add_inflow(gwd = 160, cdist = 100, Rmax = 20)
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector add_inflow_Cpp(Rcpp::NumericVector gwd,
                                   Rcpp::NumericVector cdist,
                                   Rcpp::NumericVector Rmax){
  for (int i = 0; i < gwd.size(); i++){
    if(Rcpp::NumericVector :: is_na(gwd[i]) ||
       Rcpp::NumericVector :: is_na(cdist[i]) ||
       Rcpp::NumericVector :: is_na(Rmax[i])) {
      return -1; //Cannot return NA because the type is double
    }
  }
  // Calculate inflow angle over water based on radius of location from storm
  // center in comparison to radius of maximum winds (Phadke et al. 2003)
  Rcpp::NumericVector inflow_angle(gwd.size()),
                                   overland_inflow_angle(gwd.size()),
                                   gwd_with_inflow(gwd.size());

  for (int k = 0; k < gwd.size(); k++){
    if (cdist[k] < Rmax[k]){
      inflow_angle[k] = 10 + (1 + (cdist[k] / Rmax[k]));
    } else if (cdist[k] >= Rmax[k] && cdist[k] < 1.2 * Rmax[k]){
      inflow_angle[k] = 20 + 25 * ((cdist[k] / Rmax[k]) - 1);
    } else {
      inflow_angle[k] = 25;
    }

    // Add 20 degrees to inflow angle since location is over land, not water
    overland_inflow_angle[k] = inflow_angle[k] + 20;

    // Add inflow angle to gradient wind direction
    gwd_with_inflow[k] = fmod((gwd[k] + overland_inflow_angle[k]), 360);
  }
  return gwd_with_inflow;
}

/*** R
add_inflow(160, 100, 20)
add_inflow_Cpp(160, 100, 20)
library(microbenchmark)
microbenchmark(add_inflow(160, 100, 20),
               add_inflow_Cpp(160, 100, 20))
# C++ function is about 1.5 times faster
*/
