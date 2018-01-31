#include <Rcpp.h>
#include <cmath>
#include "degrees_to_radians.h"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' Adds forward speed component to modeled surface wind (C++ version)
//'
//' Adds the storm's forward speed component back into the estimated
//' surface wind speed at a grid point location.
//'
//' @param wind_sfc_sym A numeric vector with maximum 10-meter 1-minute
//'    sustained wind with motion asymmetry removed (m / s).
//' @param tcspd_u A numeric vector with the tropical cyclone speed, u-component
//'    (m / s).
//' @param tcspd_v A numeric vector with the tropical cyclone speed, v-component
//'    (m / s).
//' @param swd A numeric vector with surface wind direction (degree).
//' @inheritParams add_inflow
//' @inheritParams will3_right
//'
//' @return A numeric vector giving asymmeric surface windspeed (m / s)
//'
//' @details
//'
//' This function uses equation 12 from Phadke et al. (2003).
//'
//' @references
//'
//' Phadke AC, Martino CD, Cheung KF, and Houston SH. 2003. Modeling of
//'    tropical cyclone winds and waves for emergency management. Ocean
//'    Engineering 30(4):553-578.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector add_forward_speed_Cpp(Rcpp::NumericVector wind_sfc_sym,
                                          Rcpp::NumericVector tcspd_u,
                                          Rcpp::NumericVector tcspd_v,
                                          Rcpp::NumericVector swd,
                                          Rcpp::NumericVector cdist,
                                          Rcpp::NumericVector Rmax){
  Rcpp::NumericVector wind_sfc_sym_u(swd.size()),
                      wind_sfc_sym_v(swd.size()),
                      correction_factor(swd.size()),
                      wind_sfc_u(swd.size()),
                      wind_sfc_v(swd.size()),
                      wind_sfc(swd.size());
  swd = degrees_to_radians_Cpp(swd);
  // Calculate u- and v-components of surface wind speed
  for (int i=0; i < swd.size(); i++) {
    wind_sfc_sym_u[i] = wind_sfc_sym[i] * cos(swd[i]);
    wind_sfc_sym_v[i] = wind_sfc_sym[i] * sin(swd[i]);

  // Add back in component from forward motion of the storm
    correction_factor[i] = (Rmax[i] * cdist[i]) /
      (pow(Rmax[i],2) + pow(cdist[i],2));

  // Add tangential and forward speed components and calculate
  // magnitude of this total wind
    wind_sfc_u[i] = wind_sfc_sym_u[i] + correction_factor[i] * tcspd_u[i];
    wind_sfc_v[i] = wind_sfc_sym_v[i] + correction_factor[i] * tcspd_v[i];
    wind_sfc[i] = sqrt(pow(wind_sfc_u[i],2) + pow(wind_sfc_v[i],2));

  // Reset any negative values to 0
    if (wind_sfc[i] < 0) {
        wind_sfc[i] = 0;
    }
  }
  return wind_sfc;
}

/*** R
add_forward_speed(1, 1, 1, 1, 1, 1)
add_forward_speed_Cpp(1, 1, 1, 1, 1, 1)
microbenchmark::microbenchmark(add_forward_speed(1, 1, 1, 1, 1, 1),
                               add_forward_speed_Cpp(1, 1, 1, 1, 1, 1))
# C++ function around 25 times faster
*/
