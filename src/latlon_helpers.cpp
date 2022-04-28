#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//' Convert from radians to degrees
//'
//' Convert an angle from radians to degrees
//'
//' @param radians A numeric vector with measurements in radians.
//'
//' @return A numeric vector with the measurement in degrees.

// [[Rcpp::export]]
NumericVector radians_to_degrees(NumericVector radians) {
  return  radians * 180.0 / M_PI;
}

// Test

/*** R
stormwindmodel:::radians_to_degrees(c(-0.2, 0, 0.2, 4.3))
*/

//' Convert from degrees to radians
//'
//' Convert an angle from degrees to radians
//'
//' @param degrees A numeric vector with measurements in degrees.
//'
//' @return A numeric vector with measurement in radians.
//' @export

// [[Rcpp::export]]
NumericVector degrees_to_radians(NumericVector degrees) {
  return degrees * M_PI / 180.0;
}


// Test

/*** R
stormwindmodel:::degrees_to_radians(c(-45, 0, 45, 90, 375))
*/

//' Calculate distance between two locations
//'
//' This function takes latitudes and longitudes for two locations and
//' calculates the distance (in meters) between the locations using the
//' Haversine method.
//'
//' @param tclat_1 A numeric vector giving latitude of the first location
//'    (degrees)
//' @param tclon_1 A numeric vector giving longitude of the first location
//'    (degrees). This value should be expressed as a positive value for Western
//'    hemisphere longitudes.
//' @param tclat_2 A numeric vector giving latitude of the second location
//'    (degrees)
//' @param tclon_2 A numeric vector giving longitude of the second location
//'    (degrees). This value should be expressed as a positive value for Western
//'    hemisphere longitudes.
//' @param Rearth Radius of the earth (km). Default is 6378.14 km.
//'
//' @return A vector with the distance between the two locations, in kilometers.
//'
//' @details This function uses the Haversine method with great circle distance
//'    to calculate this distance. It is applying the following equations to
//'    determine distance (in kilometers) between two latitude-longitude pairs:
//'    \deqn{hav(\gamma) = hav(\phi_1 - \phi_2) + cos(\phi_1)*cos(\phi_2)*hav(L_1 - L_2)}{
//'    hav(\gamma) = hav(\phi1 - \phi2) + cos(\phi1)*cos(\phi2)*hav(L1 - L2)}
//'    where:
//'    \itemize{
//'      \item{\eqn{\phi_1}{\phi1}: Latitude of first location, in radians}
//'      \item{\eqn{\phi_2}{\phi2}: Latitude of second location, in radians}
//'      \item{\eqn{L_1}{L1}: Longitude of first location, in radians}
//'      \item{\eqn{L_2}{L2}: Longitude of second location, in radians}
//'      \item{\eqn{hav(\gamma)}: The haversine function,
//'         \eqn{hav(\gamma) = sin^2 \left(\frac{\gamma}{2}\right)}{
//'         hav(\gamma) = sin^2 (\gamma / 2)}}
//'      \item{\eqn{R_earth}{Rearth}: Radius of the earth, here assumed to be 6378.14 kilometers}
//'      \item{\eqn{D}}: Distance between the two locations, in kilometers
//'    }
//'
//'
//' @export

// [[Rcpp::export]]
NumericVector latlon_to_km(NumericVector tclat_1, NumericVector tclon_1,
                           NumericVector tclat_2, NumericVector tclon_2,
                           float Rearth =  6378.14) {
  tclat_1 = degrees_to_radians(tclat_1);
  tclon_1 = degrees_to_radians(tclon_1);
  tclat_2 = degrees_to_radians(tclat_2);
  tclon_2 = degrees_to_radians(tclon_2);

  NumericVector delta_L = tclon_1 - tclon_2;
  NumericVector  delta_tclat = tclat_1 - tclat_2;

  NumericVector hav_L = pow(sin(delta_L / 2.0), 2.0);
  NumericVector hav_tclat = pow(sin(delta_tclat / 2.0), 2.0);

  NumericVector hav_gamma = hav_tclat + cos(tclat_1) * cos(tclat_2) * hav_L;

  NumericVector gamma = 2.0 * asin(sqrt(hav_gamma));

  NumericVector dist = Rearth * gamma;

  return dist;
}

// Test

/*** R
stormwindmodel:::latlon_to_km(c(45, 43), c(-80, -81), c(48, 49), c(-82, -80))
*/

//' Calculate storm's forward speed
//'
//' This storm takes two storm locations and their observations times and
//' calculates the average speed of the storm between the two observations.
//'
//' @param time_1 A date-time vector giving the time of the first observation.
//' @param time_2 A date-time vector giving the time of the second observation.
//' @param tclat_1 A numeric vector giving latitude of the first location
//'    (degrees)
//' @param tclon_1 A numeric vector giving longitude of the first location
//'    (degrees). This value should be expressed as a positive value for Western
//'    hemisphere longitudes.
//' @param tclat_2 A numeric vector giving latitude of the second location
//'    (degrees)
//' @param tclon_2 A numeric vector giving longitude of the second location
//'    (degrees). This value should be expressed as a positive value for Western
//'    hemisphere longitudes.
//'
//' @return A numeric vector with the average forward speed of the storm between
//'    the two observations, in meters per second.
//'
//' @export
// [[Rcpp::export]]
NumericVector calc_forward_speed(NumericVector tclat_1, NumericVector tclon_1,
                                 Rcpp::DatetimeVector time_1,
                                 NumericVector tclat_2, NumericVector tclon_2,
                                 Rcpp::DatetimeVector time_2) {
  NumericVector dist = latlon_to_km(tclat_1, tclon_1, tclat_2, tclon_2) * 1000.0;
  NumericVector time = time_2 - time_1;
  NumericVector tcspd = dist / time;
  return tcspd;
}

// Test

/*** R
stormwindmodel:::calc_forward_speed(c(24, 24.3),
                                    c(-80.1, -80.3),
                                    lubridate::ymd_hm(c("2020-01-01 00:00",
                                                        "2020-01-01 00:00")),
                                    c(24.0, 24.0),
                                    c(-80.1, -80.1),
                                    lubridate::ymd_hm(c("2020-01-01 00:01",
                                                        "2020-01-01 00:02")))
*/

//' Calculate bearing from one location to another
//'
//' Calculates the bearing of a second location, as seen from
//' the first location, based on latitude and longitude coordinates for both
//' locations.
//'
//' @param tclat_1 A numeric vector giving latitude of the first location
//'    (degrees)
//' @param tclon_1 A numeric vector giving longitude of the first location
//'    (degrees). This value should be expressed as a positive value for Western
//'    hemisphere longitudes.
//' @param tclat_2 A numeric vector giving latitude of the second location
//'    (degrees)
//' @param tclon_2 A numeric vector giving longitude of the second location
//'    (degrees). This value should be expressed as a positive value for Western
//'    hemisphere longitudes.
//'
//' @return A numeric vector giving the direction of the second location from the first location,
//'    in degrees. A direction of 0 degrees indicates the second location is
//'    due east of the first, 90 degrees indicates the second location is due
//'    north of the first, etc (i.e., polar, rather than meteorological, coordinate system).
//'
//' @details This function uses the following equations to calculate the bearing
//'    from one latitude-longitude pair to another:
//'
//'    \deqn{S = cos(\phi_2) * sin(L_2 - L_1)}{
//'    S = cos(\phi2) * sin(L2 - L1)}
//'
//'    \deqn{C = cos(\phi_1) * sin(\phi_2) - sin(\phi_1) * cos(\phi_2) * cos(L_2 - L_1)}{
//'    C = cos(\phi1) * sin(\phi2) - sin(\phi1) * cos(\phi2) * cos(L2 - L1)}
//'
//'    \deqn{\theta = 90 - atan2(S, C) * \frac{180}{\pi}}
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
NumericVector calc_bearing(NumericVector tclat_1, NumericVector tclon_1,
                          NumericVector tclat_2, NumericVector tclon_2) {
  tclat_1 = degrees_to_radians(tclat_1);
  tclon_1 = degrees_to_radians(tclon_1);
  tclat_2 = degrees_to_radians(tclat_2);
  tclon_2 = degrees_to_radians(tclon_2);

  NumericVector S = cos(tclat_2) * sin(tclon_2 - tclon_1);
  NumericVector C = cos(tclat_1) * sin(tclat_2) - sin(tclat_1) *
    cos(tclat_2) * cos(tclon_2 - tclon_1);

  int n = tclat_1.size();
  NumericVector theta_rad(n);

  for(int i = 0; i < n; i++){
    theta_rad[i] = atan2(S[i], C[i]);
  }

  NumericVector theta1 = 90 - radians_to_degrees(theta_rad);
  NumericVector theta(n);

  for(int i = 0; i < n; i++){
    theta[i] = fmod(theta1[i] + 360.0, 360.0); // restrict to be between 0 and 360 degrees
  }

  return theta;
}

// Test

/*** R
stormwindmodel:::calc_bearing(c(29.1, 24.3),
                              c(-93.15, -80.3),
                              c(29.8, 24.0),
                              c(-93.3, -80.1))
***/
