#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' Calculate distance between two locations (C++ version)
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
//' @export
// [[Rcpp::export]]
NumericVector degrees_to_radians_Cpp2(NumericVector degrees){
  NumericVector radians(degrees.size());
  for (int i=0; i < degrees.size(); i++) {
    radians[i] = degrees[i] * M_PI / 180;
  }
  return radians;
}

// [[Rcpp::export]]
NumericVector latlon_to_km_Cpp(NumericVector tclat_1, NumericVector tclon_1,
                               NumericVector tclat_2, NumericVector tclon_2,
                               NumericVector Rearth = 6378.14){
  NumericVector delta_L(tclat_1.size()), delta_tclat(tclat_1.size()), hav_L(tclat_1.size()),
  hav_tclat(tclat_1.size()), hav_gamma(tclat_1.size()), gamma(tclat_1.size()),
  dist(tclat_1.size());

  tclat_1 = degrees_to_radians_Cpp2(tclat_1);
  tclon_1 = degrees_to_radians_Cpp2(tclon_1);
  tclat_2 = degrees_to_radians_Cpp2(tclat_2);
  tclon_2 = degrees_to_radians_Cpp2(tclon_2);

  for (int i = 0; i < tclat_1.size(); i++){

    delta_L[i] = tclon_1[i] - tclon_2[i];
    delta_tclat[i] = tclat_1[i] - tclat_2[i];

    hav_L[i] = pow(sin(delta_L[i] / 2), 2);
    hav_tclat[i] = pow(sin(delta_tclat[i] / 2), 2);

    hav_gamma[i] = hav_tclat[i] + cos(tclat_1[i]) * cos(tclat_2[i]) * hav_L[i];
    gamma[i] = 2 * asin(sqrt(hav_gamma[i]));

    dist[i] = Rearth[i] * gamma[i];
  }
  return dist;
}



/*** R
latlon_to_km(1,2,1,1,6378.14)
latlon_to_km_Cpp(1,2,1,1,6378.14)
microbenchmark::microbenchmark(latlon_to_km(1,2,1,1,6378.14),
                               latlon_to_km_Cpp(1,2,1,1,6378.14))
*/
