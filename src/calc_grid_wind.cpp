#include <Rcpp.h>
#include <math.h>
#include <testthat.h>
using namespace Rcpp;

//' Calculate distance between two latitude / longitude points
//'
//' @param tclat A numeric value with the latitude of the tropical cyclone's center
//'   in radians
//' @param tclon A numeric value with the longitude of the tropical cyclone's center
//'   in radians
//' @param glat A numeric value with the latitude of the grid point in radians
//' @param glon A numeric value with the longitude of the grid point in radians
//' @param Rearth A numeric value with the radius of the earth in kilometers. This
//'   value defaults to 6,371 km, which is the median radius of the earth.
//' @return A numeric value with the distance (in kilometers) from the tropical
//'   cyclone center to the grid point.
// [[Rcpp::export]]
double calc_distance(double tclat, double tclon,
                 double glat, double glon,
                 double Rearth =  6371) {

  double delta_L = tclon - glon;
  double delta_tclat = tclat - glat;

  double hav_L = pow(sin(delta_L / 2.0), 2.0);
  double hav_tclat = pow(sin(delta_tclat / 2.0), 2.0);

  double hav_gamma = hav_tclat + cos(tclat) * cos(glat) * hav_L;

  double gamma = 2.0 * asin(sqrt(hav_gamma));

  double dist = Rearth * gamma;

  return dist;
}

context("Check C++ calc_distance function") {
  test_that("Floyd to Dare Co distance is correct") {
    double floyd_lat = 33.7 * M_PI / 180.0;
    double floyd_lon = -78.0 * M_PI / 180.0;
    double dare_lat = 35.90756 * M_PI / 180.0;
    double dare_lon = -75.67488 * M_PI / 180.0;

    double calculated_distance = calc_distance(floyd_lat, floyd_lon,
                                               dare_lat, dare_lon);

    expect_true(round(calculated_distance) == 325);
  }

  test_that("Floyd to Miami-Dade Co distance is correct") {
    double floyd_lat = 33.7 * M_PI / 180.0;
    double floyd_lon = -78.0 * M_PI / 180.0;
    double miami_lat = 25.77456 * M_PI / 180.0;
    double miami_lon = -80.29889 * M_PI / 180.0;

    double calculated_distance = calc_distance(floyd_lat, floyd_lon,
                                               miami_lat, miami_lon);

    expect_true(round(calculated_distance) == 909);
  }

  test_that("Distance across the prime meridian is correct") {
    double ophelia_lat_1 = 65.60 * M_PI / 180.0;
    double ophelia_lon_1 = -1.0 * M_PI / 180.0;
    double ophelia_lat_2 = 66.60 * M_PI / 180.0;
    double ophelia_lon_2 = 1.90 * M_PI / 180.0;

    double calculated_distance = calc_distance(ophelia_lat_1, ophelia_lon_1,
                                               ophelia_lat_2, ophelia_lon_2);

    expect_true(round(calculated_distance) == 172);
  }

  test_that("Distance across the international date line is correct") {
    double harold_lat_1 = -20.11 * M_PI / 180.0;
    double harold_lon_1 = 179.70 * M_PI / 180.0;
    double harold_lat_2 = -20.60 * M_PI / 180.0;
    double harold_lon_2 = -178.1 * M_PI / 180.0;

    double calculated_distance = calc_distance(harold_lat_1, harold_lon_1,
                                               harold_lat_2, harold_lon_2);

    expect_true(round(calculated_distance) == 236);
  }

  test_that("Distance across the international date line is correct using IBTrACS conventions") {
    double harold_lat_1 = -20.11 * M_PI / 180.0;
    double harold_lon_1 = 179.70 * M_PI / 180.0;
    double harold_lat_2 = -20.60 * M_PI / 180.0;
    double harold_lon_2 = 181.90 * M_PI / 180.0;

    double calculated_distance = calc_distance(harold_lat_1, harold_lon_1,
                                               harold_lat_2, harold_lon_2);

    expect_true(round(calculated_distance) == 236);
  }
}

// Calculate equation 1a from Willoughby
double will1a(double vmax_gl, double r,
             double Rmax, double n) {
  double Vi = vmax_gl * pow((r / Rmax), n);

  return Vi;
}

// Calculate equation 4 from Willoughby
double will4(double vmax_gl, double A, double r, double Rmax, double X1,
            double X2 = 25.0){

  double Vo = vmax_gl * ((1 - A) * exp((Rmax - r) / X1) +
                        A * exp((Rmax - r) / X2));

  return Vo;
}

// Calculate equation 2 from Willoughby
double will2(double r, double R1, double R2) {

  double w;
  double xi = (r - R1) / (R2 - R1);

  if(xi <= 0){
    w = 0.0;
  } else if(xi >= 1){
    w = 1.0;
  } else{
    w = 126.0 * pow(xi, 5.0) - 420.0 * pow(xi, 6.0) +
      540.0 * pow(xi, 7.0) - 315.0 * pow(xi, 8.0) +
      70.0 * pow(xi, 9.0);
  }

  return w;
}

//' Calculate gradient wind speed using equation 1 from Willoughby
//'
//' @param cdist A numeric value with the distance between the grid point and the
//'   center of the storm (in km)
//' @param Rmax A numeric value with the radius at which max winds occur
//'   (in km from the center of the storm)
//' @param R1 A numeric value with the lower boundary of the transition zone (in
//'   km from the storm center)
//' @param R2 A numeric value with the upper boundary of the transition zone (in
//'   km from the storm center)
//' @param vmax_gl A numeric value with the maximum gradient level 1-minute
//'   sustained wind, in (m/s)
//' @param A A numeric value as a parameter for the Willoughby model
//' @param r A numeric value with the radius from the storm center to the grid
//'   point (in km)
//' @param Rmax A numeric value with the radius at which the maximum wind occurs
//'   (in km)
//' @param X1 A numeric value as a parameter for the Willoughby model
//' @param X2 A numeric value as a parameter for the Willoughby model, set to 25
//'   (Willoughby, Darling, and Rahn 2006)
//' @return wind_gl_aa A numeric value ...
//' @export
// [[Rcpp::export]]
double will1new(double cdist, double Rmax, double R1,
                double R2, double vmax_gl, double n,
                double A, double X1, double X2 = 25){

  double wind_gl_aa;
  double Vi, Vo, w;

  if(cdist < R1){
    Vi = will1a(vmax_gl, cdist, Rmax, n);
    wind_gl_aa = Vi;
  } else if(cdist > R2){
    Vo = will4(vmax_gl, A, cdist, Rmax, X1,  X2);
    wind_gl_aa = Vo;
  } else {
    Vi = will1a(vmax_gl, cdist, Rmax, n);
    Vo = will4(vmax_gl, A, cdist, Rmax, X1,  X2);
    w = will2(cdist, R1, R2);
    // Willoughby equation 1b
    wind_gl_aa = Vi * (1 - w) + Vo * w;
  }

  if(wind_gl_aa < 0){
    wind_gl_aa = 0;
  }

  return wind_gl_aa;
}

/*** R
# Check function

data("floyd_tracks")
floyd_tracks <- create_full_track(floyd_tracks)
with_wind <- add_wind_radii(floyd_tracks)

test <- will1new(cdist = test_data, Rmax = with_wind[1,]$Rmax, R1 = with_wind[1,]$R1, R2 = with_wind[1,]$R2,
                    vmax_gl = with_wind[1,]$vmax_gl, n = with_wind[1,]$n, A = with_wind[1,]$A,
                    X1 = with_wind[1,]$X1)



*/

//' Calculate bearing from one lat/long to another for a single point
//'
//' @param tclat A numeric value with the latitude of the tropical cyclone's center
//'   in radians
//' @param tclon A numeric value with the longitude of the tropical cyclone's center
//'   in radians
//' @param glat A numeric value with the latitude of the grid point in radians
//' @param glon A numeric value with the longitude of the grid point in radians
//' @return A numeric value with the bearing from the storm's center to the grid point
//'   in polar coordinates
//' @export
// [[Rcpp::export]]
double calc_bearing_single(double tclat, double tclon,
                           double glat, double glon) {

  double S = cos(glat) * sin(glon - tclon);
  double C = cos(tclat) * sin(glat) - sin(tclat) *
    cos(glat) * cos(glon - tclon);

  double theta_rad = atan2(S, C);

  double theta = (theta_rad * 180.0 / M_PI);

  // The previous result is in a coordinate system where north is 0 degrees,
  // east is 90 degrees, and so on. Convert to a coordinate system where 0 degrees
  // is due east, 90 degrees is due north, and so on.
  theta = 90 - theta;

  theta = fmod(theta + 360.0, 360.0); // restrict to be between 0 and 360 degrees

  return theta;
}

context("Check C++ calc_bearing function") {
  test_that("Single bearing calculation correct for North Atlantic") {
    double laura_lat_1 = 29.10 * M_PI / 180.0;
    double laura_lon_1 = -93.15 * M_PI / 180.0;
    double laura_lat_2 = 29.80 * M_PI / 180.0;
    double laura_lon_2 = -93.30 * M_PI / 180.0;

    double bearing = calc_bearing_single(laura_lat_1, laura_lon_1,
                                         laura_lat_2, laura_lon_2);

    expect_true(round(bearing) == 101);
  }

  test_that("Single bearing calculation correct for Eastern Pacific") {
    double douglas_lat_1 = 14.60 * M_PI / 180.0;
    double douglas_lon_1 = -138.00 * M_PI / 180.0;
    double douglas_lat_2 = 14.95 * M_PI / 180.0;
    double douglas_lon_2 = -138.74 * M_PI / 180.0;

    double bearing = calc_bearing_single(douglas_lat_1, douglas_lon_1,
                                         douglas_lat_2, douglas_lon_2);

    expect_true(round(bearing) == 154);
  }

  test_that("Single bearing calculation correct for Western Pacific"){
    double haishen_lat_1 = 34.31 * M_PI / 180.0;
    double haishen_lon_1 = 128.97 * M_PI / 180.0;
    double haishen_lat_2 = 35.50 * M_PI / 180.0;
    double haishen_lon_2 = 128.90 * M_PI / 180.0;

    double bearing = calc_bearing_single(haishen_lat_1, haishen_lon_1,
                                         haishen_lat_2, haishen_lon_2);

    expect_true(round(bearing) == 93);
  }

  test_that("Single bearing calculation correct for Northern Indian basin storm"){
    double amphan_lat_1 = 19.20 * M_PI / 180.0;
    double amphan_lon_1 = 87.40 * M_PI / 180.0;
    double amphan_lat_2 = 19.79 * M_PI / 180.0;
    double amphan_lon_2 = 87.66 * M_PI / 180.0;

    double bearing = calc_bearing_single(amphan_lat_1, amphan_lon_1,
                                         amphan_lat_2, amphan_lon_2);

    expect_true(round(bearing) == 67);
  }

  test_that("Single bearing calculation correct for Southern Indian basin storm") {
    double belna_lat_1 = -12.40 * M_PI / 180.0;
    double belna_lon_1 = 46.50 * M_PI / 180.0;
    double belna_lat_2 = -12.63 * M_PI / 180.0;
    double belna_lon_2 = 46.41 * M_PI / 180.0;

    double bearing = calc_bearing_single(belna_lat_1, belna_lon_1,
                                         belna_lat_2, belna_lon_2);

    expect_true(round(bearing) == 249);
  }

  test_that("Single bearing calculation correct for Southern Pacific") {
    double harold_lat_1 = -12.70 * M_PI / 180.0;
    double harold_lon_1 = 163.00 * M_PI / 180.0;
    double harold_lat_2 = -13.71 * M_PI / 180.0;
    double harold_lon_2 = 163.41 * M_PI / 180.0;

    double bearing = calc_bearing_single(harold_lat_1, harold_lon_1,
                                         harold_lat_2, harold_lon_2);

    expect_true(round(bearing) == 292);
  }

  test_that("Single bearing calculation correct for Southern Atlantic") {
    double not_named_lat_1 = -29.00 * M_PI / 180.0;
    double not_named_lon_1 = -49.60 * M_PI / 180.0;
    double not_named_lat_2 = -28.77 * M_PI / 180.0;
    double not_named_lon_2 = -49.93 * M_PI / 180.0;

    double bearing = calc_bearing_single(not_named_lat_1, not_named_lon_1,
                                         not_named_lat_2, not_named_lon_2);

    expect_true(round(bearing) == 142);
  }

}

// Calculate gradient wind direction at a point
double calc_gwd(double tclat, double tclon, double glat, double glon) {

  double gwd;

  // calculate the gradient wind direction (gwd) at the
  // grid point
  double chead = calc_bearing_single(tclat, tclon, glat, glon);

  // Cyclonic winds will be perpendicular to the bearing
  // from the storm to the grid point. In the Northern
  // Hemisphere, they'll be counterclockwise, so add 90 degrees
  // in polar coordinates. In the Southern Hemisphere, they'll
  // be clockwise, so subtract 90 degrees in polar coordinates.
  if(tclat > 0){
    gwd = chead + 90;
  } else {
    gwd = chead - 90;
  }

  gwd = fmod(gwd + 360.0, 360.0); // restrict to be between 0 and 360 degrees

  return gwd;
}

// Calculate symmetrical surface wind from gradient wind
double gradient_to_surface_new(double wind_gl_aa, double cdist) {
  double wind_sfc_sym, reduction_factor;

  if(cdist <= 100){
    reduction_factor = 0.9;
  } else if (cdist >= 700) {
    reduction_factor = 0.75;
  } else{
    reduction_factor = 0.90 - (cdist - 100.0) * (0.15 / 600.0);
  }

  // Since all counties are over land, reduction factor should
  // be 20% lower than if it were over water
  reduction_factor = reduction_factor * 0.80;

  wind_sfc_sym = wind_gl_aa * reduction_factor;

  return wind_sfc_sym;
}

//' Add inflow to direction of surface winds
//' @param gwd A numeric value with the gradient wind direction in degrees
//' @param cdist A numeric value with the radius from the storm's center to the
//' grid point in kilometers
//' @param Rmax A numeric value with the radius at which maximum winds occur in kilometers
//' @param tclat A numeric value with latitude in radians
//' @return swd A numeric value with the surface wind direction in degrees
//' @export
// [[Rcpp::export]]
double add_inflow(double gwd, double cdist, double Rmax, double tclat) {
  double inflow_angle, swd;

  // Calculate inflow angle over water based on radius of location from storm
  // center in comparison to radius of maximum winds (Phadke et al. 2003)
  if (cdist < Rmax){
    inflow_angle = 10.0 + (1.0 + (cdist / Rmax));
  } else if (Rmax <= cdist && cdist < 1.2 * Rmax) {
    inflow_angle = 20.0 + 25.0 * ((cdist / Rmax) - 1);
  } else {
    inflow_angle = 25.0;
  }

  // Add 20 degrees to inflow angle since location is over land, not water
  double overland_inflow_angle = inflow_angle + 20.0;

  // Add an adjustment for northern versus southern hemisphere
  double hemisphere_adj;
  if (tclat > 0.0) {
    hemisphere_adj = 1.0;
  } else {
    hemisphere_adj = -1.0;
  }

  swd = gwd + hemisphere_adj * overland_inflow_angle;
  swd = fmod(swd + 360.0, 360.0);

  return swd;
}

/*** R
#check function
gwd_test <- 20
cdist_test <- 135
Rmax_test <- 100
tc_location <- c(-29.1, -93.15) * pi / 180

add_inflow(gwd_test,cdist_test, Rmax_test, tc_location[1])

*/


//' Add in forward speed of the storm
//' @param wind_sfc_sym A numeric value with the estimated symmetric surface
//' wind speed at the grid point, in meters / second
//' @param tcspd_u A numeric value with the u-component of the tropical cyclone
//' speed in meters / second
//' @param tcspd_v A numeric value with the v-component of the tropical cyclone
//' speed in meters / second
//' @param swd A numeric value with the surface wind direction in degrees
//' @param cdist A numeric value with the radius from the storm's center to the
//' grid point in kilometers
//' @param Rmax A numeric value with the radius at which maximum winds occur in kilometers
//' @return windspeed A numeric value with the asymmetric surface windspeed at the
//' location, in meters/second
//' @export
// [[Rcpp::export]]
double add_forward_speed(double wind_sfc_sym, double tcspd_u, double tcspd_v, double swd,
                         double cdist, double Rmax) {
  // Calculate u- and v-components of surface wind speed
  double wind_sfc_sym_u = wind_sfc_sym * cos((swd * M_PI / 180.0));
  double wind_sfc_sym_v = wind_sfc_sym * sin((swd * M_PI / 180.0));

  // Add back in component from forward motion of the storm
  double correction_factor = (Rmax * cdist) / (pow(Rmax, 2.0) + pow(cdist, 2.0));

  // Add tangential and forward speed components and calculate
  // magnitude of this total wind
  double wind_sfc_u = wind_sfc_sym_u + correction_factor * tcspd_u;
  double wind_sfc_v = wind_sfc_sym_v + correction_factor * tcspd_v;
  double windspeed = sqrt(pow(wind_sfc_u, 2.0) + pow(wind_sfc_v, 2.0));

  // Reset any negative values to 0
  if (windspeed < 0) {
    windspeed = 0;
  }

  return windspeed;
}
/*** R
wind <- 35
tcu_test <- 16
tcv_test <- -2
swd <- 40
cdist_test <- 55
Rmax <- 70
add_forward_speed(wind, tcu_test, tcv_test, swd, cdist_test, Rmax)

#Test 1
#correction_factor = 0.4858
#wind_u = 3.83
#wind_v = 3.214

#Test 2
#correction_factor = 0.4858
#wind_u = 26.812
#wind_v = 22.498
*/

//' @export
// [[Rcpp::export]]
NumericVector calc_grid_wind_cpp(double glat, double glon, double max_dist,
    NumericVector tclat, NumericVector tclon,
    NumericVector Rmax, NumericVector R1, NumericVector R2, NumericVector vmax_gl,
    NumericVector n, NumericVector A, NumericVector X1,
    NumericVector tcspd_u, NumericVector tcspd_v) {

  int size_data = tclat.size();
  double cdist, wind_gl_aa, gwd, wind_sfc_sym, swd;
  NumericVector windspeed(size_data);

  // Convert everything from degrees to radians
  double glat_rad = glat * M_PI / 180.0;
  double glon_rad = glon * M_PI / 180.0;
  NumericVector tclat_rad = tclat * M_PI / 180.0;
  NumericVector tclon_rad = tclon * M_PI / 180.0;

  for(int i = 0; i < size_data; i++){
    // Calculate the distance between the storm's center at each time and the grid point
    cdist = calc_distance(tclat_rad[i], tclon_rad[i], glat_rad, glon_rad);
    // Assume all storm-associated wind beyond a certain distance is 0
    if(cdist > max_dist){
      windspeed[i] = 0;
    } else {
      // Calculate gradient-level wind at the point
      wind_gl_aa = will1new(cdist, Rmax[i], R1[i], R2[i], vmax_gl[i], n[i], A[i], X1[i]);
      // Calculate the gradient wind direction at the point
      gwd = calc_gwd(tclat_rad[i], tclon_rad[i], glat_rad, glon_rad);
      // Calculate symmetrical surface wind at the point
      wind_sfc_sym = gradient_to_surface_new(wind_gl_aa, cdist);
      // Add inflow
      swd = add_inflow(gwd, cdist, Rmax[i], tclat[i]);
      // Add forward speed of storm
      windspeed[i] = add_forward_speed(wind_sfc_sym, tcspd_u[i], tcspd_v[i], swd, cdist, Rmax[i]);
    }
  }

  return windspeed;
}

/*** R
old_version <- calc_grid_wind(grid_point = tibble(glat = 29.97270, glon = -90.05284),
                   with_wind_radii = tibble(date = lubridate::ymd_hms(c("1999-09-13 06:00:00", "1999-09-13 06:15:00")),
                                            tclat = c(29.6, 29.0), tclon = c(-90.0, -90.1),
                                            Rmax = c(22.1, 22.1), R1 = c(4.42, 4.43), R2 = c(29.4, 29.4), vmax_gl = c(73.6, 73.6),
                                            n = c(1.38, 1.38), A = c(0.279, 0.279), X1 = c(213.0, 213.0),
                                            tcspd_u = c(-6.23, -6.25), tcspd_v = c(73.6, 73.6)))

stormwindmodel:::calc_grid_wind_cpp(glat = 29.97270, glon = -90.05284, max_dist =  2222.4,
                   tclat = c(29.6, 29.0, NA), tclon = c(-90.0, -90.1, NA),
                   Rmax = c(22.1, 22.1, NA), R1 = c(4.42, 4.43, NA), R2 = c(29.4, 29.4, NA), vmax_gl = c(73.6, 73.6, NA),
                   n = c(1.38, 1.38, NA), A = c(0.279, 0.279, NA), X1 = c(213.0, 213.0, NA),
                   tcspd_u = c(-6.23, -6.25, NA), tcspd_v = c(73.6, 73.6, NA))


katrina_full_track <- create_full_track(hurr_track = stormwindmodel::katrina_tracks, tint = 0.25)
katrina_wind_radii <- add_wind_radii(full_track = katrina_full_track) %>%
  slice(210:220)
miami_dade_location <- county_points %>%
  filter(gridid == "12086")

stormwindmodel:::calc_grid_wind_cpp(glat = miami_dade_location$glat, glon = miami_dade_location$glon, max_dist =  2222.4,
                                    tclat = katrina_wind_radii$tclat, tclon = katrina_wind_radii$tclon,
                                    Rmax = katrina_wind_radii$Rmax, R1 = katrina_wind_radii$R1,
                                    R2 = katrina_wind_radii$R2, vmax_gl = katrina_wind_radii$vmax_gl,
                                    n = katrina_wind_radii$n, A = katrina_wind_radii$A, X1 = katrina_wind_radii$X1,
                                    tcspd_u = katrina_wind_radii$tcspd_u, tcspd_v = katrina_wind_radii$tcspd_v)
*/


// [[Rcpp::export]]
List calc_grid_wind_cpp2(NumericVector glat, NumericVector glon, double max_dist,
                                 NumericVector tclat, NumericVector tclon,
                                 NumericVector Rmax, NumericVector R1, NumericVector R2, NumericVector vmax_gl,
                                 NumericVector n, NumericVector A, NumericVector X1,
                                 NumericVector tcspd_u, NumericVector tcspd_v) {

  int size_lat_lon = tclat.size();
  int size_glat_glon = glat.size();
  double cdist, wind_gl_aa, gwd, wind_sfc_sym, swd;
  NumericMatrix windspeed(size_lat_lon, size_glat_glon);
  NumericMatrix distance_from_storm(size_lat_lon, size_glat_glon);
  NumericMatrix surface_wind_direction(size_lat_lon, size_glat_glon);

  // Convert everything from degrees to radians
  NumericVector glat_rad = glat * M_PI / 180.0;
  NumericVector glon_rad = glon * M_PI / 180.0;
  NumericVector tclat_rad = tclat * M_PI / 180.0;
  NumericVector tclon_rad = tclon * M_PI / 180.0;

  for(int j = 0; j < size_glat_glon; j++){
    for(int i = 0; i < size_lat_lon; i++){
      // Calculate the distance between the storm's center at each time and the grid point
      cdist = calc_distance(tclat_rad[i], tclon_rad[i], glat_rad[j], glon_rad[j]);
      distance_from_storm(i, j) = cdist;
      // Assume all storm-associated wind beyond a certain distance is 0
      if(cdist > max_dist){
        windspeed(i, j) = 0;
        surface_wind_direction(i, j) = NA_REAL;
      } else {
        // Calculate gradient-level wind at the point
        wind_gl_aa = will1new(cdist, Rmax[i], R1[i], R2[i], vmax_gl[i], n[i], A[i], X1[i]);
        // Calculate the gradient wind direction at the point
        gwd = calc_gwd(tclat_rad[i], tclon_rad[i], glat_rad[j], glon_rad[j]);
        // Calculate symmetrical surface wind at the point
        wind_sfc_sym = gradient_to_surface_new(wind_gl_aa, cdist);
        // Add inflow
        swd = add_inflow(gwd, cdist, Rmax[i], tclat[i]);
        surface_wind_direction(i, j) = swd;
        // Add forward speed of storm
        windspeed(i, j) = add_forward_speed(wind_sfc_sym, tcspd_u[i], tcspd_v[i], swd, cdist, Rmax[i]);
      }
    }
  }

  //return windspeed;
  return List::create(Named("vmax_sust") = windspeed,
                      Named("distance_from_storm") = distance_from_storm,
                      Named("surface_wind_direction") = surface_wind_direction);
}

/*** R
old_version <- calc_grid_wind(grid_point = tibble(glat = 29.97270, glon = -90.05284),
                              with_wind_radii = tibble(date = lubridate::ymd_hms(c("1999-09-13 06:00:00", "1999-09-13 06:15:00")),
                                                       tclat = c(29.6, 29.0), tclon = c(-90.0, -90.1),
                                                       Rmax = c(22.1, 22.1), R1 = c(4.42, 4.43), R2 = c(29.4, 29.4), vmax_gl = c(73.6, 73.6),
                                                       n = c(1.38, 1.38), A = c(0.279, 0.279), X1 = c(213.0, 213.0),
                                                       tcspd_u = c(-6.23, -6.25), tcspd_v = c(73.6, 73.6)))

stormwindmodel:::calc_grid_wind_cpp2(glat = 29.97270, glon = -90.05284, max_dist =  2222.4,
                                    tclat = c(29.6, 29.0, NA), tclon = c(-90.0, -90.1, NA),
                                    Rmax = c(22.1, 22.1, NA), R1 = c(4.42, 4.43, NA), R2 = c(29.4, 29.4, NA), vmax_gl = c(73.6, 73.6, NA),
                                    n = c(1.38, 1.38, NA), A = c(0.279, 0.279, NA), X1 = c(213.0, 213.0, NA),
                                    tcspd_u = c(-6.23, -6.25, NA), tcspd_v = c(73.6, 73.6, NA))

stormwindmodel:::calc_grid_wind_cpp2(glat = c(29.97270, 30, 29), glon = c(-90.05284, -90, -89), max_dist =  2222.4,
                                     tclat = c(29.6, 29.0, NA), tclon = c(-90.0, -90.1, NA),
                                     Rmax = c(22.1, 22.1, NA), R1 = c(4.42, 4.43, NA), R2 = c(29.4, 29.4, NA), vmax_gl = c(73.6, 73.6, NA),
                                     n = c(1.38, 1.38, NA), A = c(0.279, 0.279, NA), X1 = c(213.0, 213.0, NA),
                                     tcspd_u = c(-6.23, -6.25, NA), tcspd_v = c(73.6, 73.6, NA))



katrina_full_track <- create_full_track(hurr_track = stormwindmodel::katrina_tracks, tint = 0.25)
katrina_wind_radii <- add_wind_radii(full_track = katrina_full_track)
florida_location <- county_points %>%
  filter(stringr::str_sub(gridid, 1, 2) == "12")

a <- stormwindmodel:::calc_grid_wind_cpp2(glat = florida_location$glat, glon = florida_location$glon,
                                     max_dist =  2222.4,
                                    tclat = katrina_wind_radii$tclat, tclon = katrina_wind_radii$tclon,
                                    Rmax = katrina_wind_radii$Rmax, R1 = katrina_wind_radii$R1,
                                    R2 = katrina_wind_radii$R2, vmax_gl = katrina_wind_radii$vmax_gl,
                                    n = katrina_wind_radii$n, A = katrina_wind_radii$A, X1 = katrina_wind_radii$X1,
                                    tcspd_u = katrina_wind_radii$tcspd_u, tcspd_v = katrina_wind_radii$tcspd_v)
*/

