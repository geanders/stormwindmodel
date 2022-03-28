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

//' Willoughby et al. (2006), Equation 1(a)
//'
//' This is the equation that is used for the wind profile inside the  eye (more
//' specifically, from the center of the storm to R1, where the transition region
//' for the model starts). It assumes that the wind increases from the center of
//' the storm to the radius of maximum wind in proportion to a power of the radius
//' from the center.
//'
//' @param vmax_gl A numeric value giving the maximum gradient-level 1-minute
//'   sustained wind for the tropical cyclone, in meters per second. This is the
//'   value given for the storm as a whole at the time point (for example, in
//'   tracking data).
//' @param r A numeric value of radius from the storm center to the point you are
//'   measuring, in kilometers
//' @param Rmax A numeric value with the distance from the center of the storm to
//'   the storm's radius of maximum wind, in kilometers
//' @param n A numeric value giving the power by which the wind is assumed to
//'   increase with radius within the center of the storm (i.e., from the center
//'   to the radius of maximum wind)
//'
//' @return A numeric value with the modeled wind at distance r from the center of
//'   the storm, in meters per second.
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
double will1a(double vmax_gl, double r,
             double Rmax, double n) {
  double Vi = vmax_gl * pow((r / Rmax), n);

  return Vi;
}

context("Check C++ will1a function") {
  test_that("C++ will1a works with a power value (n) that isn't an integer") {
    double storm_max_wind = 45.3;
    double rad_to_model = 16.1;
    double rad_max_wind = 30.2;
    double power_for_eq = 0.8;

    double wind_at_r = will1a(storm_max_wind, rad_to_model, rad_max_wind, power_for_eq);
    expect_true(round(wind_at_r) == 27);
  }

  test_that("C++ will1a works at low maximum storm winds"){
    double storm_max_wind = 10.0; // Lowest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 50.2;
    double rad_max_wind = 79.8;
    double power_for_eq = 0.4; // Approx. value of n for a wind of 10.0 in Fig. 11

    double wind_at_r = will1a(storm_max_wind, rad_to_model, rad_max_wind, power_for_eq);
    expect_true(round(wind_at_r) == 8);
  }

  test_that("C++ will1a works at high maximum storm winds"){
    double storm_max_wind = 80.0; // Highest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 8.3;
    double rad_max_wind = 25.1;
    double power_for_eq = 1.4; // Approx. value of n for a wind of 80.0 in Fig. 11

    double wind_at_r = will1a(storm_max_wind, rad_to_model, rad_max_wind, power_for_eq);
    expect_true(round(wind_at_r) == 17);
  }
}

//' Calculate equation 4 from Willoughby
//'
//' This is the equation that is used for the wind profile in outer regions of the
//' storm (more specifically, outside of the second radius defining the transition
//' region, R2).
//'
//' @param vmax_gl A numeric value giving the maximum gradient-level 1-minute
//'   sustained wind for the tropical cyclone, in meters per second. This is the
//'   value given for the storm as a whole at the time point (for example, in
//'   tracking data).
//' @param A A numeric value with the fitted contribution of the faster exponential
//'   to the profile for Willoughby
//' @param r A numeric value giving the distance from the center of the storm to
//'   the location where you would like to model the local wind, in kilometers
//' @param Rmax A numeric value with the distance from the center of the storm to
//'   the storm's radius of maximum wind, in kilometers
//' @param X1 A numeric value with the fitted slower decay length for Willoughby
//' @param X2 A numeric value with the fixed rapid decay length for Willoughby
//'
//' @return A numeric value with the modeled wind at distance r from the center of
//'   the storm, in meters per second.
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
double will4(double vmax_gl, double A, double r, double Rmax, double X1,
            double X2 = 25.0){

  double Vo = vmax_gl * ((1 - A) * exp((Rmax - r) / X1) +
                        A * exp((Rmax - r) / X2));

  return Vo;
}

context("Check C++ will4 function") {
  test_that("C++ will4 works further from the eye with low wind"){
    double storm_max_wind = 10.0; // Lowest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 120.3;
    double rad_max_wind = 79.8;
    double A_for_test = 0.0; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 325.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will4(storm_max_wind, A_for_test, rad_to_model, rad_max_wind, X1_for_test);
    expect_true(round(wind_at_r) == 9);
  }

  test_that("C++ will4 works further from the eye at high maximum storm winds"){
    double storm_max_wind = 80.0; // Highest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 42.1;
    double rad_max_wind = 25.1;
    double A_for_test = 0.3; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 200.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will4(storm_max_wind, A_for_test, rad_to_model, rad_max_wind, X1_for_test);
    expect_true(round(wind_at_r) == 64);
  }
}

//' Calculate equation 2 from Willoughby
//'
//' @param r A numeric value giving the distance from the center of the storm to
//'   the location where you would like to model the local wind, in kilometers
//' @param R1 A numeric value with the lower boundary of the transition zone (in
//'   km from the storm center)
//' @param R2 A numeric value with the upper boundary of the transition zone (in
//'   km from the storm center)
//'
//' @return A numeric value between 0 and 1 that gives the amount to weight Vi
//'   versus Vo when estimating wind within the transition zone.
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
// [[Rcpp::export]]
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

context("Check C++ will2 function") {
  test_that("C++ will2 function returns zero inside R1") {
    double r = 5.0;
    double R1 = 10.0;
    double R2 = 40.0;

    double w = will2(r, R1, R2);
    expect_true(w == 0.0);
  }

  test_that("C++ will2 function returns one outside R2") {
    double r = 50.0;
    double R1 = 10.0;
    double R2 = 40.0;

    double w = will2(r, R1, R2);
    expect_true(w == 1.0);
  }

  test_that("C++ will2 function works inside the transition region (R1 to R2)") {
    double r = 25.0;
    double R1 = 10.0;
    double R2 = 40.0;

    double w = will2(r, R1, R2);
    expect_true(w == 0.5);
  }
}

/*** R
library(tidyverse)
library(purrr)

## Based on Willoughby, w should smoothly ramp up from 0 to 1 between R1 and
## R2. Check this using R1 and R2 estimates from Fig 1 in Willoughby. The
## result should look like the bottom plot in Fig 1 of Willoughby.
R1 <- 10
R2 <- 40

df <- tibble(r = 0:100,
             w = map_dbl(r, stormwindmodel:::will2, R1 = R1, R2 = R2))
df %>%
  ggplot(aes(x = r, y = w)) +
  geom_point() +
  geom_line()
*/


//' Model wind speed at a grid point for a storm track observation
//'
//' Models the gradient wind speed at a certain radius from
//' a storm's center. To do this, it uses different equations and subfunctions
//' depending on how large the radius is (see details). This function requires,
//' as inputs, Willoughby wind model parameters calculated using the
//' \code{\link{add_wind_radii}} function.
//'
//' @param cdist A numeric value with the distance between the grid point and the
//'   center of the storm (in km)
//' @param R1 A numeric value with the lower boundary of the transition zone (in
//'   km from the storm center)
//' @param R2 A numeric value with the upper boundary of the transition zone (in
//'   km from the storm center)
//' @param vmax_gl A numeric value with the maximum gradient level 1-minute
//'   sustained wind, in (m/s)
//' @param A A numeric value that is a parameter between 0 and 1 for the Willoughby
//'   model
//' @param Rmax A numeric value with the radius at which the maximum wind occurs
//'   (in km)
//' @param X1 A numeric value that is a parameter for the Willoughby model
//' @param X2 A numeric value as a parameter for the Willoughby model, set to 25
//'   (Willoughby, Darling, and Rahn 2006)
//'
//' @return Returns a numeric vector with gradient wind speed at a radius of
//'    \eqn{r} from the storm's center, in meters per second.
//'
//' @details If \eqn{r \le R_1}{r \le R1}, this function is calculating the equation:
//'
//'    \deqn{V(r) = V_i = V_{max} \left( \frac{r}{R_{max}} \right)^n}{
//'    V(r) = Vi = vmax_gl (r / Rmax)^n}
//'
//'    where:
//'    \itemize{
//'      \item{\eqn{V(r)}: Maximum sustained gradient wind speed at a radius of
//'        \eqn{r} from the storm's center}
//'      \item{\eqn{r}: Radius from the storm center, in kilometers}
//'      \item{\eqn{V_{max,G}}{vmax_gl}: Maximum sustained gradient wind speed of the
//'        storm, in meters per second}
//'      \item{\eqn{R_1}{R1}: A parameter for the Willoughby wind model (radius to
//'        start of transition region)}
//'      \item{\eqn{R_{max}}{Rmax}: Radius (in kilometers) to highest winds}
//'      \item{\eqn{n}: A parameter for the Willoughby wind model}
//'    }
//'
//'    If \eqn{R_2 < r}{R2 \le r}, this function is calculating
//'      the equation:
//'
//'      \deqn{V(r) = V_o = V_{max}\left[(1 - A) e^\frac{R_{max} - r}{X_1} + A e^\frac{R_{max} - r}{X_2}\right]}{
//'      V(r) = Vo = vmax_gl[(1 - A) e^((Rmax - r) / X1) + A e^((Rmax - r) / X_2)]}
//'
//'    where:
//'    \itemize{
//'      \item{\eqn{V(r)}: Maximum sustained gradient wind speed at a radius of
//'        \eqn{r} kilometers from the storm's center}
//'      \item{\eqn{r}: Radius from the storm center, in kilometers}
//'      \item{\eqn{V_{max,G}}{vmax_gl}: Maximum sustained gradient wind speed of the
//'        storm, in meters per second}
//'      \item{\eqn{R_{max}}{Rmax}: Radius (in kilometers) to highest winds}
//'      \item{\eqn{A}, \eqn{X_1}{X1}, \eqn{X_2}{X2}: Parameters for the
//'         Willoughby wind model}
//'    }
//'
//'    If \eqn{R_1 < r \le R_2}{R1 < r \le R2}, this function uses
//'      the equations:
//'
//'      \deqn{\xi = \frac{r - R_1}{R_2 - R_1}}{
//'      \xi = (r - R1) / (R2 - R1)}
//'
//'      and, if \eqn{0 \le \xi < \le 1} (otherwise, \eqn{w = 0}):
//'
//'      \deqn{w = 126 \xi^5 - 420 \xi^6 + 540 \xi^7- 315 \xi^8 + 70 \xi^9}
//'
//'      and then:
//'
//'      \deqn{V(r) = V_i (1 - w) + V_o w, (R_1 \le r \le R_2)}{
//'      V(r) = Vi (1 - w) + Vo w}
//'
//'    where, for this series of equations:
//'    \itemize{
//'      \item{\eqn{V(r)}: Maximum sustained gradient wind speed at a radius of
//'        \eqn{r} kilometers from the storm's center}
//'      \item{\eqn{r}: Radius from the storm center, in kilometers}
//'      \item{\eqn{w}: Weighting variable}
//'      \item{\eqn{R_1}{R1}, \eqn{R_2}{R2}: Parameters for the Willoughby wind model}
//'    }
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
// [[Rcpp::export]]
double will1(double cdist, double Rmax, double R1,
                double R2, double vmax_gl, double n,
                double A, double X1, double X2 = 25.0){

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

context("Check that C++ will1 function works") {
  test_that("C++ will1 works very close to the eye with low wind"){
    double storm_max_wind = 10.0; // Lowest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 50.2;
    double rad_max_wind = 79.8;
    double rad_start_transition = 70.0;
    double rad_end_transition = 85.0;
    double power_for_eq = 0.4; // Approx. value of n for a wind of 10.0 in Fig. 11
    double A_for_test = 0.0; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 325.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 8);
  }

  test_that("C++ will1 works very close to the eye at high maximum storm winds"){
    double storm_max_wind = 80.0; // Highest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 8.3;
    double rad_max_wind = 25.1;
    double rad_start_transition = 20.0;
    double rad_end_transition = 35.0;
    double power_for_eq = 1.4; // Approx. value of n for a wind of 80.0 in Fig. 11
    double A_for_test = 0.3; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 200.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 17);
  }

  test_that("C++ will1 works further from the eye with low wind"){
    double storm_max_wind = 10.0; // Lowest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 120.3;
    double rad_max_wind = 79.8;
    double rad_start_transition = 70.0;
    double rad_end_transition = 85.0;
    double power_for_eq = 0.4; // Approx. value of n for a wind of 10.0 in Fig. 11
    double A_for_test = 0.0; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 325.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 9);
  }

  test_that("C++ will1 works further from the eye at high maximum storm winds"){
    double storm_max_wind = 80.0; // Highest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 42.1;
    double rad_max_wind = 25.1;
    double rad_start_transition = 20.0;
    double rad_end_transition = 35.0;
    double power_for_eq = 1.4; // Approx. value of n for a wind of 80.0 in Fig. 11
    double A_for_test = 0.3; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 200.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 64);
  }

  test_that("C++ will1 works within the transition region with low wind"){
    double storm_max_wind = 10.0; // Lowest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 72;
    double rad_max_wind = 79.8;
    double rad_start_transition = 70.0;
    double rad_end_transition = 85.0;
    double power_for_eq = 0.4; // Approx. value of n for a wind of 10.0 in Fig. 11
    double A_for_test = 0.0; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 325.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 10);
  }

  test_that("C++ will1 works within the transition region at high maximum storm winds"){
    double storm_max_wind = 80.0; // Highest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 22.0;
    double rad_max_wind = 25.1;
    double rad_start_transition = 20.0;
    double rad_end_transition = 35.0;
    double power_for_eq = 1.4; // Approx. value of n for a wind of 80.0 in Fig. 11
    double A_for_test = 0.3; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 200.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 67);
  }

  test_that("C++ will1 works at Rmax with low wind"){
    double storm_max_wind = 10.0; // Lowest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 79.8;
    double rad_max_wind = 79.8;
    double rad_start_transition = 70.0;
    double rad_end_transition = 85.0;
    double power_for_eq = 0.4; // Approx. value of n for a wind of 10.0 in Fig. 11
    double A_for_test = 0.0; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 325.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 10);
  }

  test_that("C++ will1 works at Rmax at high maximum storm winds"){
    double storm_max_wind = 80.0; // Highest example wind given in Fig. 11 of Willoughby
    double rad_to_model = 25.1;
    double rad_max_wind = 25.1;
    double rad_start_transition = 20.0;
    double rad_end_transition = 35.0;
    double power_for_eq = 1.4; // Approx. value of n for a wind of 80.0 in Fig. 11
    double A_for_test = 0.3; // A reasonable value for A at this wind based on Fig. 11
    double X1_for_test = 200.0; // A reasonable value for X1 at this wind based on Fig. 11

    double wind_at_r = will1(rad_to_model, rad_max_wind, rad_start_transition,
                                rad_end_transition, storm_max_wind, power_for_eq,
                                A_for_test, X1_for_test);
    expect_true(round(wind_at_r) == 80);
  }
}

/*** R
library(purrr)
library(tidyverse)
library(ggplot2)

# Replicate Figure 2 in Willoughby
cdist <- 0:150
fig_2_data <- cdist %>%
  tibble(cdist = .,
         v_at_dist = map_dbl(., .f = will1,
                             Rmax = 24, R1 = 10, R2 = 35,
                             vmax_gl = 42, n = 0.9, A = 0.39,
                             X1 = 305, X2 = 25))
fig_2_data %>%
  ggplot(aes(x = cdist, y = v_at_dist)) +
  geom_line() +
  geom_point()

# Replicate Figure 9a in Willoughby
cdist <- 0:150
fig_2_data <- cdist %>%
  tibble(cdist = .,
         v_at_dist = map_dbl(., .f = will1,
                             Rmax = 23, R1 = 15, R2 = 15 + 25,
                             vmax_gl = 62, n = 1.2, A = 0.41,
                             X1 = 301, X2 = 25))
fig_2_data %>%
  ggplot(aes(x = cdist, y = v_at_dist)) +
  geom_line() +
  geom_point()
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
//'   in polar coordinates (i.e., due East is 0 degrees, due North is 90 degrees, etc.)
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

  test_that("Single bearing calculation correct for crossing prime meridian") {
    double ophelia_lat_1 = 65.60 * M_PI / 180.0;
    double ophelia_lon_1 = -1.0 * M_PI / 180.0;
    double ophelia_lat_2 = 66.60 * M_PI / 180.0;
    double ophelia_lon_2 = 1.90 * M_PI / 180.0;

    double bearing = calc_bearing_single(ophelia_lat_1, ophelia_lon_1,
                                         ophelia_lat_2, ophelia_lon_2);

    expect_true(round(bearing) == 42);
  }

    test_that("Single bearing calculation correct for crossing international date line") {
    double harold_lat_1 = -20.11 * M_PI / 180.0;
    double harold_lon_1 = 179.70 * M_PI / 180.0;
    double harold_lat_2 = -20.60 * M_PI / 180.0;
    double harold_lon_2 = -178.10 * M_PI / 180.0;

    double bearing = calc_bearing_single(harold_lat_1, harold_lon_1,
                                         harold_lat_2, harold_lon_2);

    expect_true(round(bearing) == 346);
  }

  test_that("Single bearing calculation correct for crossing international date line, IBTrACS convention") {
    double harold_lat_1 = -20.11 * M_PI / 180.0;
    double harold_lon_1 = 179.70 * M_PI / 180.0;
    double harold_lat_2 = -20.60 * M_PI / 180.0;
    double harold_lon_2 = 181.90 * M_PI / 180.0;

    double bearing = calc_bearing_single(harold_lat_1, harold_lon_1,
                                         harold_lat_2, harold_lon_2);

    expect_true(round(bearing) == 346);
  }

}

//' Calculate gradient wind direction at a point
//'
//' This function determines the bearing from the storm's center to a
//' grid point location and then calculates the direction of gradient
//' winds at that grid point. Gradient winds will be perpendicular to
//' the direction from the storm's center to the grid point. In the
//' Northern hemisphere, cyclonic winds are counterclockwise, so you
//' add 90 degrees to the bearing from the storm center to the grid point
//' when using polar coordinates. In the Southern hemisphere, cyclonic
//' winds are clockwise, so you subtract 90 degrees.
//'
//' @param tclat A numeric value with the latitude of the tropical cyclone's center
//'   in radians
//' @param tclon A numeric value with the longitude of the tropical cyclone's center
//'   in radians
//' @param glat A numeric value with the latitude of the grid point in radians
//' @param glon A numeric value with the longitude of the grid point in radians
//'
//' @return A numeric value with the the direction of gradient storm winds at a location,
//'   in polar coordinates (i.e., due East is 0 degrees, due North is 90 degrees, etc.)
//'
// [[Rcpp::export]]
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

context("Check C++ calc_gwd function") {
  test_that("Gradient wind calculation correct for Northern Hemisphere") {
    double laura_lat_1 = 29.10 * M_PI / 180.0;
    double laura_lon_1 = -93.15 * M_PI / 180.0;
    double laura_lat_2 = 29.80 * M_PI / 180.0;
    double laura_lon_2 = -93.30 * M_PI / 180.0;

    double bearing = calc_gwd(laura_lat_1, laura_lon_1,
                              laura_lat_2, laura_lon_2);

    expect_true(round(bearing) == 191);
  }

  test_that("Gradient wind calculation correct for Southern Hemisphere") {
    double belna_lat_1 = -12.40 * M_PI / 180.0;
    double belna_lon_1 = 46.50 * M_PI / 180.0;
    double belna_lat_2 = -12.63 * M_PI / 180.0;
    double belna_lon_2 = 46.41 * M_PI / 180.0;

    double bearing = calc_gwd(belna_lat_1, belna_lon_1,
                              belna_lat_2, belna_lon_2);

    expect_true(round(bearing) == 159);
  }

}

//' Calculate symmetrical surface wind from gradient wind
//'
//' Calculates the surface wind speed based on an estimated gradient
//' wind speed at a point and the radius from the storm center to
//' the grid point.
//'
//' @param wind_gl_aa The gradient-level wind at the location at which the wind
//'   is being modeled (in m/s)
//' @param cdist The distance from the center of the storm to the location at
//'   which the wind is being modeled (in kilometers)
//' @param glandsea A logical value that specifies whether the location at which
//'   the wind is being modeled is over land (TRUE) or over water (FALSE)
//'
//' @return A numeric value giving the surface-level wind at that location (before
//'   incorporating the role of forward motion of the storm) (in m/s)
//'
//' @details The reduction factor is based on Figure 3 of Knaff et al., 2003.
//' Over water, it is estimated to be 0.9 up to a radius of 100 km,
//' 0.75 for a radius of 700 km or more, and decrease linearly between
//' a radius of 100 km and 700 km. Points over land use a reduction
//' factor that is 20\% lower.
//'
//' @references
//'
//' Knaff JA, DeMaria M, Molenar DA, Sampson CR, and Seybold MG. 2011. An
//' automated, objective, multiple-satellite-platform tropical cyclone surface
//' wind speed analysis. Journal of Applied Meteorology and Climatology
//' 50(10):2149-2166
//'
// [[Rcpp::export]]
double gradient_to_surface(double wind_gl_aa, double cdist, bool glandsea) {
  double wind_sfc_sym, reduction_factor;

  if(cdist <= 100){
    reduction_factor = 0.9;
  } else if (cdist >= 700) {
    reduction_factor = 0.75;
  } else{
    reduction_factor = 0.90 - (cdist - 100.0) * (0.15 / 600.0);
  }

  // Reduction factor should be 20% lower over land
  // than over water
  if(glandsea) {
    reduction_factor = reduction_factor * 0.80;
  }

  wind_sfc_sym = wind_gl_aa * reduction_factor;

  return wind_sfc_sym;
}

context("Check C++ gradient_to_surface function") {
  test_that("Gradient-to-surface C++ function works close to the storm center over land"){
    double gradient_wind = 20.0;
    double r_to_model = 50.0;
    bool over_land = true;

    double calculated_surface_wind = gradient_to_surface(gradient_wind,
                                                             r_to_model,
                                                             over_land);
    expect_true(round(calculated_surface_wind) == 14);
  }

  test_that("Gradient-to-surface C++ function works close to the storm center over water"){
    double gradient_wind = 20.0;
    double r_to_model = 50.0;
    bool over_land = false;

    double calculated_surface_wind = gradient_to_surface(gradient_wind,
                                                             r_to_model,
                                                             over_land);
    expect_true(round(calculated_surface_wind) == 18);
  }

  test_that("Gradient-to-surface C++ function works far from the storm center over land"){
    double gradient_wind = 20.0;
    double r_to_model = 900.0;
    bool over_land = true;

    double calculated_surface_wind = gradient_to_surface(gradient_wind,
                                                             r_to_model,
                                                             over_land);
    expect_true(round(calculated_surface_wind) == 12);
  }

  test_that("Gradient-to-surface C++ function works far from the storm center over water"){
    double gradient_wind = 20.0;
    double r_to_model = 900.0;
    bool over_land = false;

    double calculated_surface_wind = gradient_to_surface(gradient_wind,
                                                             r_to_model,
                                                             over_land);
    expect_true(round(calculated_surface_wind) == 15);
  }

  test_that("Gradient-to-surface C++ function works at mid distances from the storm center over land"){
    double gradient_wind = 20.0;
    double r_to_model = 200.0;
    bool over_land = true;

    double calculated_surface_wind = gradient_to_surface(gradient_wind,
                                                             r_to_model,
                                                             over_land);
    expect_true(round(calculated_surface_wind) == 14);
  }

  test_that("Gradient-to-surface C++ function works at mid distances from the storm center over water"){
    double gradient_wind = 20.0;
    double r_to_model = 200.0;
    bool over_land = false;

    double calculated_surface_wind = gradient_to_surface(gradient_wind,
                                                             r_to_model,
                                                             over_land);
    expect_true(round(calculated_surface_wind) == 18);
  }
}

/*** R
library(purrr)
library(tidyverse)
library(ggplot2)

# Recreate Fig 3 from Knaff et al., 2011
tibble(cdist = rep(0:900, 2),
       over_land = rep(c(TRUE, FALSE), each = 901),
       reduction_factor = map2_dbl(.x = cdist, .y = over_land,
                                   .f = ~ stormwindmodel:::gradient_to_surface(1.0, .x, .y))) %>%
  ggplot(aes(x = cdist, y = reduction_factor, color = over_land, group = over_land)) +
  geom_line() +
  ylim(c(0.5, 1.0))
*/

//' Add inflow to direction of surface winds
//'
//' This function adds an inflow angle to the angle of the wind direction.
//' It calculates an inflow angle as a function of the distance from the
//' storm center to a location (Phadke et al. 2003). If the locations is over
//' land, it then adds 20 degrees to this inflow angle to account for the location
//' being over land rather than over water.
//'
//' @param gwd A numeric value with the gradient wind direction in polar degrees
//'   (e.g., due east is 0 degrees, due north is 90 degrees)
//' @param cdist The distance from the center of the storm to the location at
//'   which the wind is being modeled (in kilometers)
//' @param Rmax A numeric value with the radius at which maximum winds occur in kilometers
//' @param tclat A numeric value with latitude in radians
//' @param glandsea A logical value that specifies whether the location at which
//'   the wind is being modeled is over land (TRUE) or over water (FALSE)
//'
//' @return swd A numeric value with the surface wind direction in polar degrees
//'   (e.g., due east is 0, due north is 90)
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
// [[Rcpp::export]]
double add_inflow(double gwd, double cdist, double Rmax, double tclat, bool glandsea) {
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

  // Add 20 degrees to inflow angle if location is over land, not water
  if(glandsea) {
    inflow_angle = inflow_angle + 20.0;
  }

  // Add an adjustment for northern versus southern hemisphere
  double hemisphere_adj;
  if (tclat > 0.0) {
    hemisphere_adj = 1.0;
  } else {
    hemisphere_adj = -1.0;
  }

  swd = gwd + hemisphere_adj * inflow_angle;
  swd = fmod(swd + 360.0, 360.0);

  return swd;
}

context("Check C++ add_inflow function") {
  test_that("C++ add_inflow function works at the storm center over water in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 0.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 11);
  }

  test_that("C++ add_inflow function works within the eye over water in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 10.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 12);
  }

  test_that("C++ add_inflow function works at Rmax over water in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 20.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 20);
  }

  test_that("C++ add_inflow function works at 1.2 * Rmax over water in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 24.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 25);
  }

  test_that("C++ add_inflow function works at the storm center over land in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 0.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 31);
  }

  test_that("C++ add_inflow function works within the eye over land in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 10.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 32);
  }

  test_that("C++ add_inflow function works at Rmax over land in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 20.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 40);
  }

  test_that("C++ add_inflow function works at 1.2 * Rmax over land in the Northern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 24.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = 0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 45);
  }

  test_that("C++ add_inflow function works at the storm center over water in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 0.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 349);
  }

  test_that("C++ add_inflow function works within the eye over water in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 10.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 349);
  }

  test_that("C++ add_inflow function works at Rmax over water in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 20.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 340);
  }

  test_that("C++ add_inflow function works at 1.2 * Rmax over water in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 24.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = false;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 335);
  }

  test_that("C++ add_inflow function works at the storm center over land in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 0.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 329);
  }

  test_that("C++ add_inflow function works within the eye over land in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 10.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 329);
  }

  test_that("C++ add_inflow function works at Rmax over land in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 20.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 320);
  }

  test_that("C++ add_inflow function works at 1.2 * Rmax over land in the Southern Hemisphere") {
    double gradient_wind_direction = 0.0;
    double rad_to_model = 24.0;
    double rad_to_max_wind = 20.0;
    double latitude_to_model = -0.45;
    bool land_sea = true;

    double surface_wind_direction = add_inflow(gradient_wind_direction,
                                                   rad_to_model, rad_to_max_wind,
                                                   latitude_to_model, land_sea);

    expect_true(round(surface_wind_direction) == 315);
  }
}

/*** R
library(tidyverse)
library(purrr)

## Create a plot of the added inflow angle versus distance from storm center and
## radius to maximum wind for the Northern Hemisphere
Rmax <- 20
tibble(cdist = rep(0:50, 2),
       over_land = rep(c(TRUE, FALSE), each = 51),
       inflow_angle = map2_dbl(.x = cdist, .y = over_land,
                              .f = ~ stormwindmodel:::add_inflow(gwd = 0, cdist = .x,
                                                                     Rmax = Rmax, tclat = .45,
                                                                     glandsea = .y))) %>%
  ggplot(aes(x = cdist, y = inflow_angle, color = over_land, group = over_land)) +
  geom_point() +
  geom_vline(xintercept = Rmax, linetype = 2)

## Create a plot of the added inflow angle versus distance from storm center and
## radius to maximum wind for the Southern Hemisphere
Rmax <- 20
tibble(cdist = rep(0:50, 2),
       over_land = rep(c(TRUE, FALSE), each = 51),
       inflow_angle = map2_dbl(.x = cdist, .y = over_land,
                               .f = ~ stormwindmodel:::add_inflow(gwd = 0, cdist = .x,
                                                                      Rmax = Rmax, tclat = -0.45,
                                                                      glandsea = .y)) - 360) %>%
  ggplot(aes(x = cdist, y = inflow_angle, color = over_land, group = over_land)) +
  geom_point() +
  geom_vline(xintercept = Rmax, linetype = 2)
*/

//' Add in forward speed of the storm
//'
//' Adds the storm's forward speed component (i.e., motion asymmetry) back
//' into the estimated surface wind speed at a grid point location after
//' rotational winds have been modeled for the location.
//'
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
//'
//' @return windspeed A numeric value with the asymmetric surface windspeed at the
//' location, in meters/second
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
//'
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

  return windspeed;
}

context("Check that C++ add_forward_speed function works correctly") {
  test_that("C++ add_forward_speed works in the Northern Hemisphere near the eye on right of storm") {
    double symmetrical_surface_wind = 40.0;
    double forward_speed_u = 2.37;
    double forward_speed_v = 7.67;
    double surface_wind_direction = 100.0;
    double r_to_model = 10.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 42);
  }

  test_that("C++ add_forward_speed works in the Northern Hemisphere near the eye on left of storm") {
    double symmetrical_surface_wind = 40.0;
    double forward_speed_u = 2.37;
    double forward_speed_v = 7.67;
    double surface_wind_direction = 300.0;
    double r_to_model = 10.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 38);
  }

  test_that("C++ add_forward_speed works in the Northern Hemisphere near the radius of maximum wind on right of storm") {
    double symmetrical_surface_wind = 55.0;
    double forward_speed_u = 2.37;
    double forward_speed_v = 7.67;
    double surface_wind_direction = 100.0;
    double r_to_model = 25.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 59);
  }

  test_that("C++ add_forward_speed works in the Northern Hemisphere near the radius of maximum wind on left of storm") {
    double symmetrical_surface_wind = 55.0;
    double forward_speed_u = 2.37;
    double forward_speed_v = 7.67;
    double surface_wind_direction = 300.0;
    double r_to_model = 25.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 52);
  }

  test_that("C++ add_forward_speed works in the Northern Hemisphere far from the eye on right of storm") {
    double symmetrical_surface_wind = 8.0;
    double forward_speed_u = 2.37;
    double forward_speed_v = 7.67;
    double surface_wind_direction = 100.0;
    double r_to_model = 80.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 10);
  }

  test_that("C++ add_forward_speed works in the Northern Hemisphere far from the eye on left of storm") {
    double symmetrical_surface_wind = 8.0;
    double forward_speed_u = 2.37;
    double forward_speed_v = 7.67;
    double surface_wind_direction = 300.0;
    double r_to_model = 80.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 7);
  }

  test_that("C++ add_forward_speed works in the Southern Hemisphere near the eye on right of storm") {
    double symmetrical_surface_wind = 40.0;
    double forward_speed_u = -2.37;
    double forward_speed_v = -7.67;
    double surface_wind_direction = 45.0;
    double r_to_model = 10.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 38);
  }

  test_that("C++ add_forward_speed works in the Southern Hemisphere near the eye on left of storm") {
    double symmetrical_surface_wind = 40.0;
    double forward_speed_u = -2.37;
    double forward_speed_v = -7.67;
    double surface_wind_direction = 225.0;
    double r_to_model = 10.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 42);
  }

  test_that("C++ add_forward_speed works in the Southern Hemisphere near the radius of maximum wind on right of storm") {
    double symmetrical_surface_wind = 55.0;
    double forward_speed_u = -2.37;
    double forward_speed_v = -7.67;
    double surface_wind_direction = 45.0;
    double r_to_model = 25.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 51);
  }

  test_that("C++ add_forward_speed works in the Southern Hemisphere near the radius of maximum wind on left of storm") {
    double symmetrical_surface_wind = 55.0;
    double forward_speed_u = -2.37;
    double forward_speed_v = -7.67;
    double surface_wind_direction = 225.0;
    double r_to_model = 25.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 59);
  }

  test_that("C++ add_forward_speed works in the Southern Hemisphere far from the eye on right of storm") {
    double symmetrical_surface_wind = 8.0;
    double forward_speed_u = -2.37;
    double forward_speed_v = -7.67;
    double surface_wind_direction = 45.0;
    double r_to_model = 80.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 6);
  }

  test_that("C++ add_forward_speed works in the Southern Hemisphere far from the eye on left of storm") {
    double symmetrical_surface_wind = 8.0;
    double forward_speed_u = -2.37;
    double forward_speed_v = -7.67;
    double surface_wind_direction = 225.0;
    double r_to_model = 80.0;
    double r_to_max_wind = 25.0;

    double estimated_windspeed = add_forward_speed(symmetrical_surface_wind,
                                                   forward_speed_u, forward_speed_v,
                                                   surface_wind_direction,
                                                   r_to_model, r_to_max_wind);

    expect_true(round(estimated_windspeed) == 10);
  }
}


// [[Rcpp::export]]
List calc_grid_winds_cpp(NumericVector glat, NumericVector glon, LogicalVector glandsea,
                         double max_dist,
                         NumericVector tclat, NumericVector tclon,
                         NumericVector Rmax, NumericVector R1, NumericVector R2,
                         NumericVector vmax_gl, NumericVector n,
                         NumericVector A, NumericVector X1,
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
        wind_gl_aa = will1(cdist, Rmax[i], R1[i], R2[i], vmax_gl[i], n[i], A[i], X1[i]);
        // Calculate the gradient wind direction at the point
        gwd = calc_gwd(tclat_rad[i], tclon_rad[i], glat_rad[j], glon_rad[j]);
        // Calculate symmetrical surface wind at the point
        wind_sfc_sym = gradient_to_surface(wind_gl_aa, cdist, glandsea[j]);
        // Add inflow
        swd = add_inflow(gwd, cdist, Rmax[i], tclat[i], glandsea[j]);
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
stormwindmodel:::calc_grid_winds_cpp(glat = 29.97270, glon = -90.05284,
                                     glandsea = TRUE, max_dist =  2222.4,
                                    tclat = c(29.6, 29.0, NA), tclon = c(-90.0, -90.1, NA),
                                    Rmax = c(22.1, 22.1, NA), R1 = c(4.42, 4.43, NA), R2 = c(29.4, 29.4, NA), vmax_gl = c(73.6, 73.6, NA),
                                    n = c(1.38, 1.38, NA), A = c(0.279, 0.279, NA), X1 = c(213.0, 213.0, NA),
                                    tcspd_u = c(-6.23, -6.25, NA), tcspd_v = c(73.6, 73.6, NA))

stormwindmodel:::calc_grid_winds_cpp(glat = c(29.97270, 30, 29), glon = c(-90.05284, -90, -89),
                                     glandsea = c(TRUE, TRUE, TRUE), max_dist =  2222.4,
                                     tclat = c(29.6, 29.0, NA), tclon = c(-90.0, -90.1, NA),
                                     Rmax = c(22.1, 22.1, NA), R1 = c(4.42, 4.43, NA), R2 = c(29.4, 29.4, NA), vmax_gl = c(73.6, 73.6, NA),
                                     n = c(1.38, 1.38, NA), A = c(0.279, 0.279, NA), X1 = c(213.0, 213.0, NA),
                                     tcspd_u = c(-6.23, -6.25, NA), tcspd_v = c(73.6, 73.6, NA))



katrina_full_track <- create_full_track(hurr_track = stormwindmodel::katrina_tracks, tint = 0.25)
katrina_wind_radii <- add_wind_radii(full_track = katrina_full_track)
florida_location <- county_points %>%
  filter(stringr::str_sub(gridid, 1, 2) == "12")

a <- stormwindmodel:::calc_grid_winds_cpp(glat = florida_location$glat, glon = florida_location$glon,
                                     glandsea = rep(TRUE, nrow(florida_location)), max_dist =  2222.4,
                                    tclat = katrina_wind_radii$tclat, tclon = katrina_wind_radii$tclon,
                                    Rmax = katrina_wind_radii$Rmax, R1 = katrina_wind_radii$R1,
                                    R2 = katrina_wind_radii$R2, vmax_gl = katrina_wind_radii$vmax_gl,
                                    n = katrina_wind_radii$n, A = katrina_wind_radii$A, X1 = katrina_wind_radii$X1,
                                    tcspd_u = katrina_wind_radii$tcspd_u, tcspd_v = katrina_wind_radii$tcspd_v)
*/

