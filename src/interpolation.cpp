#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;


//' Calculate coefficients for linear interpolation
//'
//' @param x A numeric vector with independent values of the original function.
//' @param y A numeric vector with dependent values of the original function.
//' @return A numeric vector with slope coefficients for the linear interpolation of
//'   the original function. This vector has a length one less that the input vectors.
//'   The slope is for the section starting at that location in the x input vector.
//' @export
// [[Rcpp::export]]
NumericVector calc_linear_coefs(NumericVector x, NumericVector y) {

  int n = x.size();                            // Size of the input vectors.
  int i;
  NumericVector b(n-1);                        // Output vector---one shorter than input vectors
  // (for each section *between* input values).
  for(i = 0; i < (n - 1); ++i){
    if(x[i] == x[i+1]){                         // If two x's in a row have the same value,
      b[i] = INFINITY;                            // the slope is undefined---set to Inf.
    } else {                                    // Otherwise, determine the slope based on
      b[i] = (y[i+1] - y[i]) / (x[i+1] - x[i]);   // "rise over run".
    }
  }

  return b;
}


/*** R
# Check that the function to calculate slopes for linear interpolation is working as expected

x_test <- c(1, 2, 3, 4)
y_test <- c(10, 20, 20, 25)
expected_slopes <- c(10, 0, 5)

stopifnot(all.equal(expected_slopes,
                    calc_linear_coefs(x_test, y_test)))

x_test <- c(1, 2, 2)
y_test <- c(10, 20, 30)
expected_slopes <- c(10, Inf)

stopifnot(all.equal(expected_slopes,
                    calc_linear_coefs(x_test, y_test)))

# Bench mark the function
bench::mark(calc_linear_coefs(x_test, y_test))
*/


//' Find the location of a new value in a table of values
//'
//' @param new_x A double floating point number giving a new value of x for which
//'   you want to find the indices of the sections of a vector x that contain the
//'   new value.
//' @param x A numeric vector for which you want to find the position that brackets
//'   the new value, new_x.
//' @return An integer giving the zero-based index of the position in the x vector
//'   that gives the lower bracket of the section of the x-vector containing the new
//'   x value.
//' @export
// [[Rcpp::export]]
int find_x_section(double new_x, NumericVector x) {
  int n = x.size(); // Determine size of input vector x
  int l = 0;        // Index of lower limit of search range
  int u = n - 1;    // Index of upper limit of search range
  int i;            // Index of midway point

  if(new_x < x[l]) {          // Handle cases when the new x is outside the range of the
    return -1;                  // x table. If under the minimum, return that the lower
  } else if(new_x > x[u]) {     // index is -1. If over the maximum, return that the
    return n - 1;               // lower index is n-1.
  } else if(new_x == x[u]) {  // If the new value is exactly at the upper limit of the
    return n-2;                 // table of x's, return the second-to-last index as lower index.
  } else {                    // Run a loop for all other cases
    while((u - l) > 1) {      // Stop when the lower and upper values are separated by 1
      i = ((l + u) / 2);      // Take a midpoint halfway between the lower and upper limits
      if(new_x < x[i]){       // Reset the upper and lower limits based on comparison of
        u = i;                  // the new x with the midpoint.
      } else {
        l = i;
      }
    }
    return l;
  }
}

/*** R
# Check that R function is working correctly

# Return 0 when the new value is between the first (index 0) and second (index 1)
# values in the table of x's
new_value <- 1.01
existing_values <- c(1, 2, 3, 4, 5)
expected_index <- 0L

stopifnot(all.equal(expected_index,
                    find_x_section(new_x = new_value, x = existing_values)))

# Return n-1 when the new value is higher than the maximum value in the table of x's
new_value <- 100
existing_values <- c(1, 2, 3, 4, 5)
expected_index <- 4L

stopifnot(all.equal(expected_index,
                    find_x_section(new_x = new_value, x = existing_values)))

# Return -1 when the new value is lower than the minimum value in the table of x's
new_value <- -5
existing_values <- c(1, 2, 3, 4, 5)
expected_index <- -1L

stopifnot(all.equal(expected_index,
                    find_x_section(new_x = new_value, x = existing_values)))

# Return 1 when the new value is exactly equal to the second value (index 1) in the
# table of x's
new_value <- 2
existing_values <- c(1, 2, 3, 4, 5)
expected_index <- 1L

stopifnot(all.equal(expected_index,
                    find_x_section(new_x = new_value, x = existing_values)))

# Return 3 when the new value is exactly at the upper limit of the table of x's
# (this keeps the bracket within the range of the table---otherwise, if the new
# value is right at the border, it goes into the section above that border).
new_value <- 3.01
existing_values <- c(1, 2, 3, 4, 5)
expected_index <- 2L

# Return 2 when the new value is between the third (index 2) and fourth (index 3)
# values in the table of x's
new_value <- 3.01
existing_values <- c(1, 2, 3, 4, 5)
expected_index <- 2L

stopifnot(all.equal(expected_index,
                    find_x_section(new_x = new_value, x = existing_values)))


# Benchmark the function
bench::mark(find_x_section(new_x = new_value, x = existing_values))
*/

//' Interpolate new y-values using linear interpolation
//'
//' @param new_x A numeric value providing the new x values at which you want to
//'   interpolate y values.
//' @param x A numeric vector with the x values of the original function.
//' @param y A numeric vector with the y values of the original function. This should
//'   be the same length as x.
//' @return A numeric vector with the interpolated values of y at each of the new
//'   x values provided by the new_x input.
//' @export
// [[Rcpp::export]]
NumericVector interpolate_line(NumericVector x, NumericVector y, NumericVector new_x) {
  int m = x.size();
  int n = new_x.size();
  int i, l = 0;
  NumericVector new_y(n);

  NumericVector b = calc_linear_coefs(x, y);

  for(i = 0; i < n; ++i){
    if(!(new_x[i] >= x[l] && new_x[i] <= x[l])){  // Unless the l is the same as for the last new_x,
      l = find_x_section(new_x[i], x);               // find the index of the lower bound of section
    }
    if(l == -1 || l == (m - 1)){                  // For new x's outside the original x
      new_y[i] = NA_REAL;                           // table range, set interp. y as missing.
    } else {
      new_y[i] = y[l] + b[l] * (new_x[i] - x[l]); // For new x's within the table range,
    }                                               // calculate the interpolated value of y
  }                                                 // at the new x value.

  return new_y;
}


/***R
# Test that linear interpolation functions works as desired

new_x_test <- c(1.5, 1.75, 1.8, 2, 4, 4.5, 5, -2, 2.432, 4.3, NA)
x_test <- c(1, 2, 3, 4, 4.5)
y_test <- c(10, 20, 30, 40, -100)
expected_new_y <- c(15, 17.5, 18, 20, 40, -100, NA, NA, 24.32, -44.00, NA)

all.equal(expected_new_y,
          approx(x_test, y_test, new_x_test, method = "linear")$y, # Base R version
          interpolate_line(x_test, y_test, new_x_test))

# Benchmark new function
bench::mark(approx(x_test, y_test, new_x_test, method = "linear")$y,
            interpolate_line(x_test, y_test, new_x_test))
*/

//' Make interp
//' @export
// [[Rcpp::export]]
NumericVector make_interp(NumericVector x, NumericVector y) {
  int n = x.size();
  int j;
  NumericVector y2d(n);
  NumericVector rho(n), gamma(n);
  float a, b, c, d;

  // Set second derivative to zero at endpoints
  y2d[0] = y2d[n-1] = 0.0; // For a natural cubic spline, derivatives are 0 at ends
  gamma[0] = rho[0] = 0.0;

  // Do first stage of solving the tridiagonal equation system
  for (j = 1; j < (n-1); j++){
    // Calculate coefficients for tridiagonal system for the row
    if(j == 1) {
      a = 0.0;
    } else {
      a = (x[j] - x[j-1]) / 6.0;
    }
    b = (x[j+1] - x[j-1]) / 3.0;
    c = (x[j+1] - x[j]) / 6.0;
    d = ((y[j+1] - y[j]) / (x[j+1] - x[j])) -
      ((y[j] - y[j-1]) / (x[j] - x[j-1]));

    // Do first pass of solving system (get 'a'-position coefficient to 0 and
    // 'b'-position coefficient to 1). gamma is new value in `c`-position and
    // rho is new value in 'd'-position.
    gamma[j] = c / (b - (a * gamma[j-1]));
    rho[j] = (d - (a * rho[j-1])) /
      (b - (a * gamma[j-1]));
  }

  // Do second stage in solving the tridiagonal equation system
  // (backsubstitution)
  for (j = (n-2); j > 0; j--){
    y2d[j] = rho[j] - (gamma[j] * y2d[j+1]);
  }

  return y2d;
}

/***R
test_x <- c(1, 2, 3, 4, 5)
test_y <- c(0, 1, 0, 1, 0)


new_y <- stormwindmodel:::make_interp(test_x, test_y)
*/

// [[Rcpp::export]]
NumericVector apply_interp(NumericVector new_x, NumericVector x,
                           NumericVector y, NumericVector y2d){
  int m = x.size(); // 9
  int n = new_x.size(); // 90
  int j, l, u, k;
  NumericVector new_y(n); // length of 90
  float diff_x, A, B, C, D;

  l = 0;
  u = m - 1;

  for(j = 0; j < n; j++){ // j going from 0 to 89
    // Only look for the section if it's not the same as for the last value
    // At this point, l and u are still set to their final values from the
    // last iteration. Since we have ordered new x values, this should reduce
    // having to do the bisection pretty often.
    // l: index of lower bracket, u: index of upper bracket
    //if(1){
    if(!(x[l] <= new_x[j] && new_x[j] <= x[u] && j > 0)){
      l = 0;
      u = m-1;
      while ((u - l) > 1){
        k = (u + l) / 2;
        if(x[k] > new_x[j]) {
          u = k;
        } else {
          l = k;
        }
      }
    }

    // Determine the new (interpolated) y value based on the equation within this
    // segment of the original function.
    diff_x = x[u] - x[l];
    A = (x[u] - new_x[j]) / diff_x;
    B = 1 - A;
    C = (1.0 / 6.0) * (pow(A, 3) - A) * pow(diff_x, 2);
    D = (1.0 / 6.0) * (pow(B, 3) - B) * pow(diff_x, 2);

    // Apply the spline equation
    new_y[j] = A * y[l] + B * y[u] + C * y2d[l] + D * y2d[u];
  }

  return new_y;
}

/***R
## Try with example from `spline` help documentation
n <- 9
x <- 1:n
y <- rnorm(n)

(xout <- seq(min(x), max(x), length.out = 10 * length(x)))
(yout <- spline(x, y, method = "natural", xout = xout)$y)
(y2d <- stormwindmodel:::make_interp(x, y))
(test_yout <- stormwindmodel:::apply_interp(xout, x, y, y2d))

plot(x, y)
lines(xout, yout, col = "red")
lines(xout, test_yout, col = "blue")

stopifnot(all.equal(yout, test_yout,
                    tolerance = 10 * sqrt(.Machine$double.eps)))
bench::mark(spline(x, y, method = "natural", xout = xout),
            stormwindmodel:::apply_interp(xout, x, y, make_interp(x, y)),
            check = FALSE)

*/

//' @export
// [[Rcpp::export]]
NumericVector interpolate_spline(NumericVector x, NumericVector y, NumericVector new_x) {

  int m = x.size();
  int n = new_x.size();
  NumericVector y2d(m);
  NumericVector new_y(n);

  y2d = make_interp(x, y);
  new_y = apply_interp(new_x, x, y, y2d);

  return new_y;
}

/***R
## Try with example from `spline` help documentation
n <- 9
x <- 1:n
y <- rnorm(n)

(xout <- seq(min(x), max(x), length.out = 10 * length(x)))
(yout <- spline(x, y, method = "natural", xout = xout)$y)
(test_yout <- interpolate_spline(x, y, xout))

plot(x, y)
lines(xout, yout, col = "red")
lines(xout, test_yout, col = "blue")

stopifnot(all.equal(yout, test_yout,
                    tolerance = 10 * sqrt(.Machine$double.eps)))
bench::mark(spline(x, y, method = "natural", xout = xout),
            interpolate_spline(x, y, xout),
            check = FALSE)

## Try with a hurricane example
library(dplyr)
library(lubridate)

data(katrina_tracks)
katrina_tracks <- katrina_tracks %>%
  mutate(date = ymd_hm(date),
         since_start = difftime(date, first(date), units = "hours"),
         since_start = as.numeric(since_start))

time_new <- seq(from = 0, to = max(katrina_tracks$since_start), by = 0.25)
katrina_interp_lon_1 <- spline(x = katrina_tracks$since_start,
                               y = katrina_tracks$longitude,
                               xout = time_new, method = "natural")$y
katrina_interp_lon_2 <- interpolate_spline(x = katrina_tracks$since_start,
                                           y = katrina_tracks$longitude,
                                           new_x = time_new)
katrina_interp_lat_1 <- spline(x = katrina_tracks$since_start,
                               y = katrina_tracks$latitude,
                               xout = time_new, method = "natural")$y
katrina_interp_lat_2 <- interpolate_spline(x = katrina_tracks$since_start,
                                           y = katrina_tracks$latitude,
                                           new_x = time_new)

stopifnot(all.equal(katrina_interp_lat_1, katrina_interp_lat_2))

plot(katrina_interp_lon_1, katrina_interp_lat_1, col = "red", cex = 0.01)
points(katrina_interp_lon_2, katrina_interp_lat_2, col = "blue", cex = 0.01)
*/
