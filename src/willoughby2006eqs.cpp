#include <Rcpp.h>
using namespace Rcpp;

//' Willoughby et al. (2006), Equation 1(a)
//'
//' @param r Numeric vector of radius from the storm center to the point you are
//'    measuring, in kilometers
//' @param vmax_gl Numeric vector of the tangential wind component of the maximum
//'    gradient wind speed, in meters per second
//' @param Rmax Numeric vector of the radius at which the maximum wind occurs,
//'    in kilometers
//' @inheritParams will3_right
//'
//' @export

// [[Rcpp::export]]
float will1a(float vmax_gl, float r,
             float Rmax, float n) {
  float Vi = vmax_gl * pow((r / Rmax), n);

  return Vi;
}

// Test

/*** R
will1a(20.0, 50.0, 25.0, 1.2)
*/


//' Willoughby et al. (2006), Equation 4
//'
//' @inheritParams will3_right
//' @inheritParams will1a
//'
//' @export

// [[Rcpp::export]]
float will4(float vmax_gl, float A, float r, float Rmax, float X1,
            float X2 = 25.0){

  float Vo = vmax_gl * ((1 - A) * exp((Rmax - r) / X1) +
    A * exp((Rmax - r) / X2));

  return Vo;
}

// Test

/*** R
will4(20.0, 3.2, 50.0, 25.0, 3.0)
*/


//' Willoughby et al. (2006), Equation 2
//'
//' Calculates equation 2 from Willoughby et al. (2006).
//'
//' @param R1 Numeric vector of radius at start of transition zone, in
//'    kilometers
//' @inheritParams will1a
//'
//' @return A numeric vector of the weighting parameter for Willoughby's wind
//'    profile equations (\eqn{w}).
//'
//' @export

// [[Rcpp::export]]
float will2(float r, float R1, float R2) {

  float w;
  float xi = (r - R1) / (R2 - R1);

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

// Test

/*** R
will2(20.0, 15, 30)
*/



//' Calculate right-hand side of Willoughby Eqn. 3
//'
//' Calculates the right hand side of the version of Eqn. 3 in Willoughby et al.
//' (2006) with the dual exponential profile.
//'
//' @param n A numeric vector of one of the parameters of the Willoughby model.
//' @param A A numeric vector of one of the parameters of the Willoughby model.
//' @param X1 A numeric vector of one of the parameters of the Willoughby model.
//' @param Rmax A numeric vector giving the radius to maximum winds (in kilometers)
//'    for the tropical storm.
//'
//' @return A numeric vector with the value for the right-hand side of Eqn. 3 in
//'    Willoughby et al. 2006, using the dual exponential version of that
//'    equation.
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
//' @export

// [[Rcpp::export]]
NumericVector will3_right(NumericVector n, NumericVector A,
                          NumericVector X1, NumericVector Rmax){
  NumericVector eq3_right = (n * ((1 - A) * X1 + 25 * A)) /
    (n * ((1 - A) * X1 + 25 * A) + Rmax);

  return eq3_right;
}


//' Calculate radius to start of transition region
//'
//' Once you've solved for \eqn{\xi}, this function uses this value and the estimated
//' \eqn{R_{max}}{Rmax} to determine \eqn{R1}, the radius from the storm center to the start of
//' the transition region.
//'
//' @param xi A numeric value with the \eqn{\xi} value determined by
//'    \code{\link{solve_for_xi}}
//' @inheritParams will1a
//'
//' @return A numeric vector with the estimated value of \eqn{R_1}, a parameter
//'    required for the Willoughby wind model.
//'
//' @details This function is calculating the equation:
//'
//'    \deqn{R_1 = R_{max} - \xi(R_2 - R_1)}{
//'    R1 = Rmax - \xi(R2 - R1)}
//'
//'    where:
//'    \itemize{
//'      \item{\eqn{R_1}{R1}: A parameter for the Willoughby wind model (radius to
//'        start of transition region)}
//'      \item{\eqn{R_{max}}{Rmax}: Radius (in kilometers) to highest winds}
//'      \item{\eqn{R_2 - R_1}{R2 - R1}: Width of the transition region. This is
//'        assumed to be 25 kilometers if \eqn{R_{max}}{Rmax} is greater than
//'        20 kilometers and 15 kilometers otherwise.}
//'    }
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
//' @export

// [[Rcpp::export]]
NumericVector calc_R1(NumericVector Rmax, NumericVector xi) {
  int n = Rmax.size();
  NumericVector R2_minus_R1(n);

  for(int i = 0; i < n;  i++){
    if(Rmax[i] > 20.0){
      R2_minus_R1[i] = 25;
    } else {
      R2_minus_R1[i] = 15;
    }
  }

  NumericVector R1 = Rmax - xi * R2_minus_R1;

  return R1;
}


//' Calculate radius of maximum winds
//'
//' Calculates the radius at which the maximum wind occurs
//' (\eqn{R_{max}}{Rmax}), based on the maximum gradient-level 1-min sustained
//' wind (\eqn{V_{max,G}}{vmax_gl}) and latitude (\eqn{\phi}). This function
//' implements Willoughby et al. (2006), Equation 7a.
//'
//' @param tclat Numeric vector of the absolute value of latitude, in degrees.
//' @inheritParams will1a
//'
//' @details This function fits the following equation:
//' \deqn{R_{max} = 46.4 e^{- 0.0155 V_{max,G} + 0.0169\phi}}{
//' Rmax = 46.4 e^(- 0.0155 vmax_gl) + 0.0169\phi}
//' where:
//' \itemize{
//'   \item{\eqn{R_{max}}{Rmax}: Radius from the storm center to the point at which the maximum wind occurs (km)}
//'   \item{\eqn{V_{max,G}}{vmax_gl}: Tangential wind component of the gradient-level maximum wind speed (m / s)}
//'   \item{\eqn{\phi}: Absolute value of latitude (in decimal degrees)}
//' }
//'
//' @return A numeric vector with \eqn{R_{max}}{Rmax}, the radius of maximum
//'    winds, in kilometers.
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
//' @export

// [[Rcpp::export]]
NumericVector will7a(NumericVector vmax_gl, NumericVector tclat) {

  NumericVector Rmax = 46.4 * exp(-0.0155 * vmax_gl + 0.0169 * abs(tclat));

  return Rmax;
}

//' Calculate X1 for Willoughby model
//'
//' Calculates \eqn{X_1}{X1}, a parameter for the Willoughby wind model using
//' equation 10a (Willoughby et al. 2006).
//'
//' @inheritParams will1a
//' @inheritParams will7a
//'
//' @return A numeric vector giving one of the parameters (\eqn{X_1}{X1})
//'    required for the Willoughby wind model.
//'
//' @details This function uses the following equation (equation 10a, Willoughby
//' et al. 2006) to calculate the \eqn{X_1}{X1} parameter:
//'    \deqn{X_1 = 317.1 - 2.026V_{max,G} + 1.915 \phi}{
//'    X1 = 317.1 - 2.026vmax_gl + 1.915 \phi}
//'    where:
//'    \itemize{
//'      \item{\eqn{X_1}{X1}: Parameter for the Willoughby wind model}
//'      \item{\eqn{V_{max,G}}{vmax_gl}: Maximum gradient-level 1-min sustained
//'         wind (m / s)}
//'      \item{\eqn{\phi}: Absolute value of latitude, in decimal degrees}
//'    }
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
//' @export

// [[Rcpp::export]]
NumericVector will10a(NumericVector vmax_gl, NumericVector tclat) {

  NumericVector X1 = 317.1 - 2.026 * vmax_gl + 1.915 * abs(tclat);

  return X1;
}


//' Calculate n for the Willoughby model
//'
//' Calculates \eqn{n}, a parameter for the Willoughby model, using equation 10b
//' (Willoughby et al. 2006).
//'
//' @inheritParams will1a
//' @inheritParams will7a
//'
//' @return A numeric vector for \eqn{n}, a parameter needed for the Willoughby wind
//'    model.
//'
//' @details This function is calculating the equation:
//'    \deqn{n = 0.4067 + 0.0144 V_{max,G} - 0.0038 \phi}{
//'    n = 0.4067 + 0.0144 vmax_gl - 0.0038 \phi}
//'    where:
//'    \itemize{
//'      \item{\eqn{n}: Parameter for the Willoughby wind model}
//'      \item{\eqn{V_{max,G}}{vmax_gl}:  Maximum gradient-level 1-min sustained
//'         wind (m / s)}
//'      \item{\eqn{\phi}: Absolute value of latitude, in decimal degrees}
//'    }
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
//' @export

// [[Rcpp::export]]
NumericVector will10b(NumericVector vmax_gl, NumericVector tclat){

  NumericVector n = 0.4067 + 0.0144 * vmax_gl - 0.0038 * abs(tclat);

  return n;
}


//' Calculate A for Willoughby model
//'
//' Calculates \eqn{A}, a parameter for the Willoughby wind model, using equation 10c
//' (Willoughby et al. 2006).
//'
//' @inheritParams will1a
//' @inheritParams will7a
//'
//' @return A numeric vector that is a parameter required for the Willoughby
//'    model.
//'
//' @details This function calculates \eqn{A} using equation 10c (Willoughby et al.
//'    2006):
//'
//'    \deqn{A = 0.0696 + 0.0049 V_{max,G} - 0.0064 \phi}{
//'    A = 0.0696 + 0.0049 vmax_gl - 0.0064 \phi}
//'
//'    where:
//'    \itemize{
//'      \item{\eqn{A}: Parameter for the Willoughby wind model (any value
//'          of A calculated as negative is re-set to 0)}
//'      \item{\eqn{V_{max,G}}{vmax_gl}: Tangential component of the maximum
//'            gradient-level sustained wind speed (in m / s)}
//'      \item{\eqn{\phi}: Absolute value of latitude, in decimal degrees}
//'    }
//'    Any negative values of \eqn{A} are reset to 0.
//'
//' @references
//'
//' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
//' of the primary hurricane vortex. Part II: A new family of sectionally
//' continuous profiles. Monthly Weather Review 134(4):1102-1120.
//'
//' @export

// [[Rcpp::export]]
NumericVector will10c(NumericVector vmax_gl, NumericVector tclat){

  int n = vmax_gl.size();

  NumericVector A = 0.0696 + 0.0049 * vmax_gl - 0.0064 * abs(tclat);

  for(int i = 0; i < n; i++){
    if(A[i] < 0){
      A[i] = 0;
    }
  }

  return A;
}


//' Model wind speed at a grid point for a storm track observation
//'
//' Models the gradient wind speed at a certain radius from
//' a storm's center. To do this, it uses different equations and subfunctions
//' depending on how large the radius is (see details). This function requires,
//' as inputs, Willoughby wind model parameters calculated using the
//' \code{\link{add_wind_radii}} function.
//'
//' @param cdist Distance (in km) from center of tropical cyclone to grid point.
//' @param R1 A numeric vector of one of the parameters of the Willoughby model.
//' @param R2 A numeric vector of one of the parameters of the Willoughby model.
//' @param X2 A numeric vector of one of the parameters of the Willoughby model.
//' @inheritParams will1a
//' @inheritParams will3_right
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
//' @export

// [[Rcpp::export]]
NumericVector will1(NumericVector cdist, NumericVector Rmax, NumericVector R1,
                    NumericVector R2, NumericVector vmax_gl, NumericVector n,
                    NumericVector A, NumericVector X1, NumericVector X2 = 25){

    int vec_length = Rmax.size();
    NumericVector wind_gl_aa(vec_length);
    float Vi, Vo, w;

    for(int i = 0; i < vec_length; i++){
      if(cdist[i] < R1[i]){
        Vi = will1a(vmax_gl[i], cdist[i], Rmax[i], n[i]);
        wind_gl_aa[i] = Vi;
      } else if(cdist[i] > R2[i]){
        Vo = will4(vmax_gl[i], A[i], cdist[i], Rmax[i], X1[i],  X2[i]);
        wind_gl_aa[i] = Vo;
      } else {
        Vi = will1a(vmax_gl[i], cdist[i], Rmax[i], n[i]);
        Vo = will4(vmax_gl[i], A[i], cdist[i], Rmax[i], X1[i],  X2[i]);
        w = will2(cdist[i], R1[i], R2[i]);
        // Willoughby equation 1b
        wind_gl_aa[i] = Vi * (1 - w) + Vo * w;
      }

      if(wind_gl_aa[i] < 0){
        wind_gl_aa[i] = 0;
      }
    }

    return wind_gl_aa;
}

// Test

/*** R
will1a(46.4, 12.4, 35.0, 0.98) # Vi
will4(46.4, 0.13, 12.4, 35.0, 272.6,  25.0) # Vo

will1(12.4, 35.0, 18.0, 43.0, 46.4, 0.98, 0.13, 272.6) # Expect under 35
*/
