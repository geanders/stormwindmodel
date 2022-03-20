#include <Rcpp.h>
using namespace Rcpp;

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
//' @param tclat Numeric vector of the value of latitude, in degrees.
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


