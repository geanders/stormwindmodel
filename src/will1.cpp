#include <Rcpp.h>
#include <cmath>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

//' Model wind speed at a grid point for a storm track observation (C++ version)
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
double will1_Cpp(double cdist, double Rmax, double R1, double R2, double vmax_gl, double n,
                 double A, double X1, double X2 = 25){

  if(NumericVector::is_na(Rmax) || NumericVector::is_na(vmax_gl) || NumericVector::is_na(n) ||
     NumericVector::is_na(A) || NumericVector::is_na(X1)){
    return -1;
  } else {

    double Vi = vmax_gl * pow((cdist / Rmax), n);
    double Vo = vmax_gl * ((1 - A) * exp((Rmax - cdist)/X1) + A * exp((Rmax - cdist) / X2));
    double wind_gl_aa;

    if(cdist < R1){
      wind_gl_aa = Vi;
    } else if (cdist > R2){
      wind_gl_aa = Vo;
    } else {
      double eps = (cdist - R1) / (R2 - R1);
      double w = 126 * pow(eps, 5) - 420 * pow(eps, 6) + 540 * pow(eps, 7) - 315 * pow(eps, 8)
        + 70 * pow(eps, 9);
      wind_gl_aa = Vi * (1 - w) + Vo * w;
    }

    //wind_gl_aa[wind_gl_aa < 0 & !is.na(wind_gl_aa)] <- 0
    if(wind_gl_aa < 0) {
      wind_gl_aa = 0;
    } else if(NumericVector::is_na(wind_gl_aa)) {
      return -1;
    }

    return wind_gl_aa;
  }
}



/*** R
will1(9,1,1,1,1,1,1,5,25)
will1_Cpp(9,1,1,1,1,1,1,5,25)
microbenchmark::microbenchmark(will1(9,1,1,1,1,1,1,5,25),will1_Cpp(9,1,1,1,1,1,1,5,25))
*/
