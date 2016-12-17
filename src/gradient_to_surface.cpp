#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]


//' Calculate surface wind speed from gradient (C++ version)
//' Calculates the surface wind speed based on an estimated gradient
//' wind speed at a point and the radius from the storm center to
//' the grid point.
//'
//' @param wind_gl_aa A numerical value with estimated gradient-level wind speed
//'    (m / s) at a grid point.
//' @param cdist A numerical value with radius from storm center to the grid
//'    point, in kilometers.
//'
//' @return A numeric vector with the estimated symmetric surface wind speed at
//'    the grid point, in meters / second.
//'
//' @details The reduction factor is based on Figure 3 of Knaff et al., 2003.
//' Over water, it is estimated to be 0.9 up to a radius of 100 km,
//' 0.75 for a radius of 700 km or more, and decrease linearly between
//' a radius of 100 km and 700 km. Points over land should use a reduction
//' factor that is 20\% lower. Because all of the counties are over
//' land, the function makes this adjustment for all grid points.
//'
//' @references
//'
//' Knaff JA, DeMaria M, Molenar DA, Sampson CR, and Seybold MG. 2011. An
//' automated, objective, multiple-satellite-platform tropical cyclone surface
//' wind speed analysis. Journal of Applied Meteorology and Climatology
//' 50(10):2149-2166
//'
//' @export
// [[Rcpp::export]]
double gradient_to_surface_Cpp(double wind_gl_aa, double cdist) {
  double reduction_factor;
  if(cdist < 100 || cdist == 100){
     reduction_factor = 0.9;
  } else if(cdist > 700 || cdist == 700){
    reduction_factor = 0.75;
  } else {
    reduction_factor = 0.90 - (cdist - 100) * (0.15/ 600);
  }
// Since all counties are over land, reduction factor should
// be 20% lower than if it were over water
  reduction_factor = reduction_factor * 0.8;
  return wind_gl_aa * reduction_factor;
}



/*** R
gradient_to_surface(3814.862,9.182584e-05)
gradient_to_surface_Cpp(3814.862,9.182584e-05)
library(microbenchmark)
microbenchmark(gradient_to_surface(3814.862,9.182584e-05),gradient_to_surface_Cpp(3814.862,9.182584e-05))
*/
