#' Willoughby et al. (2006), Equation 1(a)
#'
#' @param r Numeric vector of radius from the storm center to the point you are
#'    measuring, in kilometers
#' @param vmax_gl Numeric vector of the tangential wind component of the maximum
#'    gradient wind speed, in meters per second
#' @param Rmax Numeric vector of the radius at which the maximum wind occurs,
#'    in kilometers
#' @param n Numeric vector of ...
#'
#' @export
will1a <- function(vmax_gl, r, Rmax, n){
  Vi <- vmax_gl * (r / Rmax) ^ n
  return(Vi)
}

#' Model wind speed at a grid point for a storm track observation
#'
#' Models the gradient wind speed at a certain radius from
#' a storm's center. To do this, it uses different equations and subfunctions
#' depending on how large the radius is (see details). This function requires,
#' as inputs, Willoughby wind model parameters calculated using the
#' \code{\link{add_wind_radii}} function.
#'
#' @param cdist Distance (in km) from center of tropical cyclone to grid point.
#' @param R1 A numeric vector of one of the parameters of the Willoughby model.
#' @param R2 A numeric vector of one of the parameters of the Willoughby model.
#' @param X2 A numeric vector of one of the parameters of the Willoughby model.
#' @inheritParams will1a
#' @inheritParams will3_right
#'
#' @return Returns a numeric vector with gradient wind speed at a radius of
#'    \eqn{r} from the storm's center, in meters per second.
#'
#' @details If \eqn{r \le R_1}{r \le R1}, this function is calculating the equation:
#'
#'    \deqn{V(r) = V_i = V_{max} \left( \frac{r}{R_{max}} \right)^n}{
#'    V(r) = Vi = vmax_gl (r / Rmax)^n}
#'
#'    where:
#'    \itemize{
#'      \item{\eqn{V(r)}: Maximum sustained gradient wind speed at a radius of
#'        \eqn{r} from the storm's center}
#'      \item{\eqn{r}: Radius from the storm center, in kilometers}
#'      \item{\eqn{V_{max,G}}{vmax_gl}: Maximum sustained gradient wind speed of the
#'        storm, in meters per second}
#'      \item{\eqn{R_1}{R1}: A parameter for the Willoughby wind model (radius to
#'        start of transition region)}
#'      \item{\eqn{R_{max}}{Rmax}: Radius (in kilometers) to highest winds}
#'      \item{\eqn{n}: A parameter for the Willoughby wind model}
#'    }
#'
#'    If \eqn{R_2 < r}{R2 \le r}, this function is calculating
#'      the equation:
#'
#'      \deqn{V(r) = V_o = V_{max}\left[(1 - A) e^\frac{R_{max} - r}{X_1} + A e^\frac{R_{max} - r}{X_2}\right]}{
#'      V(r) = Vo = vmax_gl[(1 - A) e^((Rmax - r) / X1) + A e^((Rmax - r) / X_2)]}
#'
#'    where:
#'    \itemize{
#'      \item{\eqn{V(r)}: Maximum sustained gradient wind speed at a radius of
#'        \eqn{r} kilometers from the storm's center}
#'      \item{\eqn{r}: Radius from the storm center, in kilometers}
#'      \item{\eqn{V_{max,G}}{vmax_gl}: Maximum sustained gradient wind speed of the
#'        storm, in meters per second}
#'      \item{\eqn{R_{max}}{Rmax}: Radius (in kilometers) to highest winds}
#'      \item{\eqn{A}, \eqn{X_1}{X1}, \eqn{X_2}{X2}: Parameters for the
#'         Willoughby wind model}
#'    }
#'
#'    If \eqn{R_1 < r \le R_2}{R1 < r \le R2}, this function uses
#'      the equations:
#'
#'      \deqn{\xi = \frac{r - R_1}{R_2 - R_1}}{
#'      \xi = (r - R1) / (R2 - R1)}
#'
#'      and, if \eqn{0 \le \xi < \le 1} (otherwise, \eqn{w = 0}):
#'
#'      \deqn{w = 126 \xi^5 - 420 \xi^6 + 540 \xi^7- 315 \xi^8 + 70 \xi^9}
#'
#'      and then:
#'
#'      \deqn{V(r) = V_i (1 - w) + V_o w, (R_1 \le r \le R_2)}{
#'      V(r) = Vi (1 - w) + Vo w}
#'
#'    where, for this series of equations:
#'    \itemize{
#'      \item{\eqn{V(r)}: Maximum sustained gradient wind speed at a radius of
#'        \eqn{r} kilometers from the storm's center}
#'      \item{\eqn{r}: Radius from the storm center, in kilometers}
#'      \item{\eqn{w}: Weighting variable}
#'      \item{\eqn{R_1}{R1}, \eqn{R_2}{R2}: Parameters for the Willoughby wind model}
#'    }
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will1 <- function(cdist, Rmax, R1, R2, vmax_gl, n, A, X1, X2 = 25){

  if(is.na(Rmax) || is.na(vmax_gl) ||
     is.na(n) || is.na(A) || is.na(X1)){
    return(NA)
  } else {

    Vi <- vmax_gl * (cdist / Rmax) ^ n
    Vo <- vmax_gl * ((1 - A) * exp((Rmax - cdist)/X1) + A * exp((Rmax - cdist) / X2))

    if(cdist < R1){
      wind_gl_aa <- Vi
    } else if (cdist > R2){
      wind_gl_aa <- Vo
    } else {
      eps <- (cdist - R1) / (R2 - R1)
      w <- 126 * eps ^ 5 - 420 * eps ^ 6 + 540 * eps ^ 7 - 315 *
        eps ^ 8 + 70 * eps ^ 9
      wind_gl_aa <- Vi * (1 - w) + Vo * w
    }

    wind_gl_aa[wind_gl_aa < 0 & !is.na(wind_gl_aa)] <- 0

    return(wind_gl_aa)
  }
}

#' Willoughby et al. (2006), Equation 2
#'
#' @param R1 Numeric vector of radius at start of transition zone, in
#'    kilometers
#' @inheritParams will1a
#'
#' @return w Numeric vector of the weighting parameter for Willoughby's wind
#'    profile equations.
#'
#' @export
will2 <- function(r, R1){
  xi = (r - R1) / 25

  if(xi <= 0){
      w <- 0
      } else if (xi >= 1){
        w <- 1
        } else {
          w <- 126 * xi ^ 5 - 420 * xi ^ 6 + 540 * xi ^ 7 - 315 * xi ^ 8 +
            70 * xi ^ 9
        }

  return(w)
}

#' Calculate right-hand side of Willoughby Eqn. 3
#'
#' Calculates the right hand side of the version of Eqn. 3 in Willoughby et al.
#' (2006) with the dual exponential profile.
#'
#' @param n A numeric vector of one of the parameters of the Willoughby model.
#' @param A A numeric vector of one of the parameters of the Willoughby model.
#' @param X1 A numeric vector of one of the parameters of the Willoughby model.
#' @param Rmax A numeric vector giving the radius to maximum winds (in kilometers)
#' @inheritParams will1a
#'
#' @return A numeric vector with the value for the right-hand side of Eqn. 3 in
#'    Willoughby et al. 2006, using the dual exponential version of that
#'    equation.
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will3_right <- function(n, A, X1, Rmax){
  eq3_right <- (n * ((1 - A) * X1 + 25 * A)) /
      (n * ((1 - A) * X1 + 25 * A) + Rmax)
  return(eq3_right)
}

#' Calculate the function value and derivative for Willoughby Eqn. 3
#'
#' Calculates values of both the function and the derivative of
#' the function for which you are trying to solve the root in Eqn. 3
#' (the version using the dual exponential profile) of Willoughby et al. (2006).
#'
#' @param xi A numerical value for \eqn{\xi} from Willoughby et al. (2006),
#'    Eqn. 2
#' @param eq3_right A numerical value with the right-hand side of Willoughby
#'    et al. (2006), Eqn. 3, the dual-exponential version. This value is
#'    calculated at each storm observation point using the
#'    \code{\link{will3_right}} function.
#'
#' @return A numeric vector of length two. The first value is the calculated
#'    value of \eqn{f'(x)} for \eqn{x = \xi}, while the second value is the
#'    calculated value of \eqn{f(x)} for \eqn{x = \xi}. These two values
#'    are used in iterating through the Newton-Raphson method to determine
#'    \eqn{\xi}.
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will3_deriv_func <- function(xi, eq3_right){
  deriv <- 70 * 9 * xi ^ 8 - 315 * 8 * xi ^ 7 + 540 * 7 * xi ^ 6 -
    420 * 6 * xi ^ 5 + 126 * 5 * xi ^ 4
  func <- 70 * xi ^ 9 - 315 * xi ^ 8 + 540 * xi ^ 7 - 420 * xi ^ 6 +
    126 * xi ^ 5 - eq3_right
  deriv_func <-c(deriv, func)
  return(deriv_func)
}

#' Numerically solve Willoughby Eqn. 3 for xi
#'
#' This function uses the Newton-Raphson method to solve equation 3 (the
#' dual-exponential profile version) for \eqn{\xi} in Willoughby et al. (2006).   This value of \eqn{xi} can
#' THis value of \eqn{\xi} can then be used to determine \eqn{R_1}{R1} for that
#' storm observation.
#'
#' @param xi0 A numeric value giving the starting guess for \eqn{xi}
#' @param eps The convergence threshold for determining if the algorithm has
#'    converged.
#' @param itmax The maximum number of iterations to try before deciding that
#'    the algorithm did not converge.
#' @inheritParams will3_deriv_func
#'
#' @note If this algorithm does not converge, it returns a missing value for
#'    \eqn{\xi}.
#'
#' @references
#'
#' Jones O, Maillardet R, and Robinson A. 2009. Introduction to Scientific
#' Programming and Simulation Using R. Boca Raton, FL: Chapman & Hall/CRC Press.
#'
#' Press WH, Teukolsky SA, Vetterling WT, and Flannery BP. 2002. Numerical
#' Recipes in C++: The Art of Scientific Computing. 2nd ed. Cambridge, UK:
#' Cambridge University Press.
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
solve_for_xi <- function(xi0 = 0.5, eq3_right, eps = 10e-4, itmax = 100){
  if(is.na(eq3_right)){
    return(NA)
  } else{
    i <- 1
    xi <- xi0
    while(i <= itmax){
      deriv_func <- will3_deriv_func(xi, eq3_right)
      if(abs(deriv_func[2]) <= eps){ break }
      xi <- xi - deriv_func[2] / deriv_func[1]
    }
    if(i < itmax){
      return(xi)
    } else{
      warning("Newton-Raphson did not converge.")
      return(NA)
    }
  }
}

#' Calculate radius to start of transition region
#'
#' Once you've solved for \eqn{\xi}, use this value and the estimated
#' \eqn{R_{max}}{Rmax} to determine \eqn{R1}, the radius from the storm center to the start of
#' the transition region.
#'
#' @param xi A numeric value with the \eqn{\xi} value determined by
#'    \code{\link{solve_for_xi}}
#' @inheritParams will1a
#'
#' @return A numeric vector with the estimated value of \eqn{R_1}, a parameter
#'    required for the Willoughby wind model.
#'
#' @details This function is calculating the equation:
#'
#'    \deqn{R_1 = R_{max} - \xi(R_2 - R_1)}{
#'    R1 = Rmax - \xi(R2 - R1)}
#'
#'    where:
#'    \itemize{
#'      \item{\eqn{R_1}{R1}: A parameter for the Willoughby wind model (radius to
#'        start of transition region)}
#'      \item{\eqn{R_{max}}{Rmax}: Radius (in kilometers) to highest winds}
#'      \item{\eqn{R_2 - R_1}{R2 - R1}: Width of the transition region. This is
#'        assumed to be 25 kilometers if \eqn{R_{max}}{Rmax} is greater than
#'        20 kilometers and 15 kilometers otherwise.}
#'    }
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
calc_R1 <- function(Rmax, xi){
  R2_minus_R1 <- ifelse(Rmax > 20, 25, 15)
  R1 <- Rmax - xi * R2_minus_R1
  return(R1)
}

#' Calculate radius of maximum winds
#'
#' Calculates the radius at which the maximum wind occurs
#' (\eqn{R_{max}}{Rmax}), based on the maximum gradient-level 1-min sustained
#' wind (\eqn{V_{max,G}}{vmax_gl}) and latitude (\eqn{\phi}). This function
#' implements Willoughby et al. (2006), Equation 7a.
#'
#' @param tclat Numeric vector of the absolute value of latitude, in degrees.
#' @inheritParams will1a
#'
#' @details This function fits the following equation:
#' \deqn{R_{max} = 46.4 e^{- 0.0155 V_{max,G} + 0.0169\phi}}{
#' Rmax = 46.4 e^(- 0.0155 vmax_gl) + 0.0169\phi}
#' where:
#' \itemize{
#'   \item{\eqn{R_{max}}{Rmax}: Radius from the storm center to the point at which the maximum wind occurs (km)}
#'   \item{\eqn{V_{max,G}}{vmax_gl}: Tangential wind component of the gradient-level maximum wind speed (m / s)}
#'   \item{\eqn{\phi}: Latitude (degrees)}
#' }
#'
#' @return A numeric vector with \eqn{R_{max}}{Rmax}, the radius of maximum
#'    winds, in kilometers.
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will7a <- function(vmax_gl, tclat){
    Rmax <- 46.4 * exp(-0.0155 * vmax_gl + 0.0169 * tclat)
    return(Rmax)
}

#' Calculate X1 for Willoughby model
#'
#' Calculates \eqn{X_1}{X1}, a parameter for the Willoughby wind model using
#' equation 10a (Willoughby et al. 2006).
#'
#' @inheritParams will1a
#' @inheritParams will7a
#'
#' @return A numeric vector giving one of the parameters (\eqn{X_1}{X1})
#'    required for the Willoughby wind model.
#'
#' @details This function uses the following equation (equation 10a, Willoughby
#' et al. 2006) to calculate the \eqn{X_1}{X1} parameter:
#'    \deqn{X_1 = 317.1 - 2.026V_{max,G} + 1.915 \phi}{
#'    X1 = 317.1 - 2.026vmax_gl + 1.915 \phi}
#'    where:
#'    \itemize{
#'      \item{\eqn{X_1}{X1}: Parameter for the Willoughby wind model}
#'      \item{\eqn{V_{max,G}}{vmax_gl}: Maximum gradient-level 1-min sustained
#'         wind (m / s)}
#'      \item{\eqn{\phi}: Latitude, in decimal degrees}
#'    }
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will10a <- function(vmax_gl, tclat){
  X1 <- 317.1 - 2.026 * vmax_gl + 1.915 * tclat
  return(X1)
}

#' Calculate n for the Willoughby model
#'
#' Calculates n, a parameter for the Willoughby model, using equation 10b
#' (Willoughby et al. 2006).
#'
#' @inheritParams will1a
#' @inheritParams will7a
#'
#' @return A numeric vector for n, a parameter needed for the Willoughby wind
#'    model.
#'
#' @details This function is calculating the equation:
#'    \deqn{n = 0.4067 + 0.0144 V_{max,G} - 0.0038 \phi}{
#'    n = 0.4067 + 0.0144 vmax_gl - 0.0038 \phi}
#'    where:
#'    \itemize{
#'      \item{\eqn{n}: Parameter for the Willoughby wind model}
#'      \item{\eqn{V_{max,G}}{vmax_gl}:  Maximum gradient-level 1-min sustained
#'         wind (m / s)}
#'      \item{\eqn{\phi}: Latitude, in decimal degrees}
#'    }
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will10b <- function(vmax_gl, tclat){
  n <- 0.4067 + 0.0144 * vmax_gl - 0.0038 * tclat
  return(n)
}

#' Calculate A for Willoughby model
#'
#' Calculates A, a paramter for the Willoughby wind model, using equation 10c
#' (Willoughby et al. 2006).
#'
#' @inheritParams will1a
#' @inheritParams will7a
#'
#' @return A numeric vector that is a parameter required for the Willoughby
#'    model.
#'
#' @details This function calculates A using (equation 10c, Willoughby et al.
#'    2006):
#'
#'    \deqn{A = 0.0696 + 0.0049 V_{max,G} - 0.0064 \phi}{
#'    A = 0.0696 + 0.0049 vmax_gl - 0.0064 \phi}
#'
#'    where:
#'    \itemize{
#'      \item{\eqn{A}: Parameter for the Willoughby wind model (any value
#'          of A calculated as negative is re-set to 0)}
#'      \item{\eqn{V_{max,G}}{vmax_gl}: Tangential component of the maximum
#'            gradient-level sustained wind speed (in m / s)}
#'      \item{\eqn{\phi}: Latitude, in decimal degrees}
#'    }
#'    Any negative values of \eqn{A} are reset to 0.
#'
#' @references
#'
#' Willoughby HE, Darling RWR, and Rahn ME. 2006. Parametric representation
#' of the primary hurricane vortex. Part II: A new family of sectionally
#' continuous profiles. Monthly Weather Review 134(4):1102-1120.
#'
#' @export
will10c <- function(vmax_gl, tclat){
  A <- 0.0696 + 0.0049 * vmax_gl - 0.0064 * tclat
  A[A < 0 & !is.na(A)] <- 0
  return(A)
}
