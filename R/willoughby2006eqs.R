#' Willoughby et al. (2006), Equation 1(a)
#'
#' @param r Numeric vector of radius from the storm center to the point you are
#'    measuring, in kilometers
#' @param Vmax Numeric vector of the tangential wind component of the maximum
#'    gradient wind speed, in meters per second
#' @param Rmax Numeric vector of the radius at which the maximum wind occurs,
#'    in kilometers
#' @param n Numeric vector of ...
will1a <- function(Vmax, r, Rmax, n){
  Vi <- Vmax * (r / Rmax)^n
  return(Vi)
}

#' Model wind speed at each grid point for each storm track observation
#'
#' @inheritParams will1a
#' @inheritParams create_fnc_will3
#'
will1 <- function(r, Rmax, R1, R2, Vmax, n, A, X1, X2 = 25){

  if(is.na(Rmax) || is.na(Vmax) ||
     is.na(n) || is.na(A) || is.na(X1)){
    return(NA)
  } else {

    Vi <- Vmax * (r / Rmax) ^ n
    Vo <- Vmax * ((1 - A) * exp((Rmax - r)/X1) + A * exp((Rmax - r) / X2))

    if(r < R1){
      track <- Vi
    } else if (r > R2){
      track <- Vo
    } else {
      eps <- (r - R1) / (R2 - R1)
      w <- 126 * eps^5 - 420 * eps^6 + 540 * eps^7- 315 *
        eps^8 + 70 * eps^9
      track <- Vi * (1 - w) + Vo * w
    }

    track[track < 0 & !is.na(track)] <- 0

    return(track)
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
will2 <- function(r, R1){
  xi = (r - R1) / 25

  if(xi <= 0){
      w <- 0
      } else if (xi >= 1){
        w <- 1
        } else {
          w <- 126 * xi^5 - 420 * xi^6 + 540 * xi^7 - 315 * xi^8 + 70 * xi^9
        }

  return(w)
}

#' Calculate right-hand side of Willoughby Eqn. 3
#'
#' Create the right hand side of the version of Eqn. 3 in Willoughby et al. 2006
#' with the dual exponential profile.
#'
#' @param n A numeric vector ...
#' @param A A numeric vector ...
#' @param X1 A numeric vector ...
#' @inheritParams will1a
#'
#' @return A numeric vector with the value for the right-hand side of Eqn. 3 in
#'    Willoughby et al. 2006, using the dual exponential version of that
#'    equation.
will3_right <- function(n, A, X1, Rmax){
  eq3_right <- (n * ((1 - A) * X1 + 25 * A)) /
      (n * ((1 - A) * X1 + 25 * A) + Rmax)
  return(eq3_right)
}

#' Calculate the function value and derivative for Willoughby Eqn. 3
#'
#' This function calculates values of both the function and the equation for
#' the function that you are trying to solve the root of in Willoughby Eqn. 3
#' (the version using the dual exponential profile).
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
#' This function uses the Newton-Raphson method to solve Eqn 3. (the
#' dual-exponential profile version) for \eqn{xi}. This value of \eqn{xi} can
#' then be used to determine \eqn{R_1} for that storm observation.
#'
#' @param xi0 A numeric value giving the starting guess for \eqn{xi}
#' @param eps The convergence threshold. [...] must be lower than this value
#'    for the algorithm to have converged.
#' @param itmax The maximum number of iterations to try before deciding that
#'    the algorithm did not converge.
#' @inheritParams will3_deriv_func
#'
#' @note If this algorithm does not converge, it returns a missing value for
#'    \eqn{xi}.
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
      return(NA)
    }
  }
}

#' Calculate R1 using Rmax and xi
#'
#' Once you've solved for \eqn{xi}, use this value and the estimated
#' \eqn{R_{max}} to determine the radius from the storm center to the start of
#' the transition region.
#'
#' @param xi A numeric value with the \eqn{\xi} value determined by
#'    \code{\link{solve_for_xi}}
#' @inheritParams will1a
#'
#' @return A numeric vector with the estimated value of \eqn{R_1} for the
#'    storm track observation.
calc_R1 <- function(Rmax, xi){
  R2_minus_R1 <- ifelse(Rmax > 20, 25, 15)
  R1 <- Rmax - xi * R2_minus_R1
  return(R1)
}

#' Calculate Rmax from Vmax and phi
#'
#' This function calculates the radius at which the maximum wind occurs
#' (\eqn{Rmax}), based on the tangential wind component of the maximum wind
#' speed (\eqn{Vmax}) and latitude (\eqn{\phi}). This function implements
#' Willoughby et al. (2006), Equation 7a.
#'
#' @param phi Numeric vector of the absolute value of latitude, in degrees.
#' @inheritParams will1a
#'
#' @details This function fits the following equation:
#' \deqn{R_{max} = 46.4 e^{- 0.0155 V_{max} + 0.0169\phi}}{
#' Rmax = 46.4 e^(- 0.0155 Vmax) + 0.0169\phi}
#' where:
#' \itemize{
#'   \item{\eqn{R_{max}}{Rmax}: Radius from the storm center to the point at which the maximum wind occurs (km)}
#'   \item{\eqn{V_{max}}{Vmax}: Tangential wind component of the gradient-level maximum wind speed (m / s)}
#'   \item{\eqn{\phi}: Latitude (degrees)}
#' }
#'
#' @return A numeric vector with the radius at which the maximum wind occurs,
#'    in kilometers.
#'
#' @export
will7a <- function(Vmax, phi){
    Rmax <- 46.4 * exp(-0.0155 * Vmax + 0.0169 * phi)
    return(Rmax)
}

#' Calculate the fitted decay length from \eqn{Vmax} and \eqn{\phi}
#'
#' This function implements Willoughby et al. (2006), Equation 10a.
#'
#' @inheritParams will1a
#' @inheritParams will7a
#'
#' @return A numeric vector giving one of the parameters (\eqn{X_1}{X1})
#'    required for the Willoughby wind model.
#'
#' @details This function uses the following equation to calculate the
#'    \eqn{X_1}{X1} parameter:
#'    \deqn{X_1 = 317.1 - 2.026V_{max} + 1.915 \phi}{
#'    X1 = 317.1 - 2.026Vmax + 1.915 \phi}
#'    where:
#'    \itemize{
#'      \item{\eqn{X_1}{X1}: Parameter for the Willoughby wind model}
#'      \item{\eqn{V_{max}}{Vmax}: Tangential component of the maximum
#'            gradient-level sustained wind speed (in m / s)}
#'      \item{\eqn{\phi}: Latitude, in decimal degrees}
#'    }
#'
#' @export
will10a <- function(Vmax, phi){
  X1 <- 317.1 - 2.026 * Vmax + 1.915 * phi
  return(X1)
}

#' Calculate the power law exponential from \eqn{Vmax} and \eqn{\phi}
#'
#' This function implements Willoughby et al. (2006), Equation 10b.
#'
#' @inheritParams will1a
#' @inheritParams will7a
#'
#' @return A numeric vector of the exponential for the power law inside the eye
#'
#' @details This function is calculating the equation:
#'    \deqn{n = 0.4067 + 0.0144 V_{max} - 0.0038 \phi}{
#'    n = 0.4067 + 0.0144 Vmax - 0.0038 \phi}
#'    where:
#'    \itemize{
#'      \item{\eqn{n}: Parameter for the Willoughby wind model}
#'      \item{\eqn{V_{max}}{Vmax}: Tangential component of the maximum
#'            gradient-level sustained wind speed (in m / s)}
#'      \item{\eqn{\phi}: Latitude, in decimal degrees}
#'    }
#'
#' @export
will10b <- function(Vmax, phi){
  n <- 0.4067 + 0.0144 * Vmax - 0.0038 * phi
  return(n)
}

#' Calculate ... from \eqn{Vmax} and \eqn{\phi}
#'
#' This function implements Willoughby et al. (2006), Equation 10c.
#'
#' @inheritParams will1a
#' @inheritParams will7a
#'
#' @return A numeric vector ...
will10c <- function(Vmax, phi){
  A <- 0.0696 + 0.0049 * Vmax - 0.0064 * phi
  A[A < 0 & !is.na(A)] <- 0
  return(A)
}
