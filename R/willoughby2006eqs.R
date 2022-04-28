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
#'    \code{will3_right} function, defined in C++ code for this package.
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
#' dual-exponential profile version) for \eqn{\xi} in Willoughby et al. (2006).
#' This value of \eqn{\xi} can then be used to determine \eqn{R_1}{R1} for that
#' storm observation.
#'
#' @param xi0 A numeric value giving the starting guess for \eqn{\xi}
#' @param eps The convergence threshold for determining if the algorithm has
#'    converged.
#' @param itmax The maximum number of iterations to try before deciding that
#'    the algorithm did not converge.
#' @inheritParams will3_deriv_func
#'
#' @note If this algorithm does not converge, the function returns a missing value for
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

