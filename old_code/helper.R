#' Calculate motion direction angle from ...
#'
#' Use trig angles, so E=0, N=90, W=180, S=270)
#'
#' @param dpair Numeric vector of length 2 with the values for "dx" and "dy"
#'
#' @return This function returns the motion direction angle ...
calcangle_pair <- function(dx, dy) {
  if(is.na(dx) | is.na(dy)){
    return(NA)
  } else {
    if (dx > 0) {
      angle <- atan(dy / dx) * 180 / pi
    } else if (dx < 0) {
      angle <- 180 + atan(dy / dx) * 180 / pi
    } else {
      if (dy == 0) {
        angle <- 0
      } else{
        angle <- atan2(dy, dx) * 180 / pi
      }
    }
    if (angle < 0) {
      angle <- 360 + angle
    }
    else if (angle > 360) {
      angle <- angle - 360
    }
    return(angle)
  }
}

#' Apply the angle calculation across a vector
#'
#' @param dx Numeric vector with x-component of forward speed, in m / s
#' @param dy Numeric vector with y-component of forward speed, in m / s
#'
#' @return This function returns a vector with wind direction angles.
calcangle <- function(dx, dy){
  calcd_angle <- mapply(calcangle_pair, dx, dy)
  return(calcd_angle)
}

f <- function(x, known){
  deriv <- 70 * 9 * x ^ 8 - 315 * 8 * x ^ 7 + 540 * 7 * x ^ 6 -
    420 * 6 * x ^ 5 + 126 * 5 * x ^ 4
  f <- 70 * x ^ 9 - 315 * x ^ 8 + 540 * x ^ 7 - 420 * x ^ 6 +
    126 * x ^ 5 - known
  temp <-c(deriv, f)
  return(temp)
}

newton2 <-function(known, eps = 10e-4, itmax = 100){

  x <- 0.5
  for(it in seq(1,itmax,by=1)){
    xo <- x
    temp2 <- f(xo,known)
    dfdx <- temp2[1]
    fx <- temp2[2]
    dx <- -fx / dfdx
    x <- xo + dx
    if(abs(dx) < eps){
      x <- 0.5
      break
    }
  }

  xin <- 0.5 + 0.1
  for(it in seq(0, itmax, by = 2)){
    xo <- x
    temp2 <- f(xo, known)
    dfdx <- temp2[1]
    fx <- temp2[2]
    dx <- -fx/dfdx
    x <- xo + dx
    if(abs(dx) < eps * abs(x)){
      break
    }
  }
  sigma <- x
  return(sigma)
}
