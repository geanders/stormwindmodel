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

f <- function(x,known){
  f <- numeric()
  deriv <- 70*9*x^8-315*8*x^7+540*7*x^6-420*6*x^5+126*5*x^4
  f <- 70*x^9-315*x^8+540*x^7-420*x^6+126*x^5-known
  temp <-c(deriv,f)
  return(temp)
}

newton2 <-function(known){
  #   x = numeric()
  #   f = numeric()
  #   dfdx = numeric()
  #   xo = numeric()
  #   eps = numeric()
  #   fx = numeric()
  #   dx = numeric()
  #   xin = numeric()
  #   sigma = numeric()
  #   known = numeric()
  #
  #   it = integer()
  #   itmax = integer()

  eps=10^(-4)
  itmax=100
  #      print *,'What is your initial guess for the solution?'
  #      read *, xin
  xin=0.5
  x=xin
  #      write(*,'(/,"Simple do loop",/)')
  for(it in seq(1,itmax,by=1)){
    xo=x
    temp2=f(xo,known)
    dfdx = temp2[1]
    fx = temp2[2]
    dx = -fx/dfdx
    x = xo + dx
    if(abs(dx) < eps){
      x = xin
      break
    }
  }
  xin = xin + 0.1
  #      print*, it,xo,dfdx,fx,dx
  #         write (*,2000)it,x,fx,dx
  #      write(*,'(/,"Do loop with a special increment on the index",/)')
  for(it in seq(0, itmax, by = 2)){
    xo=x
    temp2=f(xo,known)
    dfdx = temp2[1]
    fx = temp2[2]
    dx = -fx/dfdx
    x=xo+dx
    if(abs(dx)<eps*abs(x)){
      #      print it
      break
    }
  }
  sigma = x
  return(sigma)
}

tdiff <- function(iy2,im2,id2,it2,iy1,im1,id1,it1)
{
  #     This routine calculates the number of hours (delt) between
  #     two date/times.
  #
  #    Note: Times are in hours

  nday <- integer(length = 12)
  nday[1] = 0
  nday[2] = 31
  nday[3] = 59
  nday[4] = 90
  nday[5] = 120
  nday[6] = 151
  nday[7] = 181
  nday[8] = 212
  nday[9] = 243
  nday[10] = 273
  nday[11] = 304
  nday[12] = 334

  #     Calculate reference year

  iry = iy1-2
  if (iy2 < iry){
    iry=iy2-2
  }

  #     Calculate the number of hours from 00 Jan. 1 of the reference year
  ity1 = 0
  for(i in iry:iy1-1){
    if ((i%%4) == 0){
      ity1 = ity1 + 24*366
    }
    else{
      ity1 = ity1 + 24*365
    }
  }

  ity2 = 0
  for(i in iry:iy2-1){
    if ((i%%4) == 0){
      ity2 = ity2 + 24*366
    }
    else{
      ity2 = ity2 + 24*365
    }
  }
  ity1 = ity1 + 24*nday[im1]

  if ((iy1%%4 == 0) && (im1 > 2)){
    ity1=ity1+24
  }

  ity2 = ity2 + 24*nday[im2]

  if ((iy2%%4==0) && (im2>2)){
    ity2=ity2+24
  }
  ity1 = ity1 + 24*id1 + it1
  ity2 = ity2 + 24*id2 + it2

  idelt = ity2 - ity1
  return(idelt)
}


