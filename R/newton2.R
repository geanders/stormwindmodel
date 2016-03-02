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