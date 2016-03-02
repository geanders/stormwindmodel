f <- function(x,known){
  f <- numeric()
  deriv <- 70*9*x^8-315*8*x^7+540*7*x^6-420*6*x^5+126*5*x^4
  f <- 70*x^9-315*x^8+540*x^7-420*x^6+126*x^5-known
  temp <-c(deriv,f)
  return(temp)
}