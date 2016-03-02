calcangle <- function(dx,dy){
  
#   dx <- numeric()
#   dy <- numeric()
    angle <- numeric()
#   rad2deg <- numeric()
  
  rad2deg = 180.0/3.14159
  
  if (dx>0.0){
    angle = atan(dy/dx)*rad2deg
  }
  else if(dx<0.0){
    angle = 180.0 + atan(dy/dx)*rad2deg
  }
  else{
    if (dy==0.0){
      angle = 0.0
    }
    else{
      angle = atan2(dy,dx)*rad2deg
    }
  }
  if(angle<0.0)
  {
    angle = 360.0 + angle
  }
  else if(angle > 360.0){
    angle = angle - 360.0
  }
  return(angle)
}
