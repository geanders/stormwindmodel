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

