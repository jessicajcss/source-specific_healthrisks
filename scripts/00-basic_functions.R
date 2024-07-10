#############################################################################
#############################################################################

# Basic functions ----


###################
# Last update: 2024-07-09
# Author: Santos-Silva, J. C. <jessica.jcss@gmail.com>

#############################################################################
#############################################################################



## WIND COMPONENTS ----

### Create columns to and calculate the u and v wind components to all data ###

u.wind <- function(ws, wd) {
  u <- - ws * sin(2 * pi * wd/360)
  return(u)
}

v.wind <- function(ws, wd) {
  v <- - ws * cos(2 * pi * wd/360)
  return(v)
}



### Calculate wind direction for this hour-long period ###
wind_direction <- function(u, v) {
  x <- (atan2(u, v) * 360/2/pi) + 180
  return(x) 
}

### Calculate vectorial wind speed for this hour-long period ###
wind_speed <- function(u, v) {
  z <- ((u^2 + v^2)^0.5)
  return(z)
}
