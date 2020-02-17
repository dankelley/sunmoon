rm(list=ls())
#setwd('~/git/sunmoon/sandbox/dek/issue04/angle.R')


## Meeus (1991) Ex 46.a (p317 PDF p322)
library(oce)
source("drawMoonRotated.R")
lon <- -63.6                           # Halifax
lat <- 44.6                            # Halifax

t <- as.POSIXct("1992-04-12 00:00:00", tz="UTC")
chi <- moonRotationAngle(t, lon=-63, lat=43)
expect_equal(chi, 285.0, tol=0.1, scale=1)


## my photo has dark arc at bottom-left, roughly 45deg
t <- as.POSIXct("2020-02-05 11:46:44", tz="UTC") # 14:46:44 Halifax time
## chi is position of mid-point of bright outline of moon,
## with chi=0 meaning "up" or north and 90 meaning "right" or east.
chi <- moonRotationAngle(t, lon=-63, lat=43)

## Test case: 2020-02-05 is nearly full moon, with dark arc on LHS
## angle=0 corresponds to chi=90. In our notation, increasing
## angle moves the bright arc clockwise.  From these,
## we infer that angle=chi-90.
## (See https://www.moongiant.com/phase/2/05/2020 to verify nearly-full
## with dark arc on left.)
par(mar=rep(0.5, 4))
##? drawMoon(moonAngle(t,lon=lon,lat=lat)$phase, angle=chi-90)
##? mtext(paste0(format(t), ", chi=", round(chi,1)), line=-1)

##? ## https://www.youtube.com/watch?v=zdTpIGHIBSU
##? ## angle to horizon 84.62 deg (illum limb facing horizon)
##? t <- as.POSIXct("2013-02-18 03:37:00", tz="UTC")
##? ma <- moonAngle(t, lon=-83, lat=27)
##? chi <- moonRotationAngle(t, lon=-83, lat=27)

#lon <- 0
#lat <- -40
t<-  lubridate::with_tz(Sys.time(), "UTC")
chi <- moonRotationAngle(t, lon=lon, lat=lat)
ma <- moonAngle(t, longitude=lon, latitude=lat)
drawMoon(phase=ma$phase, angle=chi-90)
IF <- ma$illuminatedFraction
mtext(format(t), adj=0, line=-0.5)
mtext(paste0("chi=", round(chi,1), ", IF=",round(100*IF), "%"), adj=1, line=-0.5)
mtext(paste0("lon=", lon, ", lat=", lat), adj=0, side=1, line=-0.5)
