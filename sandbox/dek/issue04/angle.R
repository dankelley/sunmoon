rm(list=ls())

## Meeus (1991) Ex 46.a (p317 PDF p322)
library(oce)
source("drawMoonRotated.R")
lon <- -63.6                           # Halifax
lat <- 44.6                            # Halifax

moonRotationAngleNEW <- function(t, lon, lat)
{
    to360 <- function(x)
    {
        while(x < 0)
            x <- x + 360
        x
    }
    testCase <- t == as.POSIXct("1992-04-12 00:00:00", tz="UTC")
    k <- pi / 180
    sa <- sunAngle(t, lon=lon, lat=lat)
    da <- sunDeclinationRightAscension(t, apparent=TRUE)
    delta0 <- da$declination
    alpha0 <- da$rightAscension
    if (testCase) {
        ## why do these not match up to all 4 digits after the decimal?
        expect_equal(alpha0, 20.6579, tol=0.0004, scale=1)
        expect_equal(delta0,  8.6964, tol=0.0003, scale=1)
    }
    ma <- moonAngle(t, lon=lon, lat=lat)
    alpha <- ma$rightAscension
    delta <- ma$declination
    if (testCase) {
        ## why do these not match so poorly?
        expect_equal(alpha, 134.6885, tol=0.0200, scale=1)
        expect_equal(delta,  13.7684, tol=0.0050, scale=1)
    }
    chi <- 1/k*atan2(cos(k*delta0) * sin(k*(alpha0-alpha)),
                     sin(k*delta0)*cos(k*delta) - cos(k*delta0)*sin(k*delta)*cos(k*(alpha0-alpha)))
    chi <- to360(chi)
    if (testCase) {
        expect_equal(chi, 285.0, tol=0.1, scale=1)
    }
    chi
}
t <- as.POSIXct("1992-04-12 00:00:00", tz="UTC")
chi <- moonRotationAngleNEW(t, lon=-63, lat=43)
expect_equal(chi, 285.0, tol=0.1, scale=1)


## my photo has dark arc at bottom-left, roughly 45deg
t <- as.POSIXct("2020-02-05 11:46:44", tz="UTC") # 14:46:44 Halifax time
## chi is position of mid-point of bright outline of moon,
## with chi=0 meaning "up" or north and 90 meaning "right" or east.
chi <- moonRotationAngleNEW(t, lon=-63, lat=43)

## Test case: 2020-02-05 is nearly full moon, with dark arc on LHS
## angle=0 corresponds to chi=90. In our notation, increasing
## angle moves the bright arc clockwise.  From these,
## we infer that angle=chi-90.
## (See https://www.moongiant.com/phase/2/05/2020 to verify nearly-full
## with dark arc on left.)
par(mar=rep(0.5, 4))
drawMoon(moonAngle(t,lon=lon,lat=lat)$phase, angle=chi-90)
mtext(paste0(format(t), ", chi=", round(chi,1)), line=-1)

## https://www.youtube.com/watch?v=zdTpIGHIBSU
## angle to horizon 84.62 deg (illum limb facing horizon)
t <- as.POSIXct("2013-02-18 03:37:00", tz="UTC")
lon <- -83
lat <- 27
ma <- moonAngle(t, lon=lon, lat=lat)
chi <- moonRotationAngleNEW(t, lon=lon, lat=lat)
180-chi
ma
