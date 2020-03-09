rm(list=ls())
tz <- "America/Halifax" # for display of local times
showWork <- FALSE

angles <- function(day=Sys.Date(), lon=-63.61, lat=44.67, sun=TRUE)
{
    t <- seq(as.POSIXct(paste(day, "04:00:00"), tz="UTC"), length.out=24*60, by="1 min")
    a <- if (sun) sunAngle(t, lon=lon, lat=lat) else moonAngle(t, lon=lon, lat=lat)
    #invisible <- a$altitude < 0
    #a$altitude[invisible] <- NA
    #a$azimuth[invisible] <- NA
    list(t=t, altitude=a$altitude, azimuth=a$azimuth)
}



#setwd('~/git/sunmoon/sandbox/dek/issue04/angle.R')

#' Convert astronomical angle in degrees to hours, minutes, and seconds
#'
#' @param d angle in degrees
#' @return a list containing numerical values `hour`, `minute`, and `second`,
#' and a character value `string` that combines these together in the
#' astronomical convention; see \dQuote{Examples}.
#'
#' @examples
#' # A randomly-chosen example on page 99 of Meeus (1991),
#' # yielding 11h 50m 58.0992s.
#' hms <- degree2hms(177.74208)
#' stopifnot(hms$hour == 11)
#' stopifnot(hms$minute == 50)
#' stopifnot(hms$second == 58.0992)
#' stopifnot(hms$string == "11h50m58s.10")
#'
#' @references
#' Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA,
#' USA. 429 pages.
#'
#' @author Dan Kelley
#' @export
degree2hms <- function(d)
{
    t <- 24 * d / 360
    hour <- floor(t)
    minute <- floor(60*(t - hour))
    second <- 3600*(t - hour - minute / 60)
    floorSecond <- floor(second)
    centiSecond <- round(100 * (second - floorSecond))
    string <- sprintf("%.0fh%.0fm%.0fs.%.0f", hour, minute, floorSecond, centiSecond)
    list(hour=hour, minute=minute, second=second, string=string)
}

## Meeus (1991) Ex 46.a (p317 PDF p322)
library(oce)
source("drawMoonRotated.R")
lon <- -63.6                           # Halifax
lat <- 44.6                            # Halifax

t <- as.POSIXct("1992-04-12 00:00:00", tz="UTC")
mra <- moonRotationAngle(t, lon=-63, lat=43)
chi <- mra$chi                         # chi (Meeus 1991  ch 46 pdf p321)
q <- mra$q                             # parallactic angle (Meeus 1991 ch 13 pdf p100)
expect_equal(chi, 285.0, tol=0.1, scale=1)


## my photo has dark arc at bottom-left, roughly 45deg
t <- as.POSIXct("2020-02-05 11:46:44", tz="UTC") # 14:46:44 Halifax time
## chi is position of mid-point of bright outline of moon,
## with chi=0 meaning "up" or north and 90 meaning "right" or east.
mra <- moonRotationAngle(t, lon=-63, lat=43)
chi <- mra$chi
q <- mra$q

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
t <-  lubridate::with_tz(Sys.time(), "UTC")
## moon 1/2 full, dark on left, arc rotated left 45deg
t <- as.POSIXct("2020-03-02 14:00:00", tz="UTC")
mra<- moonRotationAngle(t, lon=lon, lat=lat)
chi <- mra$chi
q <- mra$q
#chi <- ifelse(chi > 180, chi-360, chi)
ma <- moonAngle(t, longitude=lon, latitude=lat)
#chi <- 90
angle <- chiq2angle(chi, q)
par(mfrow=c(1,1))
drawMoon(phase=ma$phase, angle=angle)
if (showWork) {
    lines(c(0, cos(angle*pi/180)), c(0, sin(angle*pi/180)))
    mtext(format(t), adj=0, line=-0.5)
    mtext(paste0("chi=", round(chi,1), ", IF=",round(100*IF), "%"), adj=1, line=-0.5)
    mtext(paste0("angle=", round(angle,1)), adj=1, line=-1.5)
    mtext(paste0("lon=", lon, ", lat=", lat), adj=0, side=1, line=-0.5)
    text(0, 0.2, round(chi), font=2, cex=1.4, col="white")
    text(0, -0.2, round(angle), font=2, cex=1.4, col="white")
}


# Observed Mar 2 at 10AM chi=-45 or so, bright on RHS, half moon
# but the code says chi=-98 on that day.
year <- 2020
for (mo in 3) {#1:12) {
    if (!interactive()) png(sprintf("mo%02d.png", mo), res=150, pointsize=9,
                            width=5, height=5, unit="in")
    par(mfrow=c(5, 7), mar=rep(0.25, 4))
    ## some websites (moongiant?) show phase at 12UTC, I think
    times <- seq(as.POSIXct(sprintf("2020-%02d-01 00:00:00", mo), tz="UTC"), by="day", length.out=31)
    weekDay <- lubridate::wday(ISOdatetime(year, mo, 1, 12, 0, 0, tz="UTC"))
    ## blank out some days to get Monday etc
    if (weekDay < 8) {
        for (d in seq_len(weekDay-1)) {
            plot(c(-1,1), c(-1,1), xlab="", ylab="", axes=FALSE, asp=1, type="n")
        }
    }
    for (itime in seq_along(times)) {
        ##message(itime)
        time <- times[itime]
        angleSun <- angles(times[itime], sun=TRUE)
        angleMoon <- angles(times[itime], sun=FALSE)
        ##
        moonBelowHorizon <- angleMoon$altitude < 0
        ##par(mar=c(3,3,1,1), mgp=c(2,0.7,0))
        ##plot(angleMoon$tlocal, angleMoon$altitude)
        ##plot(angleMoon$tlocal, c(0,diff(moonBelowHorizon)))
        iMoonRise <- 1 + which(diff(moonBelowHorizon) == -1)[1]
        if (is.finite(iMoonRise)) {
            moonriseTime <- angleMoon$t[iMoonRise]
            moonriseAltitude <- angleMoon$altitude[iMoonRise]
            ##plot(angleMoon$tlocal, angleMoon$altitude)
            ##abline(v=angleMoon$tlocal[iMoonRise])
            moonriseAzimuth <- angleMoon$azimuth[iMoonRise]
        }
        mra <- moonRotationAngle(time, lon=lon, lat=lat)
        chi <- mra$chi
        q <- mra$q
        angle <- chiq2angle(chi, q)
        ma <- moonAngle(time, longitude=lon, latitude=lat)
        drawMoon(phase=ma$phase, angle=angle)
        if (showWork)
            lines(c(0, cos(angle*pi/180)), c(0, sin(angle*pi/180)))
        mtext(format(time, "%a %b %d"), cex=0.7, line=-1.4)
        if (showWork) {
            text(0, 0.4, to180(round(chi)), font=2, cex=1, col="white")
            text(0, 0, to180(round(q)), font=2, cex=1, col="white")
            text(0, -0.4, round(angle), font=2, cex=1, col="white")
        }
        if (is.finite(iMoonRise)) {
            ##mtext(paste0(format(lubridate::with_tz(moonriseTime, tz), "Rise %I:%M%p"),
            ##             " [azi ", round(moonriseAzimuth), "]"), cex=0.7, line=-1.4, side=1)
            mtext(paste0(format(lubridate::with_tz(moonriseTime, tz), "%I:%M%p"),
                         " @ ", round(moonriseAzimuth)), cex=0.7, line=-1.4, side=1)
        }
    }
    if (!interactive()) dev.off()
}

