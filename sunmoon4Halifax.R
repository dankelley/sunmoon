library(lubridate)
library(oce)
source("drawMoon.R")

debug <- FALSE                         # set to TRUE while developing
msg <- function(...) if (debug) cat(file=stderr(), ...)
colMoon <- rgb(245/255,199/255,16/255)
colSun <- "red"

locationsText <- "
name;lon;lat;tz
Halifax, Canada;-63.61;44.67;America/Halifax
Chennai, India;80.26;13.08;Asia/Kolkata
Resolute, Canada;-94.83;74.70;America/Resolute
San Francisco;-122.3973;37.8030;America/Los_Angeles
Aranuka Atoll;-173.6295;0.1905;Pacific/Tarawa
Chuuk, FSM;-151.8470;7.4467;Pacific/Chuuk
Tok, Alaska;-142.9856;63.3367;America/Anchorage"
locations <- read.delim(text=locationsText, sep=";", header=TRUE, stringsAsFactors=FALSE)
locations <- locations[order(locations$name),]
halifax <- grep("Halifax, Canada", locations$name)

angles <- function(day=Sys.Date(), tz="UTC", lon=-63.61, lat=44.67, sun=TRUE)
{
    tlocal <- seq(as.POSIXct(paste(day, "00:00:00"), tz=tz), length.out=24*60, by="1 min")
    tUTC <- with_tz(tlocal, "UTC")
    a <- if (sun) sunAngle(tUTC, lon=lon, lat=lat) else moonAngle(tUTC, lon=lon, lat=lat)
    invisible <- a$altitude < 0
    a$altitude[invisible] <- NA
    a$azimuth[invisible] <- NA
    if (sun) {
        a$illuminatedFraction <- rep(NA, length(tlocal))
    } else {
        a$illuminatedFraction[invisible] <- NA
    }
    list(tlocal=tlocal, tUTC=tUTC, altitude=a$altitude, azimuth=a$azimuth, illuminatedFraction=a$illuminatedFraction)
}

day <- Sys.Date()
m <- angles(day=day, tz=locations$tz[halifax], lon=locations$lon[halifax], lat=locations$lat[halifax], sun=FALSE)
s <- angles(day=day, tz=locations$tz[halifax], lon=locations$lon[halifax], lat=locations$lat[halifax], sun=TRUE)

if (!interactive())
    png("sunmoon4halifax.png", height=4, width=4, unit="in",
        res=120, pointsize=8)
par(mar=c(0.5, 0.5, 1, 0.5))
theta <- seq(0, 2*pi, length.out=24 * 10)
radiusx <- cos(theta)
radiusy <- sin(theta)

# Horizon and labels+lines for EW and NS
plot(radiusx, radiusy, type='l', col='gray', asp=1, axes=FALSE, xlab="", ylab="")
lines(c(-1, 1), c(0, 0), col='gray')
lines(c(0, 0), c(-1, 1), col='gray')
D <- 1.06
text( 0, -D, "S", xpd=TRUE) # xpd so can go in margin
text(-D,  0, "W", xpd=TRUE)
text( 0,  D, "N", xpd=TRUE)
text( D,  0, "E", xpd=TRUE)

## Moon trace
mx <- (90 - m$altitude) / 90 * cos(pi / 180 * (90 - m$azimuth))
my <- (90 - m$altitude) / 90 * sin(pi / 180 * (90 - m$azimuth))
lines(mx, my, col=colMoon, lwd=4)
## Moon labels
mlt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz=locations$tz[halifax])
ti <- unlist(lapply(mlt, function(X) which.min(abs(X-m$tlocal))))
ok <- abs(m$tlocal[ti] - mlt) < 10 # avoid overdrawing endpoints
points(mx[ti][ok], my[ti][ok], pch=20, cex=3, col='white')
text(mx[ti][ok], my[ti][ok], 1:23, cex=3/4)

## Sun trace
sx <- (90 - s$altitude) / 90 *  cos(pi / 180 * (90 - s$azimuth))
sy <- (90 - s$altitude) / 90 *  sin(pi / 180 * (90 - s$azimuth))
lines(sx, sy, col=colSun, lwd=4)
## Sun labels
slt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz=locations$tz[halifax])
si <- unlist(lapply(slt, function(X) which.min(abs(X-s$tlocal))))
ok <- abs(s$tlocal[ti] - slt) < 10 # avoid overdrawing endpoints
points(sx[ti][ok], sy[ti][ok], pch=20, cex=3, col='white')
text(sx[ti][ok], sy[ti][ok], 1:23, cex=3/4)

## Location/time legend in top-left corner
mtext(paste(locations$name[halifax],
            paste(abs(locations$lon[halifax]),
                  if (locations$lon[halifax] < 0) "W " else "E ",
                  abs(locations$lat[halifax]),
                  if (locations$lat[halifax] < 0) "S" else "N",
                  sep=""),
            format(day, "%b %d, %Y"),
            sep="\n"),
      side=3, adj=0, line=-3)

## Trace-colour legend in top-right corner
if (any(is.finite(s$altitude))) {
    mtext("Sun Trajectory", side=3, adj=1, line=-1, col=colSun, font=2)
} else  {
    mtext("Sun below horizon", side=3, adj=1, line=-1, col=colSun)
}
if (any(is.finite(m$altitude))) {
    mtext("Moon Trajectory", side=3, adj=1, line=-2, col=colMoon, font=2)
} else {
    mtext("Moon below horizon", side=3, adj=1, line=-2, col=colMoon)
}

## Check for eclipse, equinox, and solstice (but only if sun and moon are visible)
if (any(is.finite(m$azimuth)) || any(is.finite(s$azimuth))) {
    ## Eclipse based on sun-moon distance
    mismatch <- sqrt((m$azimuth - s$azimuth)^2 + (m$altitude - s$altitude)^2)
    if (any(is.finite(mismatch))) {
        mismatch[s$altitude < 0] <- NA # concentrate on visible sky
        nearestApproachIndex <- which.min(mismatch)
        if (length(nearestApproachIndex)) {
            if (mismatch[nearestApproachIndex] <= 0.54) { # sun diameter 0.54deg
                mtext(sprintf("ECLIPSE at %s", format(s$tlocal[nearestApproachIndex], "%H:%M")),
                      side=3, adj=1, line=-4)
            }
        }
    }
    ## Equinox
    sunriseAzimuth <- s$azimuth[head(which(is.finite(s$azimuth)), 1)]
    sunsetAzimuth <- s$azimuth[tail(which(is.finite(s$azimuth)), 1)]
    dev <- 0.5 * (abs(sunriseAzimuth-90) + abs(sunsetAzimuth-270))
    if (dev < 0.3)
        mtext("Equinox", side=3, adj=1, line=-4, font=2)
    if (debug) {
        mtext(sprintf("Sunrise azimuth %.1fdeg", sunriseAzimuth), side=3, adj=1, line=-5)
        mtext(sprintf("Sunset azimuth %.1fdeg", sunsetAzimuth), side=3, adj=1, line=-6)
        mtext(sprintf("dev %.2fdeg", dev), side=3, adj=1, line=-7)
    }
    ## Solstice
    earthObliquity <- 23.43669     # https://en.wikipedia.org/wiki/Axial_tilt as of 2019-12-29
    noonSolarAltitude <- max(s$altitude, na.rm=TRUE)
    msg(vectorShow(noonSolarAltitude))
    winterSolsticeSolarAltitude <- 90 - (locations$la[halifax] + earthObliquity)
    msg(vectorShow(winterSolsticeSolarAltitude))
    summerSolsticeSolarAltitude <- 90 - (locations$lat[halifax] - earthObliquity)
    msg(vectorShow(summerSolsticeSolarAltitude))
    if (abs(noonSolarAltitude - winterSolsticeSolarAltitude) < 0.00155)
        mtext("Winter Solstice", side=3, adj=1, line=-4, font=2)
    if (abs(noonSolarAltitude - summerSolsticeSolarAltitude) < 0.002)
        mtext("Spring Solstice", side=3, adj=1, line=-4, font=2)
}

## Draw moon-phase diagram, with margin text for % illuminated
par(new=TRUE, mar=c(0,26,26,1))
noon <- as.POSIXct(paste(format(day, "%Y-%m-%d"), "12:00:00"), tz="UTC")
ma <- moonAngle(noon)
drawMoon(ma$phase)
mtext(paste0("Moon\n", round(100*ma$illuminatedFraction),
             "% full\nat 12:00 UTC"), cex=1, col=colMoon, font=2)

if (!interactive())
    dev.off()

