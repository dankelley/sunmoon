library(lubridate)
library(oce)

locationsText <- "
name;lon;lat;tz
Halifax, Canada;-63.61;44.67;America/Halifax
Chennai, India;80.26;13.08;Asia/Kolkata
Resolute, Canada;-94.83;74.70;America/Resolute
"
locations <- read.delim(text=locationsText, sep=";", header=TRUE, stringsAsFactors=FALSE)
w <- 1 # focus on Halifax

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
m <- angles(day=day, tz=locations$tz[w], lon=locations$lon[w], lat=locations$lat[w], sun=FALSE)
s <- angles(day=day, tz=locations$tz[w], lon=locations$lon[w], lat=locations$lat[w], sun=TRUE)

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
lines(mx, my, col='blue', lwd=3)
## Moon labels
mlt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz=locations$tz[w])
ti <- unlist(lapply(mlt, function(X) which.min(abs(X-m$tlocal))))
ok <- abs(m$tlocal[ti] - mlt) < 10 # avoid overdrawing endpoints
points(mx[ti][ok], my[ti][ok], pch=20, cex=3, col='white')
text(mx[ti][ok], my[ti][ok], 1:23, cex=3/4)

## Sun trace
sx <- (90 - s$altitude) / 90 *  cos(pi / 180 * (90 - s$azimuth))
sy <- (90 - s$altitude) / 90 *  sin(pi / 180 * (90 - s$azimuth))
lines(sx, sy, col='red', lwd=3)
## Sun labels
slt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz=locations$tz[w])
si <- unlist(lapply(slt, function(X) which.min(abs(X-s$tlocal))))
ok <- abs(s$tlocal[ti] - slt) < 10 # avoid overdrawing endpoints
points(sx[ti][ok], sy[ti][ok], pch=20, cex=3, col='white')
text(sx[ti][ok], sy[ti][ok], 1:23, cex=3/4)

mtext(paste(locations$name[w],
            paste(abs(locations$lon[w]),
                  if (locations$lon[w] < 0) "W " else "E ",
                  abs(locations$lat[w]),
                  if (locations$lat[w] < 0) "S" else "N",
                  sep=""),
            format(day, "%b %d, %Y"),
            sep="\n"),
      side=3, adj=0, line=-3)

mtext("Sun", side=3, adj=1, line=-1, col="red")
illuminatedFraction <- round(100*mean(m$illuminatedFraction, na.rm=TRUE))
if (!is.null(illuminatedFraction)) {
    mtext(sprintf("Moon (%.0f%% full)", illuminatedFraction),
          side=3, adj=1, line=-2, col="blue")
} else {
    mtext("Moon", side=3, adj=1, line=-2, col="blue")
}
## sun-moon distance for eclipse diagnosis
mismatch <- sqrt((m$azimuth - s$azimuth)^2 + (m$altitude - s$altitude)^2)
iNearestApproach <- which.min(mismatch)
## sun diameter 0.54deg
if (mismatch[iNearestApproach] <= 0.54)
    ## mtext(sprintf("ECLIPSE (%.1f deg at %s)", mismatch[iNearestApproach],
    mtext(sprintf("ECLIPSE at %s", format(s$tlocal[iNearestApproach], "%H:%M")),
          side=3, adj=1, line=-4)

if (!interactive())
    dev.off()

