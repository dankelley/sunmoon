## Draw a mooncalendar
library(lubridate)
library(oce)
source("drawMoon.R")

#' Draw a moon-phase calendar.  The drawings apply at noon (UTC) on the given day.
#'
#' @param year integer value giving the year
#' @param month integer value giving the month
#' @param longitude numeric value giving the longitude, in degrees east
#' @param latitude numeric value giving the latitude, in degrees north
#' @param lit colour for sunlit portion of moon, passed to drawMoon()
#' @param shadow colour for dark portion of moon, passed to drawMoon()
calendar <- function(year, month,
                     longitude=-63.61, latitude=44.67,
                     lit=rgb(200/255,150/255,10/255), shadow=gray(0.5))
{
    par(mfrow=c(6,7), mar=rep(0.2, 4))
    if (missing(year))
        year <- as.integer(format(Sys.Date(), "%Y"))
    if (missing(month))
        month <- as.integer(format(Sys.Date(), "%m"))
    weekDay <- wday(ISOdatetime(year, month, 1, 12, 0, 0, tz="UTC"))
    if (weekDay < 8) {
        for (d in seq(1, weekDay-1)) {
            plot(c(-1,1), c(-1,1), xlab="", ylab="", axes=FALSE, asp=1, type="n")
        }
    }
    for (d in seq(1, 31)) {
        t <- ISOdatetime(year, month, d, 12, 0, 0, tz="UTC")
        if (is.finite(month(t))) {
            plot(c(-1,1), c(-1,1), xlab="", ylab="", axes=FALSE, asp=1, type="n")
            box()
            par(new=TRUE)
            ma <- moonAngle(t, longitude=longitude, latitude=latitude)
            drawMoon(ma$phase, lit=lit, shadow=shadow)
            text(0,0.6,format(t, "%a\n%b %d\n%Y"), pos=1, font=2)
        }
    }
}
if (!interactive())
    png("/Users/kelley/Sites/moon_calendar/calendar.png",
        height=4.8, width=5.6, unit="in",
        res=120, pointsize=13)
calendar(lit="#FFDC00", shadow="#AAAAAA")
if (!interactive())
    dev.off()

