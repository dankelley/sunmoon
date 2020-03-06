to180 <- function(x)
{
    x <- x %% 360
    ifelse(x <= 180, x, x - 360)
}

#' Moon rotation angle, from ch13 of Meeus book
#'
#' @param time
#' @param longitude
#' @param latitude
#' @references
#' * https://en.wikipedia.org/wiki/Position_of_the_Sun
#' * http://www.jgiesen.de/elevaz/basics/meeus.htm
moonRotationAngle <- function(t, lon, lat)
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

##OLD #' Moon rotation angle, from ch13 of Meeus book
##OLD #'
##OLD #' @param time
##OLD #' @param longitude
##OLD #' @param latitude
##OLD #' @references
##OLD #' * https://en.wikipedia.org/wiki/Position_of_the_Sun
##OLD #' * http://www.jgiesen.de/elevaz/basics/meeus.htm
##OLD moonRotationAngleOldNotUsedAnymore <- function(t=as.POSIXct(Sys.time()), longitude, latitude)
##OLD {
##OLD     ma <- moonAngle(t)#,  longitude=longitude, latitude=latitude)
##OLD     sa <- sunAngle(t)# ,  longitude=longitude, latitude=latitude)
##OLD     alpha <- sa$rightAscension
##OLD     delta <- sa$declination
##OLD     alphaPrime <- ma$rightAscension
##OLD     deltaPrime <- ma$declination
##OLD     numerator <- cos(delta) * sin(alpha - alphaPrime)
##OLD     denominator <- cos(deltaPrime)*sin(delta) - sin(deltaPrime)*cos(delta)*cos(alpha - alphaPrime)
##OLD     rpd <- pi / 180
##OLD     cat("alphaPrime=", rpd*alphaPrime, "\n")
##OLD     cat("deltaPrime=", rpd*deltaPrime, "\n")
##OLD     cat("alpha=", rpd*alpha, "\n")
##OLD     cat("delta=", rpd*delta, "\n")
##OLD     atan2(numerator, denominator) * 180 / pi
##OLD }

#' Rotate vectors counter-clockwise
#'
#' @param p numeric data frame with `x` and `y` coordinates.
#' @param angle numeric value giving the rotation angle in
#' degrees, positive for clockwise rotation.
#'
#' @return A data frame with rotated coordinates
#'
#' @examples
#' x <- 0:5
#' y <- 0:5
#' plot(x, y, asp=1, xlim=c(-7,7), ylim=c(-7,7))
#' xyr <- rotateXY(x, y, 45)
#' points(xyr$x, xyr$y, col=2, pch=20)
#' legend("topleft", pch=c(1,20), col=c(1,2), legend=c("Unrotated", "Rotated with angle=45"))
rotateXY <- function(p, angle=0)
{
    x <- p$x
    y <- p$y
    xy <- cbind(x, y)
    angle <- angle * pi / 180
    m <- cbind(c(cos(angle), -sin(angle)), c(sin(angle), cos(angle)))
    xyr <- xy %*% m
    data.frame(x=xyr[,1], y=xyr[,2])
}

#' Draw the moon showing bright and shadow regions
#'
#' @param phase numeric value, ranging continuously from
#' 0 for the (dark) new moon, 1/4 for quarter moon (right side
#' lit), 1/2 for full moon, 3/4 for three-quarter moon (left
#' side lit, and 1 for new moon again.
#'
#' @param phase numeric value, giving the rotation angle, in degrees
#' clockwise.
#'
#' @param lit colour used for the lit portion (defaults to a yellow).
#'
#' @param shadow colour used for the shadowed portion (defaults to a light gray).
#'
#' @param text character value, to draw at the moon centre (mainly for debugging).
#'
#' @param write logical value indicating whether to write the polygon for
#' the lit portion to a text file with a name constructed by pasting together
#' `phase_`, the phase to two digits, and `.dat`.
#'
#' @examples
#' drawMoon(phase=0.15, angle=15)
drawMoon <- function(phase, angle=0,
                     lit=rgb(200/255,150/255,10/255),
                     shadow=gray(0.5), # rgb(230/255,230/255,230/255),
                     text="",
                     write=FALSE)
{
    ## orthographic projection
    XY <- function(lon, lat, R=1)
    {
        lon <- to180(lon)
        lambda <- pi * lon / 180
        phi <- pi * lat / 180
        x <- R * cos(phi) * sin(lambda)
        y <- R * sin(phi)
        list(x=x, y=y)
    }
    phase <- phase - floor(phase)      # put in range 0 to 1
    shadowLongitude <- 90 - 360 * phase
    shadowLongitude <- to180(shadowLongitude)
    ## draw edge-of-moon circle
    plot(c(-1,1), c(-1,1), xlab="", ylab="", axes=FALSE, asp=1, type="n")
    theta <- seq(0, 2*pi, pi/64)
    polygon(cos(theta), sin(theta), col=shadow)
    lat <- seq(-90, 90, 1)
    lhs <- XY(rep(-90, length(lat)), rev(lat))
    rhs <- XY(rep(90, length(lat)), rev(lat))
    ## polygon p will be drawn with the 'lit' colo
    if (phase > 0.5) {
        xy <- XY(rep(-180 + shadowLongitude, length(lat)), lat)
        p <- data.frame(x=c(xy$x, lhs$x), y=c(xy$y, lhs$y))
    } else {
        xy <- XY(rep(shadowLongitude, length(lat)), lat)
        p <- data.frame(x=c(xy$x, rhs$x), y=c(xy$y, rhs$y))
    }
    polygon(rotateXY(p, angle), col=lit)
    if (write)
        write.table(round(p, 5), sprintf("phase_%.4f.dat", phase), row.names=FALSE)
    if (nchar(text) > 0)
        text(0, 0, text, cex=0.8)
    ## mtext(side=1, sprintf("%.2f %.0f", phase-floor(phase), shadowLongitude))
}

